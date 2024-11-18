logger::log_formatter(logger::formatter_json)
logger::log_layout(
  logger::layout_json_parser(fields = c("time", "level", "ns"))
)

log <- function(level, message, ctx = list()) {
  purrr::exec(logger::log_level, level, message = message, !!!ctx)
}

plot_weather_forecast <- function(x, ctx = list()) {
  log(logger::DEBUG, "Plotting weather forecast", ctx)
  .data <- rlang::.data

  geocode <- openmeteo::geocode(x)
  log(logger::DEBUG,
    "Resolved geocode",
    c(ctx, list(geocode = geocode))
  )

  forecast <- openmeteo::weather_forecast(
    c(geocode$latitude, geocode$longitude),
    hourly = "temperature_2m"
  )
  log(logger::DEBUG, "Forecasted weather", ctx)

  p <- ggplot2::ggplot(forecast) +
    ggplot2::aes(.data$datetime, .data$hourly_temperature_2m) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = geocode$name,
      subtitle = paste(
        "Latitude:",
        geocode$latitude,
        "Longitude:",
        geocode$longitude
      )
    )
  log(logger::DEBUG, "Forecasted weather", ctx)

  return(p)
}

server <- function(input, output, session) {
  logger::log_threshold(logger::DEBUG)

  # Log session start
  session_id <- RcppUUID::uuid_generate_random()
  logger::log_info(message = "Session started", session_id = session_id)

  # Configure logger
  trace_id <- shiny::reactiveVal(NA_character_) # NA behaves as null in JSON
  ctx <- shiny::reactive({
    list(
      session_id = session_id,
      trace_id = trace_id()
    )
  })

  # Record request ID and optionally log request parameters
  shiny::observe({
    params <- shiny::reactiveValuesToList(input)
    trace_id(RcppUUID::uuid_generate_random())
    log(logger::DEBUG,
      "Request received",
      c(list(request_params = params), ctx())
    )
  })

  # Async strategy
  oldplan <- future::plan()
  on.exit(future::plan(oldplan))
  if (.Platform$OS.type == "windows") {
    future::plan(
      future::multisession,
      workers = min(5L, future::availableCores())
    )
  } else {
    future::plan(
      future::multicore,
      workers = min(5L, future::availableCores(constraints = "multicore"))
    )
  }

  # Parse locations as comma-separated values
  locations <- shiny::reactive({
    shiny::req(input$locations)
    ret <- input$locations |>
      stringi::stri_split_regex("\\s*,\\s*") |>
      unlist()
    log(
      logger::DEBUG,
      "Locations parsed",
      c(list(value = ret), ctx())
    )
    ret
  })

  output$weather <- shiny::renderUI({
    locations() |>
      lapply(function(x) {
        shiny::plotOutput(outputId = paste0("weather_", x))
      })
  })

  shiny::observe({
    lapply(locations(), function(location) {
      ctx_span <- c(
        list(location = location, span_id = RcppUUID::uuid_generate_random()),
        ctx()
      )
      log(logger::DEBUG, "Processing location", ctx_span)
      plot_id <- paste0("weather_", location)
      future::future({
        plot_weather_forecast(location, ctx_span)
      }) |>
        promises::then(function(x) {
          output[[plot_id]] <- shiny::renderPlot(x, res = 100)
          log(logger::DEBUG, "Processed location", ctx_span)
        }) |>
        promises::catch(function(e) {
          log(
            logger::ERROR,
            "Failed to process location",
            c(list(error = as.list(format(e))), ctx_span)
          )
        })
    })
  })
}

ui <- function() {
  shiny::basicPage(
    shiny::textInput("locations", "locations", c("tokyo, london")),
    shiny::uiOutput("weather")
  )
}

run <- function() {
  shiny::shinyApp(ui(), server, option = list(port = 5604))
}
