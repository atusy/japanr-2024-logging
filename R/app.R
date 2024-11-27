.log <- function(level, message, ctx = list()) {
  # Expected fields:
  #   - time
  #   - level
  #   - ns
  #   - message
  #   - session_id: unique per access
  #   - trace_id: unique per request
  #   - span_id: unique per task
  #   - value: miscellaneous values including errors
  purrr::exec(logger::log_level, level, message = message, !!!ctx)
}

plot_weather_forecast <- function(x, log = .log, ctx = list()) {
  log(logger::DEBUG, "Plotting weather forecast", ctx)
  .data <- rlang::.data

  geocode <- openmeteo::geocode(x)
  log(
    logger::DEBUG,
    "Resolved geocode",
    c(ctx, list(value = as.list(geocode)))
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

server <- function(log = .log) {
  force(log)

  # Server function
  function(input, output, session) {
    # Log session start
    session_id <- ulid::ulid()
    log(logger::INFO, "Started session", list(session_id = session_id))

    # Configure logger
    trace_id <- shiny::reactiveVal(NA_character_) # NA behaves as null in JSON
    ctx <- shiny::reactive({
      list(session_id = session_id, trace_id = trace_id())
    })

    # Record request ID and optionally log request parameters
    shiny::observe({
      params <- shiny::reactiveValuesToList(input)
      trace_id(ulid::ulid())
      log(logger::INFO, "Received request", ctx())
      log(
        logger::DEBUG,
        "Received request parameters",
        c(list(value = params), ctx())
      )
    })

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

    # Prepare desired number of weather forecast plots
    output$weather <- shiny::renderUI({
      locations() |>
        lapply(function(x) {
          shiny::plotOutput(outputId = paste0("weather_", x))
        })
    })

    # Draw weather forecast plots
    shiny::observe({
      promises <- lapply(locations(), function(location) {
        # task-specific logging context
        ctx_span <- c(list(span_id = ulid::ulid()), ctx())

        log(
          logger::DEBUG,
          "Processing location",
          c(list(value = location), ctx_span)
        )

        plot_id <- paste0("weather_", location)
        future::future({
          plot_weather_forecast(location, log, ctx_span)
        }) |>
          promises::then(function(x) {
            output[[plot_id]] <- shiny::renderPlot(x, res = 100)
            log(logger::DEBUG, "Processed location", ctx_span)
          }) |>
          promises::catch(function(e) {
            log(
              logger::ERROR,
              "Failed to process location",
              c(list(value = as.list(format(e))), ctx_span)
            )
          })

        # Do not log "Processed location" here as it is async
      })

      purrr::exec(promises::promise_all, !!!promises) |>
        promises::then(function(...) {
          log(logger::INFO, "Processed request", ctx())
        })
    })
  }
}

ui <- function() {
  shiny::basicPage(
    shiny::textInput("locations", "locations", c("tokyo, london")),
    shiny::uiOutput("weather")
  )
}

#' @param plan A function to set the future plan
#' @param log A function to log messages
#' @param options Options to pass to `shiny::shinyApp`
run <- function(plan = NULL, log = .log, options = list()) {
  oldplan <- future::plan()
  on.exit(future::plan(oldplan))
  if (!is.null(plan)) {
    plan()
  } else if (.Platform$OS.type == "windows") {
    workers <- min(5L, future::availableCores())
    future::plan(future::multisession, workers = workers)
  } else {
    workers <- min(5L, future::availableCores(constraints = "multicore"))
    future::plan(future::multicore, workers = workers)
  }

  shiny::shinyApp(
    ui(),
    server(log = log),
    options = options
  )
}
