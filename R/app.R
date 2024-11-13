plot_weather_forecast <- function(x) {
  .data <- rlang::.data

  location <- openmeteo::geocode(x)

  forecast <- openmeteo::weather_forecast(
    c(location$latitude, location$longitude),
    hourly = "temperature_2m"
  )

  ggplot2::ggplot(forecast) +
    ggplot2::aes(.data$datetime, .data$hourly_temperature_2m) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = location$name,
      subtitle = paste(
        "Latitude:",
        location$latitude,
        "Longitude:",
        location$longitude
      )
    )
}

server <- function(input, output, session) {
  locations <- shiny::reactive({
    shiny::req(input$locations)
    input$locations |>
      stringi::stri_split_regex("\\s*,\\s*") |>
      unlist()
  })

  output$weather <- shiny::renderUI({
    locations() |>
      lapply(function(x) {
        shiny::plotOutput(outputId = paste0("weather_", x))
      })
  })

  shiny::observe({
    lapply(locations(), function(location) {
      plot_id <- paste0("weather_", location)
      output[[plot_id]] <- shiny::renderPlot(
        {
          plot_weather_forecast(location)
        },
        res = 100
      )
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
  shiny::shinyApp(ui(), server)
}
