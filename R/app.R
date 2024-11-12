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

  output$weather <- shiny::renderPlot(
    plot_weather_forecast(locations()[[1L]]),
    res = 100
  )
}

ui <- function() {
  shiny::basicPage(
    shiny::textInput("locations", "locations", "tokyo"),
    shiny::plotOutput("weather")
  )
}

run <- function() {
  shiny::shinyApp(ui(), server)
}
