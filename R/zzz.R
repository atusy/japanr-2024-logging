.onLoad <- function(libname, pkgname) {
  logger::log_appender(logger::appender_tee("__ignored/japanr2024.log"))
  logger::log_formatter(logger::formatter_json)
  logger::log_layout(
    logger::layout_json_parser(fields = c("time", "level", "ns"))
  )
  logger::log_threshold(logger::DEBUG)
}
