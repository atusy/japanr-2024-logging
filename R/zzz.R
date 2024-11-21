.onLoad <- function(libname, pkgname) {
  logger::log_threshold(`::`("logger", Sys.getenv("JAPANR2024_LOG_LEVEL", "INFO")))
  log_appender <- stringr::str_split(Sys.getenv("JAPANR2024_LOG_APPENDER"), ";")[[1]]
  if (!identical(log_appender, "")) {
    stdout <- "-" %in% log_appender
    file <- log_appender[!stdout]
    if (any(stdout)) {
      if (length(file) > 0) {
        logger::log_appender(logger::appender_tee(file))
      }
    } else if (length(file) > 0) {
      logger::log_appender(logger::appender_file(file))
    }
  }
  logger::log_appender(logger::appender_stdout)
  logger::log_formatter(logger::formatter_json)
  logger::log_layout(
    logger::layout_json_parser(fields = c("time", "level", "ns"))
  )
}
