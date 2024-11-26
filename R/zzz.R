.onLoad <- function(libname, pkgname) {
  # log level
  logger::log_threshold(Sys.getenv("JAPANR2024_LOG_LEVEL", "OFF"), pkgname)

  # log format
  logger::log_formatter(logger::formatter_json, pkgname)
  logger::log_layout(
    logger::layout_json_parser(fields = c("time", "level", "ns")),
    pkgname
  )

  # log to stdout and/or file
  log_appender <- stringr::str_split(
    Sys.getenv("JAPANR2024_LOG_APPENDER"),
    ";"
  )[[1]]
  if (!identical(log_appender, "")) {
    stdout <- log_appender == "-"
    file <- log_appender[!stdout]
    if (any(stdout)) {
      if (length(file) > 0) {
        logger::log_appender(logger::appender_tee(file), pkgname)
      }
    } else if (length(file) > 0) {
      logger::log_appender(logger::appender_file(file), pkgname)
    }
  }
}
