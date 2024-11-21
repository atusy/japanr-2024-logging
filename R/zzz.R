.onLoad <- function(libname, pkgname) {
  ns_logger <- asNamespace("logger")
  ns <- get("top_env_name", ns_logger)

  # log level
  logger::log_threshold(
    get(
      Sys.getenv("JAPANR2024_LOG_LEVEL", "INFO"),
      envir = ns_logger
    ),
    ns
  )

  # log to stdout and/or file
  log_appender <- stringr::str_split(
    Sys.getenv("JAPANR2024_LOG_APPENDER"),
    ";"
  )[[1]]
  if (!identical(log_appender, "")) {
    stdout <- "-" %in% log_appender
    file <- log_appender[!stdout]
    if (any(stdout)) {
      if (length(file) > 0) {
        logger::log_appender(logger::appender_tee(file), ns)
      }
    } else if (length(file) > 0) {
      logger::log_appender(logger::appender_file(file), ns)
    }
  }

  # log format
  logger::log_formatter(logger::formatter_json, ns)
  logger::log_layout(
    logger::layout_json_parser(fields = c("time", "level", "ns")),
    ns
  )
}
