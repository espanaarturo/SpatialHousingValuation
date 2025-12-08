#' Lightweight logger
#'
#' @param level log level string
#' @param msg message to emit
#' @param context optional named list with context fields
log_event <- function(level = "info", msg, context = list(), quiet = FALSE) {
  if (isTRUE(quiet)) return(invisible(NULL))
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ctx <- if (length(context)) {
    paste(sprintf("%s=%s", names(context), unlist(context, use.names = FALSE)), collapse = " ")
  } else {
    ""
  }
  line <- trimws(paste0("[", level, "] ", timestamp, " - ", msg, " ", ctx))
  cat(line, "\n")
}


