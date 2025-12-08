#' Sliding and tumbling window utilities
window_stream <- function(data, size, step, type = c("sliding", "tumbling")) {
  type <- match.arg(type)
  n <- nrow(data)
  windows <- list()
  if (type == "sliding") {
    starts <- seq(1, max(1, n - size + 1), by = step)
    for (s in starts) {
      e <- min(n, s + size - 1)
      windows[[length(windows) + 1]] <- data[s:e, , drop = FALSE]
    }
  } else {
    starts <- seq(1, n, by = step)
    for (s in starts) {
      e <- min(n, s + size - 1)
      windows[[length(windows) + 1]] <- data[s:e, , drop = FALSE]
    }
  }
  windows
}


