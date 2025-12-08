#' Detector interface contracts
#'
#' Each detector should implement:
#' - fit_initial(data, config) -> state
#' - partial_fit(data, state, config) -> updated state
#' - score(data, state) -> numeric vector of scores aligned with data rows
#' - detect(data, state, config) -> list(results = tibble, state = state)
new_detector_state <- function(name, fitted_at = Sys.time(), meta = list()) {
  list(
    name = name,
    fitted_at = fitted_at,
    meta = meta
  )
}

validate_detector_output <- function(scores, n) {
  if (length(scores) != n) stop("Detector scores must align with data rows")
  invisible(TRUE)
}


