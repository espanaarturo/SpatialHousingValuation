#' Rolling median/MAD baseline detector
#'
#' Uses a simple univariate stability score aggregated across numeric columns.
baseline_fit_initial <- function(data, config) {
  numeric_cols <- names(Filter(is.numeric, data))
  state <- new_detector_state(
    name = "baseline_mad",
    meta = list(
      numeric_cols = numeric_cols,
      threshold_z = config$threshold_z %||% 3.5,
      min_points = config$min_points %||% 20,
      adaptive = config$adaptive %||% TRUE,
      threshold_quantile = config$threshold_quantile %||% 0.995,
      calib_scores = numeric()
    )
  )
  state
}

baseline_partial_fit <- function(data, state, config) {
  # No adaptive fit for baseline; return state unchanged
  state
}

baseline_score <- function(data, state) {
  cols <- state$meta$numeric_cols
  if (length(cols) == 0) stop("No numeric columns to score.")
  scores <- vapply(cols, function(col) {
    x <- data[[col]]
    med <- stats::median(x, na.rm = TRUE)
    mad_val <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
    abs(x - med) / (mad_val + 1e-6)
  }, numeric(nrow(data)))
  rowMeans(scores, na.rm = TRUE)
}

baseline_detect <- function(data, state, config) {
  scores <- baseline_score(data, state)
  validate_detector_output(scores, nrow(data))
  if (isTRUE(state$meta$adaptive)) {
    state$meta$calib_scores <- c(state$meta$calib_scores, scores)
    thr <- stats::quantile(state$meta$calib_scores, probs = state$meta$threshold_quantile, na.rm = TRUE, names = FALSE)
    state$meta$threshold_z <- thr
  } else {
    thr <- config$threshold_z %||% state$meta$threshold_z
  }
  is_anom <- scores > thr
  list(
    results = tibble::tibble(
      timestamp = data$timestamp,
      id = data$id,
      is_anomaly_true = data$is_anomaly_true %||% NA,
      score = scores,
      is_anomaly = is_anom
    ),
    state = state
  )
}


