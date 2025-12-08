#' Isolation Forest detector using isotree
iso_fit_initial <- function(data, config) {
  if (!requireNamespace("isotree", quietly = TRUE)) {
    stop("Package 'isotree' is required. Install with install.packages('isotree').")
  }
  sample_size <- min(config$sample_size %||% 256, nrow(data))
  model <- isotree::isolation.forest(
    data,
    ntrees = config$ntrees %||% 200,
    sample_size = sample_size,
    ndim = config$ndim %||% 1
  )
  scores <- as.numeric(predict(model, data, type = "score"))
  thr <- stats::quantile(scores, probs = config$threshold_quantile %||% 0.98, na.rm = TRUE)
  state <- new_detector_state(
    name = "isolation_forest",
    meta = list(
      model = model,
      threshold = thr,
      threshold_quantile = config$threshold_quantile %||% 0.98
    )
  )
  state
}

iso_partial_fit <- function(data, state, config, force_refit = FALSE) {
  if (!force_refit) return(state)
  iso_fit_initial(data, config)
}

iso_score <- function(data, state) {
  model <- state$meta$model
  as.numeric(predict(model, data, type = "score"))
}

iso_detect <- function(data, state, config, force_refit = FALSE) {
  if (force_refit) {
    state <- iso_partial_fit(data, state, config, force_refit = TRUE)
  }
  scores <- iso_score(data, state)
  thr <- state$meta$threshold
  is_anom <- scores > thr
  list(
    results = tibble::tibble(
      score_iso = scores,
      is_anomaly_iso = is_anom
    ),
    state = state
  )
}

