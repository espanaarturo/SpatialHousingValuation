#' Rolling / periodic PCA refit
#'
#' Maintains PCA state and refits on schedule.
fit_pca_state <- function(data, k = 10, center = TRUE, scale. = TRUE) {
  pca <- stats::prcomp(data, center = center, scale. = scale.)
  list(
    rotation = pca$rotation[, seq_len(min(k, ncol(pca$rotation))), drop = FALSE],
    center = if (center) pca$center else rep(0, ncol(data)),
    scale = if (scale.) pca$scale else rep(1, ncol(data)),
    k = k,
    last_refit = 0,
    feature_names = colnames(data)
  )
}

transform_pca <- function(data, state) {
  x <- scale(data, center = state$center, scale = state$scale)
  as.matrix(x) %*% state$rotation
}

maybe_refit_pca <- function(data, state, window_idx, refit_every = 5) {
  if (window_idx - state$last_refit >= refit_every) {
    state <- fit_pca_state(data, k = state$k, center = TRUE, scale. = TRUE)
    state$last_refit <- window_idx
  }
  state
}

