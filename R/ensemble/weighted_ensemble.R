#' Weighted ensemble with adaptive weights
#'
#' Weights update via EMA of detector performance (F1 by default) when truth is available.
#' Fallback heuristic uses inverse variance stability of scores.

init_ensemble_state <- function(detectors) {
  n <- length(detectors)
  list(
    weights = rep(1 / n, n),
    names = detectors,
    history = list()
  )
}

normalize_weights <- function(w) {
  w <- pmax(w, 1e-6)
  w / sum(w)
}

compute_perf <- function(flags, truth) {
  if (all(is.na(truth))) return(NA_real_)
  tp <- sum(flags & truth, na.rm = TRUE)
  fp <- sum(flags & !truth, na.rm = TRUE)
  fn <- sum(!flags & truth, na.rm = TRUE)
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), NA)
  recall <- ifelse(tp + fn > 0, tp / (tp + fn), NA)
  if (is.na(precision) || is.na(recall) || (precision + recall) == 0) return(NA_real_)
  2 * precision * recall / (precision + recall)
}

update_weights <- function(state, det_flags, det_scores, truth, alpha = 0.3) {
  active <- names(det_flags)
  current_w <- state$weights[match(active, state$names)]
  perf <- vapply(active, function(nm) compute_perf(det_flags[[nm]], truth), numeric(1))

  if (all(is.na(perf))) {
    # Fallback: inverse variance of scores as stability proxy
    vars <- vapply(det_scores, stats::var, numeric(1), na.rm = TRUE)
    inv_var <- 1 / (vars + 1e-6)
    new_w <- normalize_weights(inv_var)
  } else {
    perf[is.na(perf)] <- min(perf, na.rm = TRUE)
    new_w <- normalize_weights(alpha * perf + (1 - alpha) * current_w)
  }

  # write back into full vector
  full_w <- state$weights
  full_w[match(active, state$names)] <- new_w
  state$weights <- normalize_weights(full_w)
  state$history[[length(state$history) + 1]] <- new_w
  state
}

ensemble_score <- function(norm_scores, weights) {
  as.numeric(norm_scores %*% weights)
}

build_norm_scores <- function(det_scores, det_thresholds) {
  # Normalize scores by threshold to roughly scale to ~1 at threshold
  mats <- mapply(function(scores, thr) {
    pmin(scores / (thr + 1e-6), 5)
  }, det_scores, det_thresholds, SIMPLIFY = FALSE)
  out <- do.call(cbind, mats)
  colnames(out) <- names(det_scores)
  out
}

