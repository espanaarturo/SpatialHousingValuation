#' Simple drift detection utilities
#'
#' Uses KS distance, mean/variance shift, and Page-Hinkley style rule.
drift_metrics <- function(ref, cur) {
  cols <- intersect(numeric_cols(ref), numeric_cols(cur))
  if (length(cols) == 0) stop("No numeric columns for drift detection.")

  ks_vals <- vapply(cols, function(col) {
    suppressWarnings(stats::ks.test(ref[[col]], cur[[col]])$statistic)
  }, numeric(1))

  mean_shift <- vapply(cols, function(col) {
    abs(mean(cur[[col]], na.rm = TRUE) - mean(ref[[col]], na.rm = TRUE))
  }, numeric(1))

  var_shift <- vapply(cols, function(col) {
    abs(stats::sd(cur[[col]], na.rm = TRUE) - stats::sd(ref[[col]], na.rm = TRUE))
  }, numeric(1))

  list(
    ks = mean(ks_vals, na.rm = TRUE),
    mean_shift = mean(mean_shift, na.rm = TRUE),
    var_shift = mean(var_shift, na.rm = TRUE)
  )
}

page_hinkley <- function(series, delta = 0.005, lambda = 50) {
  mean_val <- 0
  mT <- 0
  ph_stat <- 0
  for (x in series) {
    mean_val <- mean_val + (x - mean_val) / 2
    mT <- mT + x - mean_val - delta
    ph_stat <- min(ph_stat, mT)
    if ((mT - ph_stat) > lambda) return(TRUE)
  }
  FALSE
}

detect_drift <- function(ref, cur, thresholds = list(ks = 0.2, mean_shift = 50, var_shift = 30)) {
  metrics <- drift_metrics(ref, cur)
  drift_flag <- any(c(
    metrics$ks > thresholds$ks,
    metrics$mean_shift > thresholds$mean_shift,
    metrics$var_shift > thresholds$var_shift
  ))
  list(drift = drift_flag, metrics = metrics)
}

