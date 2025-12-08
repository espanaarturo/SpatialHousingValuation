#' Explain baseline MAD by top robust z contributors
explain_baseline <- function(data, scores_vec, top_k = 5) {
  cols <- numeric_cols(data)
  contrib <- vapply(cols, function(col) {
    x <- data[[col]]
    med <- stats::median(x, na.rm = TRUE)
    mad_val <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
    mean(abs(x - med) / (mad_val + 1e-6), na.rm = TRUE)
  }, numeric(1))
  ord <- order(contrib, decreasing = TRUE)
  top_feats <- cols[ord][seq_len(min(top_k, length(ord)))]
  list(
    top_features = top_feats,
    text = sprintf("Baseline MAD top contributors: %s", paste(top_feats, collapse = ", "))
  )
}

