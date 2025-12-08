#' Map PCA components back to original expanded features
top_features_from_pca <- function(pca_state, top_components, top_k = 5) {
  if (is.null(pca_state$rotation)) return(character())
  rot <- pca_state$rotation
  comp_idx <- suppressWarnings(as.integer(gsub("[^0-9]", "", top_components)))
  comp_idx <- comp_idx[comp_idx >= 1 & comp_idx <= ncol(rot)]
  if (length(comp_idx) == 0) return(character())
  load_abs <- rowSums(abs(rot[, comp_idx, drop = FALSE]))
  ord <- order(load_abs, decreasing = TRUE)
  rownames(rot)[ord][seq_len(min(top_k, length(ord)))]
}

