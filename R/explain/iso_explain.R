#' Approximate permutation impact for Isolation Forest on PCA features
iso_explain <- function(pca_proj, iso_state, top_k = 3) {
  if (is.null(iso_state)) return(list(top_features = character(), text = "ISO not fitted"))
  base_scores <- iso_score(pca_proj, iso_state)
  p <- ncol(pca_proj)
  k <- min(top_k, p)
  impacts <- numeric(p)
  for (j in seq_len(p)) {
    permuted <- pca_proj
    permuted[, j] <- sample(permuted[, j])
    scores_perm <- iso_score(permuted, iso_state)
    impacts[j] <- mean(scores_perm - base_scores, na.rm = TRUE)
  }
  ord <- order(abs(impacts), decreasing = TRUE)
  top_pcs <- paste0("PC", ord[seq_len(k)])
  list(
    top_features = top_pcs,
    impacts = impacts[ord[seq_len(k)]],
    text = sprintf("Isolation Forest sensitive to %s", paste(top_pcs, collapse = ", "))
  )
}

