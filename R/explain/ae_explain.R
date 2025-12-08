#' Explain autoencoder by reconstruction error per PCA dimension
ae_explain <- function(pca_proj, ae_state, top_k = 3) {
  if (is.null(ae_state)) return(list(top_features = character(), text = "AE not fitted"))
  x <- scale(pca_proj, center = ae_state$meta$center, scale = ae_state$meta$scale + 1e-6)
  tens <- torch::torch_tensor(as.matrix(x), dtype = torch::torch_float(), device = ae_state$meta$device)
  preds <- ae_state$meta$model(tens)$to(device = "cpu")
  recon <- as.matrix(preds)
  errs <- (as.matrix(x) - recon)^2
  dim_err <- colMeans(errs, na.rm = TRUE)
  ord <- order(dim_err, decreasing = TRUE)
  top_pcs <- paste0("PC", ord[seq_len(min(top_k, length(ord)))])
  list(
    top_features = top_pcs,
    text = sprintf("AE recon error highest on %s", paste(top_pcs, collapse = ", "))
  )
}

