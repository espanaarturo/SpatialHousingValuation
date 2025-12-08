#' Feature expansion to scale dimensionality
#'
#' @param data tibble/data.frame with base numeric features
#' @param target_dim desired approximate dimensionality
#' @param interaction logical include pairwise interactions
#' @param noise_sd standard deviation for synthetic noise features
#' @param rolling_stats logical include rolling stats if time-ordered
expand_features <- function(data,
                            target_dim = 200,
                            interaction = TRUE,
                            noise_sd = 0.1,
                            rolling_stats = TRUE) {
  base_numeric <- data[, numeric_cols(data), drop = FALSE]
  out <- base_numeric

  if (interaction && ncol(base_numeric) > 1) {
    combs <- utils::combn(names(base_numeric), 2, simplify = FALSE)
    inter_df <- lapply(combs, function(pair) {
      out_col <- base_numeric[[pair[[1]]]] * base_numeric[[pair[[2]]]]
      out_col
    })
    names(inter_df) <- paste0("int_", vapply(combs, paste, collapse = "x", FUN.VALUE = character(1)))
    out <- cbind(out, as.data.frame(inter_df))
  }

  if (rolling_stats) {
    k <- min(10, nrow(data))
    roll_mean <- zoo::rollapply(base_numeric, k, mean, align = "right", fill = NA, by.column = TRUE)
    roll_sd <- zoo::rollapply(base_numeric, k, sd, align = "right", fill = NA, by.column = TRUE)
    colnames(roll_mean) <- paste0(colnames(base_numeric), "_rollmean")
    colnames(roll_sd) <- paste0(colnames(base_numeric), "_rollsd")
    out <- cbind(out, roll_mean, roll_sd)
  }

  # Add synthetic noise features to reach target dimensionality
  need <- max(0, target_dim - ncol(out))
  if (need > 0) {
    noise_mat <- matrix(rnorm(n = nrow(out) * need, sd = noise_sd), nrow = nrow(out))
    colnames(noise_mat) <- paste0("noise_", seq_len(need))
    out <- cbind(out, noise_mat)
  }
  out[is.na(out)] <- 0
  out
}

