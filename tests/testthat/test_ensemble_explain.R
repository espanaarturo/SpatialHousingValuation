test_that("ensemble weight update normalizes", {
  st <- init_ensemble_state(c("a","b"))
  scores <- list(a = c(1,2,3), b = c(1,1,1))
  flags <- list(a = c(TRUE, FALSE, TRUE), b = c(FALSE, FALSE, FALSE))
  truth <- c(TRUE, FALSE, TRUE)
  st2 <- update_weights(st, det_flags = flags, det_scores = scores, truth = truth, alpha = 0.5)
  expect_equal(sum(st2$weights), 1, tolerance = 1e-6)
})

test_that("explanations map PCA back to features", {
  df <- matrix(rnorm(30), ncol = 3)
  colnames(df) <- c("f1","f2","f3")
  st <- fit_pca_state(df, k = 2)
  feats <- top_features_from_pca(st, c("PC1"), top_k = 2)
  expect_true(all(feats %in% c("f1","f2","f3")))
})

