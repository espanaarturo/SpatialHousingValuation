test_that("feature expansion scales to target_dim and is non-NA", {
  df <- tibble::tibble(a = rnorm(20), b = rnorm(20))
  out <- expand_features(df, target_dim = 50, interaction = TRUE, rolling_stats = TRUE)
  expect_true(ncol(out) >= 50)
  expect_false(any(is.na(out)))
})

test_that("rolling PCA refits on schedule", {
  df <- matrix(rnorm(100 * 5), ncol = 5)
  st <- fit_pca_state(df, k = 3)
  st2 <- maybe_refit_pca(df, st, window_idx = 5, refit_every = 2)
  expect_equal(st2$k, 3)
  expect_true(!is.null(st2$rotation))
})

