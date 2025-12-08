test_that("baseline detector interface works", {
  df <- simulate_stream(30, c("a"), drift = list(enabled = FALSE), anomaly_rate = 0.1)
  st <- baseline_fit_initial(df, list(threshold_z = 3))
  res <- baseline_detect(df, st, list(threshold_z = 3))
  expect_equal(nrow(res$results), nrow(df))
})

test_that("iso detector scores", {
  skip_if_not_installed("isotree")
  df <- matrix(rnorm(50 * 5), ncol = 5)
  st <- iso_fit_initial(df, list(ntrees = 50, sample_size = 32))
  res <- iso_detect(df, st, list())
  expect_equal(nrow(res$results), nrow(df))
})

test_that("autoencoder detector skips gracefully without torch", {
  if (!requireNamespace("torch", quietly = TRUE)) {
    st <- ae_fit_initial(matrix(rnorm(20), ncol = 2), list())
    expect_null(st)
  } else {
    succeed()
  }
})

