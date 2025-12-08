test_that("simulate_stream reproducible with seed and drift/anomaly injection", {
  cfg <- list(
    n_steps = 50,
    markets = c("a", "b"),
    base_rate = 1000,
    anomaly_rate = 0.1,
    drift = list(enabled = TRUE, type = "abrupt_rate_hike", start_step = 25, magnitude = 0.2),
    seed = 123
  )
  s1 <- simulate_stream(cfg$n_steps, cfg$markets, cfg$base_rate, cfg$anomaly_rate, cfg$drift, seed = cfg$seed)
  s2 <- simulate_stream(cfg$n_steps, cfg$markets, cfg$base_rate, cfg$anomaly_rate, cfg$drift, seed = cfg$seed)
  expect_equal(s1$price, s2$price)
  expect_true(any(s1$is_anomaly_true))
  expect_true(any(s1$price[25:50] > s1$price[1:24]))
})

test_that("window_stream creates correct counts", {
  df <- tibble::tibble(x = 1:10)
  wins <- window_stream(df, size = 5, step = 2, type = "sliding")
  expect_equal(length(wins), 3)
  wins_tum <- window_stream(df, size = 5, step = 5, type = "tumbling")
  expect_equal(length(wins_tum), 2)
})

