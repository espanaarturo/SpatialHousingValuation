#' Streaming simulator for housing signals
#'
#' Generates synthetic housing events with optional anomaly rate.
simulate_stream <- function(n_steps = NULL,
                            markets,
                            base_rate = 1000,
                            anomaly_rate = 0.05,
                            drift = list(enabled = FALSE, type = "none", start_step = 1, magnitude = 0.1),
                            seed = NULL,
                            start_time = NULL,
                            step = "1 min",
                            fallback_n = 200) {
  if (!is.null(seed)) set.seed(seed)

  # derive n_steps if missing
  if (is.null(n_steps)) n_steps <- fallback_n
  n_steps <- as.integer(n_steps)
  stopifnot(length(n_steps) == 1, !is.na(n_steps), n_steps > 0)

  if (is.null(markets) || length(markets) == 0) {
    markets <- c("metro_north", "metro_south")
  }

  if (is.null(start_time)) start_time <- Sys.time()
  timestamps <- seq.POSIXt(from = start_time, by = step, length.out = n_steps)
  market <- sample(markets, n_steps, replace = TRUE)
  price <- stats::rnorm(n_steps, mean = base_rate, sd = 80)
  rent <- stats::rnorm(n_steps, mean = base_rate / 3, sd = 30)
  dom <- stats::rpois(n_steps, lambda = 25)
  inventory <- stats::rpois(n_steps, lambda = 200)
  rate_proxy <- stats::rnorm(n_steps, mean = 0.04, sd = 0.005)
  unemp_proxy <- stats::rnorm(n_steps, mean = 0.06, sd = 0.01)

  # Drift injection
  if (isTRUE(drift$enabled)) {
    start <- drift$start_step %||% 1
    mag <- drift$magnitude %||% 0.1
    if (drift$type == "abrupt_rate_hike") {
      price[start:n_steps] <- price[start:n_steps] + base_rate * mag
      rate_proxy[start:n_steps] <- rate_proxy[start:n_steps] + 0.01 * mag
    } else if (drift$type == "gradual_seasonal") {
      idx <- seq_len(n_steps)
      trend <- sin(seq(0, pi, length.out = n_steps)) * mag * base_rate
      price <- price + trend
      rent <- rent + trend * 0.3
    } else if (drift$type == "local_micro") {
      target_market <- markets[[1]]
      mask <- market == target_market & seq_len(n_steps) >= start
      price[mask] <- price[mask] + base_rate * mag * 1.2
      inventory[mask] <- inventory[mask] + 50 * mag
    }
  }

  # Inject anomalies as upward price shocks
  n_anom <- ceiling(n_steps * anomaly_rate)
  anom_idx <- sample(seq_len(n_steps), n_anom)
  price[anom_idx] <- price[anom_idx] + stats::rnorm(n_anom, mean = 400, sd = 50)

  out <- tibble::tibble(
    timestamp = timestamps,
    window_id = seq_len(n_steps),
    market = market,
    price = price,
    rent = rent,
    days_on_market = dom,
    inventory = inventory,
    rate_proxy = rate_proxy,
    unemp_proxy = unemp_proxy,
    is_anomaly_true = seq_len(n_steps) %in% anom_idx
  )

  if (!("price" %in% names(out)) && "idxprice" %in% names(out)) {
    out <- dplyr::rename(out, price = idxprice)
  }

  required_cols <- c("timestamp", "price", "window_id")
  missing_cols <- setdiff(required_cols, names(out))
  if (length(missing_cols)) {
    warning("simulate_stream missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  out
}


