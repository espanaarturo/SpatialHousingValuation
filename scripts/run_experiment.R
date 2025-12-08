# Basic experiment runner sweeping drift and dimensionality

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  invisible(NULL)
}

run_experiment <- function(config_path = "configs/demo.yaml",
                           drift_types = c("abrupt_rate_hike", "gradual_seasonal", "local_micro"),
                           anomaly_rates = c(0.05, 0.1),
                           target_dims = c(100, 500)) {
  ensure_packages(c("yaml", "dplyr", "purrr", "readr"))
  source("scripts/run_stream.R")

  base_cfg <- yaml::read_yaml(config_path)

  grid <- expand.grid(
    drift = drift_types,
    anomaly = anomaly_rates,
    dim = target_dims,
    stringsAsFactors = FALSE
  )

  results <- purrr::pmap_dfr(grid, function(drift, anomaly, dim) {
    cfg <- base_cfg
    cfg$stream$drift$enabled <- drift != "none"
    cfg$stream$drift$type <- drift
    cfg$stream$anomaly_rate <- anomaly
    cfg$features$target_dim <- dim
    cfg$logging$output_name <- sprintf("exp_%s_anom%s_dim%s.csv", drift, anomaly, dim)

    tmp_cfg <- tempfile(fileext = ".yaml")
    yaml::write_yaml(cfg, tmp_cfg)

    out <- run_stream(tmp_cfg)
    detected <- sum(out$is_anomaly, na.rm = TRUE)
    truth <- sum(out$is_anomaly_true, na.rm = TRUE)
    tp <- sum(out$is_anomaly & out$is_anomaly_true, na.rm = TRUE)
    precision <- ifelse(detected > 0, tp / detected, NA)
    recall <- ifelse(truth > 0, tp / truth, NA)
    drift_rate <- mean(out$drift_detected, na.rm = TRUE)

    dplyr::tibble(
      drift = drift,
      anomaly_rate = anomaly,
      target_dim = dim,
      detected = detected,
      true_anomalies = truth,
      precision = precision,
      recall = recall,
      drift_detect_rate = drift_rate,
      output = cfg$logging$output_name
    )
  })

  if (!dir.exists("results")) dir.create("results", recursive = TRUE)
  readr::write_csv(results, file.path("results", "experiment_summary.csv"))
  message("Experiment summary written to results/experiment_summary.csv")
  invisible(results)
}

if (sys.nframe() == 0) {
  run_experiment()
}

