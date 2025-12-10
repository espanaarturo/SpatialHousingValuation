# Minimal end-to-end runner for Milestone 1

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  invisible(NULL)
}

load_run_deps <- function(root_dir = ".") {
  root_dir <- normalizePath(root_dir)
  source(file.path(root_dir, "R/utils/helpers.R"))
  source(file.path(root_dir, "R/utils/logging.R"))
  source(file.path(root_dir, "R/detectors/detector_interface.R"))
  source(file.path(root_dir, "R/detectors/baseline_mad.R"))
  source(file.path(root_dir, "R/detectors/isolation_forest.R"))
  source(file.path(root_dir, "R/detectors/autoencoder_torch.R"))
  source(file.path(root_dir, "R/ensemble/weighted_ensemble.R"))
  source(file.path(root_dir, "R/explain/baseline_explain.R"))
  source(file.path(root_dir, "R/explain/iso_explain.R"))
  source(file.path(root_dir, "R/explain/ae_explain.R"))
  source(file.path(root_dir, "R/explain/pca_mapping.R"))
  source(file.path(root_dir, "R/streaming/stream_simulator.R"))
  source(file.path(root_dir, "R/streaming/windowing.R"))
  source(file.path(root_dir, "R/features/feature_expansion.R"))
  source(file.path(root_dir, "R/features/rolling_pca.R"))
  source(file.path(root_dir, "R/drift/drift_detection.R"))
}

normalize_cfg <- function(cfg) {
  # If already nested, return as-is
  if (!is.null(cfg$stream) && !is.null(cfg$window)) return(cfg)

  if (is.null(cfg) || length(cfg) == 0) cfg <- list()

  # defaults
  default_markets <- c("metro_north", "metro_south")
  if (is.null(cfg$stream) || !is.list(cfg$stream)) cfg$stream <- list()
  if (is.null(cfg$window) || !is.list(cfg$window)) cfg$window <- list()
  if (is.null(cfg$features) || !is.list(cfg$features)) cfg$features <- list()
  if (is.null(cfg$pca) || !is.list(cfg$pca)) cfg$pca <- list(k = 10, refit_every = 5)
  if (is.null(cfg$detectors) || !is.list(cfg$detectors) || is.atomic(cfg$detectors)) cfg$detectors <- list()
  if (is.null(cfg$ensemble) || !is.list(cfg$ensemble)) cfg$ensemble <- list()
  if (is.null(cfg$logging) || !is.list(cfg$logging)) cfg$logging <- list(out_dir = "results", output_name = "baseline_anomalies.csv")

  # stream
  cfg$stream$n_steps <- cfg$stream$n_steps %||% cfg$n_steps %||% 200
  cfg$stream$markets <- cfg$stream$markets %||% cfg$markets %||% default_markets
  cfg$stream$base_rate <- cfg$stream$base_rate %||% cfg$base_rate %||% 1000
  cfg$stream$anomaly_rate <- cfg$stream$anomaly_rate %||% cfg$anomaly_rate %||% 0.05
  cfg$stream$drift <- cfg$stream$drift %||% list(
    enabled = TRUE,
    type = cfg$drift_type %||% "abrupt_rate_hike",
    start_step = cfg$drift_start %||% 120,
    magnitude = cfg$drift_magnitude %||% 0.15
  )
  cfg$stream$seed <- cfg$stream$seed %||% cfg$seed

  # window
  cfg$window$type <- cfg$window$type %||% cfg$window_type %||% "sliding"
  cfg$window$size <- cfg$window$size %||% cfg$window_size %||% 50
  cfg$window$step <- cfg$window$step %||% cfg$window_step %||% 10

  # features
  cfg$features$target_dim <- cfg$features$target_dim %||% cfg$target_dim %||% 300
  cfg$features$interaction <- cfg$features$interaction %||% TRUE
  cfg$features$rolling_stats <- cfg$features$rolling_stats %||% TRUE
  cfg$features$noise_sd <- cfg$features$noise_sd %||% 0.1

  # detectors
  flat_det <- cfg$detectors
  if (is.atomic(flat_det)) {
    cfg$detectors <- list(
      baseline_mad = list(enabled = "baseline" %in% flat_det),
      isolation_forest = list(enabled = "iso" %in% flat_det),
      autoencoder_torch = list(enabled = "ae" %in% flat_det)
    )
  }
  cfg$detectors$baseline_mad <- cfg$detectors$baseline_mad %||% list(enabled = TRUE)
  cfg$detectors$isolation_forest <- cfg$detectors$isolation_forest %||% list(enabled = TRUE)
  cfg$detectors$autoencoder_torch <- cfg$detectors$autoencoder_torch %||% list(enabled = FALSE)

  # ensemble
  cfg$ensemble$enabled <- cfg$ensemble$enabled %||% cfg$ensemble %||% TRUE
  cfg$ensemble$alpha <- cfg$ensemble$alpha %||% 0.3
  cfg$ensemble$threshold <- cfg$ensemble$threshold %||% 1.0

  cfg
}

run_stream_with_cfg <- function(cfg, write_out = TRUE, quiet = FALSE, root_dir = ".") {
  cfg <- normalize_cfg(cfg)
  load_run_deps(root_dir)
  stream <- simulate_stream(
    n_steps = cfg$stream$n_steps,
    markets = cfg$stream$markets,
    base_rate = cfg$stream$base_rate,
    anomaly_rate = cfg$stream$anomaly_rate,
    drift = cfg$stream$drift,
    seed = cfg$stream$seed
  )
  log_event("info", "Generated stream", list(rows = nrow(stream)), quiet = quiet)

  win_list <- window_stream(
    data = stream,
    size = cfg$window$size,
    step = cfg$window$step,
    type = cfg$window$type
  )

  det_cfg <- cfg$detectors$baseline_mad
  det_state <- baseline_fit_initial(stream, det_cfg)

  iso_state <- NULL
  ae_state <- NULL

  # Initialize PCA on first window's expanded features
  first_win <- win_list[[1]]
  feat_cfg <- cfg$features
  pca_cfg <- cfg$pca
  first_feats <- expand_features(first_win, target_dim = feat_cfg$target_dim, interaction = feat_cfg$interaction, noise_sd = feat_cfg$noise_sd, rolling_stats = feat_cfg$rolling_stats)
  pca_state <- fit_pca_state(first_feats, k = pca_cfg$k)

  ref_window <- NULL
  ens_state <- init_ensemble_state(c("baseline", "iso", "ae"))

  outputs <- purrr::imap(win_list, function(win, idx) {
    feats <- expand_features(win, target_dim = feat_cfg$target_dim, interaction = feat_cfg$interaction, noise_sd = feat_cfg$noise_sd, rolling_stats = feat_cfg$rolling_stats)
    pca_state <<- maybe_refit_pca(feats, pca_state, window_idx = idx, refit_every = pca_cfg$refit_every)
    pca_proj <- transform_pca(feats, pca_state)

    # Drift detection using PCA-projected features
    if (is.null(ref_window)) {
      ref_window <<- as.data.frame(pca_proj)
      drift_res <- list(drift = FALSE, metrics = list())
    } else {
      drift_res <- detect_drift(ref_window, as.data.frame(pca_proj))
      if (drift_res$drift) ref_window <<- as.data.frame(pca_proj)
    }

    win_aug <- dplyr::bind_cols(win, as.data.frame(pca_proj))

    # Baseline
    res_base <- baseline_detect(win_aug, det_state, det_cfg)
    det_state <<- res_base$state

    # Isolation Forest
    res_iso <- NULL
    if (isTRUE(cfg$detectors$isolation_forest$enabled)) {
      if (is.null(iso_state)) {
        iso_state <<- iso_fit_initial(as.matrix(pca_proj), cfg$detectors$isolation_forest)
      } else if (drift_res$drift) {
        iso_state <<- iso_partial_fit(as.matrix(pca_proj), iso_state, cfg$detectors$isolation_forest, force_refit = TRUE)
      }
      res_iso <- iso_detect(as.matrix(pca_proj), iso_state, cfg$detectors$isolation_forest)
      iso_state <<- res_iso$state
    }

    # Autoencoder
    res_ae <- NULL
    if (isTRUE(cfg$detectors$autoencoder_torch$enabled)) {
      if (!requireNamespace("torch", quietly = TRUE)) {
        stop("torch not installed. Install with install.packages('torch'); torch::install_torch()")
      }
      if (is.null(ae_state)) {
        ae_state <<- ae_fit_initial(as.matrix(pca_proj), cfg$detectors$autoencoder_torch)
      } else if (drift_res$drift) {
        ae_state <<- ae_partial_fit(as.matrix(pca_proj), ae_state, cfg$detectors$autoencoder_torch, force_refit = TRUE)
      }
      res_ae <- ae_detect(as.matrix(pca_proj), ae_state, cfg$detectors$autoencoder_torch)
      ae_state <<- res_ae$state
    }

    # Aggregate results
    base_tbl <- res_base$results
    det_scores <- list(baseline = res_base$results$score)
    det_flags <- list(baseline = res_base$results$is_anomaly)
    det_thresholds <- list(baseline = det_state$meta$threshold_z)
    if (!is.null(res_iso)) {
      base_tbl <- dplyr::bind_cols(base_tbl, res_iso$results)
      det_scores$iso <- res_iso$results$score_iso
      det_flags$iso <- res_iso$results$is_anomaly_iso
      det_thresholds$iso <- iso_state$meta$threshold
    }
    if (!is.null(res_ae)) {
      base_tbl <- dplyr::bind_cols(base_tbl, res_ae$results)
      det_scores$ae <- res_ae$results$score_ae
      det_flags$ae <- res_ae$results$is_anomaly_ae
      det_thresholds$ae <- ae_state$meta$threshold
    }

    # Ensemble
    if (isTRUE(cfg$ensemble$enabled)) {
      norm_mat <- build_norm_scores(det_scores, det_thresholds)
      active_names <- colnames(norm_mat)
      weight_vec <- ens_state$weights[match(active_names, ens_state$names)]
      weight_vec <- normalize_weights(weight_vec)
      ens_score_vec <- as.numeric(norm_mat %*% weight_vec)
      ens_threshold <- cfg$ensemble$threshold %||% 1.0
      ens_flag <- ens_score_vec > ens_threshold

      ens_state <<- update_weights(
        ens_state,
        det_flags = det_flags,
        det_scores = det_scores,
        truth = base_tbl$is_anomaly_true,
        alpha = cfg$ensemble$alpha %||% 0.3
      )
    } else {
      ens_score_vec <- rep(NA_real_, nrow(base_tbl))
      ens_flag <- rep(FALSE, nrow(base_tbl))
    }

    # Explanations
    expl_base <- explain_baseline(win_aug)
    expl_iso <- if (!is.null(res_iso)) iso_explain(as.matrix(pca_proj), iso_state) else list(top_features = character(), text = "")
    expl_ae <- if (!is.null(res_ae)) {
      if (!is.null(ae_state)) ae_explain(as.matrix(pca_proj), ae_state) else list(top_features = character(), text = "")
    } else list(top_features = character(), text = "")

    top_feats <- unique(c(
      top_features_from_pca(pca_state, expl_iso$top_features, top_k = 5),
      top_features_from_pca(pca_state, expl_ae$top_features, top_k = 5),
      expl_base$top_features
    ))

    explanation_text <- paste(
      expl_base$text,
      expl_iso$text,
      expl_ae$text,
      sep = " | "
    )

    anomalies <- sum(ens_flag)
    log_event("info", "Window scored", list(window = idx, anomalies = anomalies, drift = drift_res$drift), quiet = quiet)

    # ensure original key fields are available for downstream plots
    if (!"price" %in% names(base_tbl) && "price" %in% names(win)) {
      base_tbl$price <- win$price
    }
    if (!"market" %in% names(base_tbl) && "market" %in% names(win)) {
      base_tbl$market <- win$market
    }
    if (!"window_id" %in% names(base_tbl)) {
      base_tbl$window_id <- idx
    }

    base_tbl$drift_detected <- drift_res$drift
    base_tbl$ks_metric <- drift_res$metrics$ks %||% NA
    base_tbl$mean_shift <- drift_res$metrics$mean_shift %||% NA
    base_tbl$var_shift <- drift_res$metrics$var_shift %||% NA
    base_tbl$score_ensemble <- ens_score_vec
    base_tbl$is_anomaly_ensemble <- ens_flag
    base_tbl$weight_baseline <- ens_state$weights[match("baseline", ens_state$names)]
    base_tbl$weight_iso <- ens_state$weights[match("iso", ens_state$names)]
    base_tbl$weight_ae <- ens_state$weights[match("ae", ens_state$names)]
    base_tbl$top_features <- paste(top_feats, collapse = ";")
    base_tbl$explanation_text <- explanation_text
    base_tbl
  })

  out <- dplyr::bind_rows(outputs, .id = "window_id")
  if (write_out) {
    if (!dir.exists(cfg$logging$out_dir)) dir.create(cfg$logging$out_dir, recursive = TRUE)
    outfile <- file.path(cfg$logging$out_dir, cfg$logging$output_name %||% "baseline_anomalies.csv")
    readr::write_csv(out, outfile)
    log_event("info", "Wrote anomaly log", list(path = outfile), quiet = quiet)
  }
  list(out = out, history = ens_state$history)
}

run_stream <- function(config_path = "configs/demo.yaml") {
  ensure_packages(c("yaml", "tibble", "dplyr", "purrr", "readr", "zoo"))
  load_run_deps(".")
  cfg <- yaml::read_yaml(config_path)
  log_event("info", "Loaded config", list(path = config_path))
  res <- run_stream_with_cfg(cfg, write_out = TRUE, root_dir = ".")
  invisible(res$out)
}

if (sys.nframe() == 0) {
  run_stream()
}

