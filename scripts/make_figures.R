library(ggplot2)
library(readr)
library(dplyr)
library(stringr)

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)

exp_summary_path <- "results/experiment_summary.csv"
exp_files <- list.files("results", pattern = "^exp_.*\\.csv$", full.names = TRUE)

save_plot <- function(p, name, width = 9, height = 5.5) {
  ggsave(file.path("results/figures", name), p, width = width, height = height, dpi = 300)
}

if (file.exists(exp_summary_path)) {
  exp <- readr::read_csv(exp_summary_path, show_col_types = FALSE)
  p_perf <- ggplot(exp, aes(target_dim, precision, color = drift)) +
    geom_point() + geom_line() +
    labs(title = "Precision vs Dimensionality", x = "Target Dim", y = "Precision") +
    theme_minimal()
  save_plot(p_perf, "precision_vs_dim.png")
}

if (length(exp_files)) {
  df_all <- purrr::map_dfr(exp_files, readr::read_csv, show_col_types = FALSE, .id = "file")
  df_all <- df_all %>%
    mutate(win = as.numeric(window_id)) %>%
    group_by(file) %>%
    mutate(post_drift = win >= max(win) / 2) %>%
    ungroup()

  # Proxy time-to-detection: first anomaly per file pre/post
  ttd <- df_all %>%
    filter(is_anomaly_ensemble) %>%
    group_by(file, post_drift) %>%
    summarise(ttd = min(win), .groups = "drop")

  if (nrow(ttd)) {
    p_ttd <- ggplot(ttd, aes(post_drift, ttd, fill = post_drift)) +
      geom_boxplot() + labs(title = "Time-to-detection proxy", x = "Post drift?", y = "First anomaly window") +
      theme_minimal()
    save_plot(p_ttd, "time_to_detection.png")
  }

  if ("weight_baseline" %in% names(df_all)) {
    df_w <- df_all %>% filter(!is.na(weight_baseline) | !is.na(weight_iso) | !is.na(weight_ae))
    p_w <- ggplot(df_w, aes(win)) +
      geom_line(aes(y = weight_baseline, color = "baseline")) +
      geom_line(aes(y = weight_iso, color = "iso"))
    if ("weight_ae" %in% names(df_w) && any(!is.na(df_w$weight_ae))) {
      p_w <- p_w + geom_line(aes(y = weight_ae, color = "ae"))
    }
    p_w <- p_w +
      labs(title = "Ensemble weight trajectories", x = "Window", y = "Weight") +
      scale_color_manual(values = c(baseline = "#2c3e50", iso = "#2980b9", ae = "#8e44ad")) +
      theme_minimal()
    save_plot(p_w, "ensemble_weights.png")
  }

  if ("top_features" %in% names(df_all)) {
    top_feat_df <- df_all %>%
      tidyr::separate_rows(top_features, sep = ";") %>%
      filter(!stringr::str_detect(top_features, "^PC")) %>%
      count(top_features, sort = TRUE) %>%
      slice_head(n = 10)
    if (nrow(top_feat_df)) {
      p_feat <- ggplot(top_feat_df, aes(reorder(top_features, n), n)) +
        geom_col(fill = "#2c3e50") +
        coord_flip() +
        labs(title = "Top explanation features", x = "Feature", y = "Count in top lists") +
        theme_minimal()
      save_plot(p_feat, "explanation_features.png", width = 6, height = 5)
    }
  }
}

