library(ggplot2)
library(readr)
library(dplyr)
library(stringr)

theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(hjust = 0),
      legend.position = "right"
    )
)

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)

exp_summary_path <- "results/experiment_summary.csv"
exp_files <- list.files("results", pattern = "^exp_.*\\.csv$", full.names = TRUE)

save_plot <- function(p, name, width = 11, height = 6) {
  ggsave(file.path("results/figures", name), p, width = width, height = height, dpi = 300)
}

if (file.exists(exp_summary_path)) {
  exp <- readr::read_csv(exp_summary_path, show_col_types = FALSE)
  drift_labels <- c(
    abrupt_rate_hike = "Abrupt rate hike",
    gradual_seasonal = "Gradual seasonal",
    local_micro = "Local micro-market"
  )
  p_perf <- ggplot(exp, aes(target_dim, precision, color = drift)) +
    geom_point(size = 2.2) + geom_line(linewidth = 1) +
    labs(
      title = "Precision vs Dimensionality",
      subtitle = "Detection precision of the ensemble under three drift regimes.",
      x = "Target dimension",
      y = "Precision",
      color = "Drift regime"
    ) +
    scale_color_manual(values = c("#2980b9", "#27ae60", "#8e44ad"), labels = drift_labels, breaks = names(drift_labels)) +
    scale_x_continuous(breaks = c(100, 300, 500), expand = expansion(mult = c(0.02, 0.05)))
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
    ttd <- ttd %>% mutate(post_drift = factor(post_drift, levels = c(FALSE, TRUE), labels = c("Pre-drift", "Post-drift")))
    p_ttd <- ggplot(ttd, aes(post_drift, ttd, fill = post_drift)) +
      geom_boxplot() +
      labs(
        title = "Time-to-detection proxy",
        subtitle = "Distribution of the first anomaly window, pre- vs post-drift.",
        x = "Regime",
        y = "First anomaly window"
      ) +
      guides(fill = "none")
    save_plot(p_ttd, "time_to_detection.png")
  }

  if ("weight_baseline" %in% names(df_all)) {
    df_w <- df_all %>% filter(!is.na(weight_baseline) | !is.na(weight_iso) | !is.na(weight_ae))
    p_w <- ggplot(df_w, aes(win)) +
      geom_line(aes(y = weight_baseline, color = "baseline"), linewidth = 1.1) +
      geom_line(aes(y = weight_iso, color = "iso"), linewidth = 1.1)
    if ("weight_ae" %in% names(df_w) && any(!is.na(df_w$weight_ae))) {
      p_w <- p_w + geom_line(aes(y = weight_ae, color = "ae"), linewidth = 1.1)
    }
    p_w <- p_w +
      labs(
        title = "Ensemble weight trajectories",
        x = "Window",
        y = "Weight",
        color = "Model"
      ) +
      scale_color_manual(
        values = c(baseline = "#2c3e50", iso = "#2980b9", ae = "#8e44ad"),
        labels = c(baseline = "MAD baseline", iso = "Isolation Forest", ae = "Autoencoder")
      )
    save_plot(p_w, "ensemble_weights.png")
  }

  if ("top_features" %in% names(df_all)) {
    top_feat_df <- df_all %>%
      tidyr::separate_rows(top_features, sep = ";") %>%
      filter(!stringr::str_detect(top_features, "^PC")) %>%
      count(top_features, sort = TRUE) %>%
      slice_head(n = 10) %>%
      mutate(top_features = dplyr::recode(top_features,
        unemp_proxy = "Unemployment proxy",
        rate_proxy = "Rate proxy",
        days_on_market = "Days on market",
        .default = top_features
      ))
    if (nrow(top_feat_df)) {
      p_feat <- ggplot(top_feat_df, aes(reorder(top_features, n), n)) +
        geom_col(fill = "#2c3e50") +
        coord_flip() +
        labs(
          title = "Top explanation features",
          subtitle = "Features most frequently appearing in top-k anomaly explanations.",
          x = "Count in top-k explanations",
          y = "Feature"
        )
      save_plot(p_feat, "explanation_features.png", width = 6, height = 5)
    }
  }
}

