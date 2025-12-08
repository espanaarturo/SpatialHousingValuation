library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(purrr)

source("../scripts/run_stream.R")
source("../scripts/run_experiment.R")

detector_available <- function(det) {
  if (det == "autoencoder_torch") {
    requireNamespace("torch", quietly = TRUE)
  } else TRUE
}

ae_ok <- detector_available("autoencoder_torch")
detector_choices <- c("baseline", "iso", if (ae_ok) "ae")
detector_selected <- c("baseline", "iso")

theme <- bs_theme(version = 5, bootswatch = "cosmo", base_font = font_google("Inter"))

ui <- page_navbar(
  title = "Housing Anomaly Lab",
  theme = theme,
  sidebar = NULL,
  nav("Live Stream", icon = icon("wave-square"),
      layout_sidebar(
        sidebar = sidebar(
          selectInput("drift_type", "Drift type", c("abrupt_rate_hike","gradual_seasonal","local_micro","none")),
          sliderInput("drift_mag", "Drift magnitude", 0, 0.3, 0.15, step = 0.01),
          sliderInput("anom_rate", "Anomaly rate", 0, 0.2, 0.05, step = 0.01),
          sliderInput("dim", "Dimensionality target", 50, 600, 300, step = 50),
          selectInput("window_type", "Window type", c("sliding","tumbling")),
          checkboxGroupInput("detectors", "Detectors", choices = detector_choices, selected = detector_selected),
          checkboxInput("ensemble_on", "Adaptive Ensemble", TRUE),
          actionButton("step_btn", "Step", class = "btn-primary"),
          actionButton("auto_btn", "Play", class = "btn-success"),
          actionButton("stop_btn", "Pause", class = "btn-outline-secondary"),
          actionButton("reset_btn", "Reset", class = "btn-outline-danger"),
          actionButton("export_btn", "Export run", class = "btn-outline-primary")
        ),
        main = fluidPage(
          fluidRow(
            column(6, card(card_header("Market & Anomalies"), plotOutput("plot_market", height = 300))),
            column(6, card(card_header("Drift (KS)"), plotOutput("plot_drift", height = 300)))
          ),
          fluidRow(
            column(6, card(card_header("Ensemble Weights"), plotOutput("plot_weights", height = 250))),
            column(6, card(card_header("Ensemble Score vs Threshold"), plotOutput("plot_thresholds", height = 250)))
          ),
          fluidRow(
            column(12, card(card_header("Run summary"), tableOutput("run_summary")))
          ),
          card(card_header("Preview"), tableOutput("table_head"))
        )
      )
  ),
  nav("Explanations", icon = icon("list"),
      fluidPage(
        card(
          card_header("Select window"),
          selectInput("expl_window", "Window", choices = NULL)
        ),
        card(
          card_header("Explanation"),
          verbatimTextOutput("expl_text"),
          tableOutput("expl_scores")
        )
      )
  ),
  nav("Experiment Viewer", icon = icon("chart-bar"),
      fluidPage(
        card(
          card_header("Run preset sweep"),
          actionButton("run_exp", "Run preset experiment", class = "btn-primary")
        ),
        card(
          card_header("Results"),
          tableOutput("exp_table"),
          plotOutput("exp_plot", height = 300)
        )
      )
  )
)

server <- function(input, output, session) {
  auto_running <- reactiveVal(FALSE)
  current_idx <- reactiveVal(1)
  run_cache <- reactiveVal(NULL)

  current_cfg <- reactive({
    cfg <- yaml::read_yaml("../configs/demo.yaml")
    cfg$stream$drift$enabled <- input$drift_type != "none"
    cfg$stream$drift$type <- input$drift_type
    cfg$stream$drift$magnitude <- input$drift_mag
    cfg$stream$anomaly_rate <- input$anom_rate
    cfg$features$target_dim <- input$dim
    cfg$window$type <- input$window_type
    cfg$detectors$baseline_mad$enabled <- "baseline" %in% input$detectors
    cfg$detectors$isolation_forest$enabled <- "iso" %in% input$detectors
    cfg$detectors$autoencoder_torch$enabled <- "ae" %in% input$detectors && detector_available("autoencoder_torch")
    cfg$ensemble$enabled <- isTRUE(input$ensemble_on)
    cfg$logging$output_name <- "shiny_stream.csv"
    cfg
  })

  generate_run <- function() {
    cfg <- current_cfg()
    res <- run_stream_with_cfg(cfg, write_out = FALSE, quiet = TRUE)
    run_cache(list(data = res$out, cfg = cfg, max_win = max(as.numeric(res$out$window_id))))
    current_idx(1)
    auto_running(FALSE)
    update_select_input_choices()
  }

  observeEvent(list(input$drift_type, input$drift_mag, input$anom_rate, input$dim, input$window_type, input$detectors, input$ensemble_on), {
    generate_run()
  })

  observeEvent(input$reset_btn, {
    generate_run()
  })

  observeEvent(input$step_btn, {
    req(run_cache())
    idx <- current_idx()
    if (idx < run_cache()$max_win) current_idx(idx + 1)
  })

  observeEvent(input$auto_btn, {
    auto_running(TRUE)
  })
  observeEvent(input$stop_btn, {
    auto_running(FALSE)
  })

  observe({
    req(run_cache())
    if (!isTRUE(auto_running())) return()
    if (current_idx() >= run_cache()$max_win) {
      auto_running(FALSE)
      return()
    }
    invalidateLater(800, session)
    current_idx(current_idx() + 1)
  })

  observeEvent(input$export_btn, {
    req(run_cache())
    df <- run_cache()$data
    if (!dir.exists("results")) dir.create("results", recursive = TRUE)
    readr::write_csv(df, "results/shiny_stream.csv")
    showNotification("Exported to results/shiny_stream.csv", type = "message")
  })

  run_stream_cfg <- function(cfg) {
    # copy of run_stream but returning data without writing disk
    ensure_packages(c("zoo","yaml","tibble","dplyr","purrr","readr"))
    source("../R/utils/helpers.R")
    source("../R/utils/logging.R")
    source("../R/detectors/detector_interface.R")
    source("../R/detectors/baseline_mad.R")
    source("../R/detectors/isolation_forest.R")
    source("../R/detectors/autoencoder_torch.R")
    source("../R/ensemble/weighted_ensemble.R")
    source("../R/explain/baseline_explain.R")
    source("../R/explain/iso_explain.R")
    source("../R/explain/ae_explain.R")
    source("../R/explain/pca_mapping.R")
    source("../R/streaming/stream_simulator.R")
    source("../R/streaming/windowing.R")
    source("../R/features/feature_expansion.R")
    source("../R/features/rolling_pca.R")
    source("../R/drift/drift_detection.R")
    res <- run_stream_with_cfg(cfg)
    res
  }

  current_data <- reactive({
    req(run_cache())
    df <- run_cache()$data
    df %>% filter(as.numeric(window_id) <= current_idx())
  })

  output$plot_market <- renderPlot({
    req(current_data())
    df <- current_data()
    ggplot(df, aes(timestamp, price)) +
      geom_line(color = "#2c3e50") +
      geom_point(data = subset(df, is_anomaly_ensemble), aes(y = price, color = "Anomaly"), size = 1.5) +
      scale_color_manual(values = c("Anomaly" = "#e74c3c"), guide = guide_legend(title = "")) +
      labs(title = "Market price with anomalies", x = "Time", y = "Price") +
      theme_minimal()
  })

  output$plot_drift <- renderPlot({
    req(current_data())
    df <- current_data()
    ggplot(df, aes(as.numeric(window_id), ks_metric, color = drift_detected)) +
      geom_line() +
      geom_point() +
      labs(title = "Drift metric (KS)", x = "Window", y = "KS") +
      theme_minimal() +
      scale_color_manual(values = c("TRUE" = "#e67e22", "FALSE" = "#2980b9"), guide = guide_legend(title = "Drift"))
  })

  output$plot_weights <- renderPlot({
    req(current_data())
    df <- current_data()
    plt <- ggplot(df, aes(as.numeric(window_id)))
    plt <- plt + geom_line(aes(y = weight_baseline, color = "baseline"))
    if ("weight_iso" %in% names(df)) plt <- plt + geom_line(aes(y = weight_iso, color = "iso"))
    if ("weight_ae" %in% names(df) && any(!is.na(df$weight_ae))) plt <- plt + geom_line(aes(y = weight_ae, color = "ae"))
    plt + labs(title = "Ensemble weights", x = "Window", y = "Weight") +
      scale_color_manual(values = c(baseline = "#2c3e50", iso = "#2980b9", ae = "#8e44ad")) +
      theme_minimal()
  })

  output$plot_thresholds <- renderPlot({
    req(current_data())
    df <- current_data()
    ggplot(df, aes(as.numeric(window_id))) +
      geom_line(aes(y = score_ensemble, color = "score")) +
      geom_hline(yintercept = 1.0, linetype = "dashed", color = "#7f8c8d") +
      labs(title = "Ensemble score vs threshold", x = "Window", y = "Score") +
      scale_color_manual(values = c(score = "#16a085"), guide = "none") +
      theme_minimal()
  })

  output$table_head <- renderTable({
    req(current_data())
    df <- current_data()
    cols <- intersect(c("window_id","timestamp","score","score_iso","score_ae","score_ensemble","is_anomaly_ensemble","top_features"), names(df))
    head(df[, cols, drop = FALSE], 5)
  })

  output$run_summary <- renderTable({
    req(run_cache())
    cfg <- run_cache()$cfg
    data.frame(
      drift_type = cfg$stream$drift$type,
      drift_magnitude = cfg$stream$drift$magnitude,
      anomaly_rate = cfg$stream$anomaly_rate,
      target_dim = cfg$features$target_dim,
      window_type = cfg$window$type,
      detectors = paste(c(
        if (cfg$detectors$baseline_mad$enabled) "baseline",
        if (cfg$detectors$isolation_forest$enabled) "iso",
        if (isTRUE(cfg$detectors$autoencoder_torch$enabled)) "ae"), collapse = ", "),
      ensemble = ifelse(cfg$ensemble$enabled, "on", "off"),
      stringsAsFactors = FALSE
    )
  })

  update_select_input_choices <- function() {
    req(run_cache())
    windows <- unique(run_cache()$data$window_id)
    updateSelectInput(session, "expl_window", choices = windows, selected = tail(windows, 1))
  }

  output$expl_text <- renderText({
    req(run_cache(), input$expl_window)
    df <- run_cache()$data %>% filter(window_id == input$expl_window)
    df$explanation_text[1]
  })

  output$expl_scores <- renderTable({
    req(run_cache(), input$expl_window)
    df <- run_cache()$data %>% filter(window_id == input$expl_window)
    cols <- intersect(c("score","score_iso","score_ae","score_ensemble","is_anomaly_true","is_anomaly_ensemble"), names(df))
    df %>% select(all_of(cols)) %>% head(10)
  })

  exp_results <- reactiveVal(NULL)
  observeEvent(input$run_exp, {
    exp_results(run_experiment("../configs/demo.yaml", drift_types = c("abrupt_rate_hike","gradual_seasonal"), anomaly_rates = c(0.05), target_dims = c(100,300)))
  })

  output$exp_table <- renderTable({
    req(exp_results())
    exp_results()
  })

  output$exp_plot <- renderPlot({
    req(exp_results())
    df <- exp_results()
    ggplot(df, aes(target_dim, precision, color = drift)) +
      geom_point() + geom_line() + theme_minimal() + labs(y = "Precision", x = "Target Dim")
  })
}

shinyApp(ui, server)

