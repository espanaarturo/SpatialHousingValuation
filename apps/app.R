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
          actionButton("step_btn", "Step (advance one window)", class = "btn-primary"),
          actionButton("auto_btn", "Auto-play", class = "btn-success"),
          actionButton("stop_btn", "Stop", class = "btn-outline-secondary")
        ),
        main = fluidPage(
          fluidRow(
            column(6, plotOutput("plot_market", height = 300)),
            column(6, plotOutput("plot_drift", height = 300))
          ),
          fluidRow(
            column(6, plotOutput("plot_weights", height = 250)),
            column(6, plotOutput("plot_thresholds", height = 250))
          ),
          tableOutput("table_head")
        )
      )
  ),
  nav("Explanations", icon = icon("list"),
      fluidPage(
        selectInput("expl_window", "Select window", choices = NULL),
        verbatimTextOutput("expl_text"),
        tableOutput("expl_scores")
      )
  ),
  nav("Experiment Viewer", icon = icon("chart-bar"),
      fluidPage(
        actionButton("run_exp", "Run preset experiment", class = "btn-primary"),
        tableOutput("exp_table"),
        plotOutput("exp_plot", height = 300)
      )
  )
)

server <- function(input, output, session) {
  auto_running <- reactiveVal(FALSE)
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

  stream_state <- reactiveValues(data = NULL, idx = 0, windows = NULL, pca = NULL, history = NULL)

  observeEvent(input$step_btn, {
    run_once()
  })

  run_once <- function() {
    cfg <- current_cfg()
    res <- run_stream_cfg(cfg)
    stream_state$data <- res$out
    stream_state$history <- res$history
    update_select_input_choices()
  }

  observeEvent(input$auto_btn, {
    auto_running(TRUE)
  })
  observeEvent(input$stop_btn, {
    auto_running(FALSE)
  })

  auto_inv <- reactiveTimer(1000)
  observe({
    if (isTRUE(auto_running())) {
      auto_inv()
      run_once()
    }
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

  output$plot_market <- renderPlot({
    req(stream_state$data)
    df <- stream_state$data
    ggplot(df, aes(timestamp, price)) +
      geom_line(color = "#2c3e50") +
      geom_point(data = subset(df, is_anomaly_ensemble), aes(y = price), color = "#e74c3c", size = 1.5) +
      theme_minimal()
  })

  output$plot_drift <- renderPlot({
    req(stream_state$data)
    df <- stream_state$data
    ggplot(df, aes(as.numeric(window_id), ks_metric)) +
      geom_line(color = "#2980b9") +
      geom_point(aes(color = drift_detected)) +
      labs(x = "Window", y = "KS metric") +
      theme_minimal()
  })

  output$plot_weights <- renderPlot({
    req(stream_state$data)
    df <- stream_state$data
    ggplot(df, aes(as.numeric(window_id))) +
      geom_line(aes(y = weight_baseline, color = "baseline")) +
      geom_line(aes(y = weight_iso, color = "iso")) +
      geom_line(aes(y = weight_ae, color = "ae")) +
      labs(x = "Window", y = "Weight") +
      theme_minimal()
  })

  output$plot_thresholds <- renderPlot({
    req(stream_state$data)
    df <- stream_state$data
    ggplot(df, aes(as.numeric(window_id))) +
      geom_line(aes(y = score_ensemble, color = "ensemble_score")) +
      geom_hline(aes(yintercept = 1.0, color = "ensemble_threshold"), linetype = "dashed") +
      theme_minimal()
  })

  output$table_head <- renderTable({
    req(stream_state$data)
    cols <- intersect(c("window_id","timestamp","score","score_iso","score_ae","score_ensemble","is_anomaly_ensemble","top_features"), names(stream_state$data))
    head(stream_state$data[, cols, drop = FALSE], 5)
  })

  update_select_input_choices <- function() {
    req(stream_state$data)
    windows <- unique(stream_state$data$window_id)
    updateSelectInput(session, "expl_window", choices = windows, selected = tail(windows, 1))
  }

  output$expl_text <- renderText({
    req(stream_state$data, input$expl_window)
    df <- stream_state$data %>% filter(window_id == input$expl_window)
    df$explanation_text[1]
  })

  output$expl_scores <- renderTable({
    req(stream_state$data, input$expl_window)
    df <- stream_state$data %>% filter(window_id == input$expl_window)
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

