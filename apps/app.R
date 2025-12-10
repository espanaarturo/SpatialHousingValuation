library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(purrr)

options(shiny.sanitize.errors = FALSE)

wd <- normalizePath(getwd())
proj_dir <- if (basename(wd) == "apps") normalizePath(file.path(wd, "..")) else wd

source(file.path(proj_dir, "scripts/run_stream.R"))
source(file.path(proj_dir, "scripts/run_experiment.R"))

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
        fluidPage(
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
          card(card_header("Preview"), tableOutput("table_head")),
          card(card_header("Live stream (debug)"),
               plotOutput("live_stream_plot", height = 200),
               tableOutput("live_stream_table"),
               verbatimTextOutput("live_stream_debug"))
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
    cfg <- yaml::read_yaml(file.path(proj_dir, "configs/demo.yaml"))
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

  observeEvent(TRUE, {
    if (is.null(run_cache())) generate_run()
  }, once = TRUE, ignoreInit = FALSE)

  output$live_stream_plot <- renderPlot({
    validate(
      need(!is.null(run_cache()), "Click Reset to generate a run"),
      need(nrow(current_data()) > 0, "Waiting for data")
    )
    rc <- run_cache()
    i <- current_idx()
    df <- rc$data %>% filter(as.numeric(window_id) <= i)
    ggplot(df, aes(as.numeric(window_id), score_ensemble)) +
      geom_line(color = "#16a085") +
      geom_point(size = 1.2, color = "#27ae60") +
      labs(title = "Ensemble score over windows", x = "Window", y = "Ensemble score")
  })

  output$live_stream_table <- renderTable({
    validate(
      need(!is.null(run_cache()), "Click Reset to generate a run"),
      need(nrow(current_data()) > 0, "Waiting for data")
    )
    rc <- run_cache()
    i <- current_idx()
    df <- rc$data %>% filter(as.numeric(window_id) <= i)
    head(df[, intersect(c("window_id","timestamp","score_ensemble","is_anomaly_ensemble","ks_metric","top_features"), names(df)), drop = FALSE], 10)
  })

  output$live_stream_debug <- renderPrint({
    list(
      cache_ready = !is.null(run_cache()),
      current_idx = current_idx(),
      max_windows = if (!is.null(run_cache())) run_cache()$n_windows else NA,
      drift_type = input$drift_type,
      anomaly_rate = input$anom_rate,
      target_dim = input$dim
    )
  })

  generate_run <- function() {
    cfg <- current_cfg()
    res <- run_stream_with_cfg(cfg, write_out = FALSE, quiet = TRUE, root_dir = proj_dir)
    run_cache(list(
      data = res$out,
      cfg = cfg,
      n_windows = max(as.numeric(res$out$window_id))
    ))
    current_idx(1)
    auto_running(FALSE)
    update_select_input_choices()
  }

  observeEvent(list(input$drift_type, input$drift_mag, input$anom_rate, input$dim, input$window_type, input$detectors, input$ensemble_on), {
    generate_run()
  }, ignoreInit = FALSE)

  observeEvent(TRUE, {
    if (is.null(run_cache())) generate_run()
  }, once = TRUE)

  observeEvent(input$reset_btn, {
    message("reset clicked")
    generate_run()
    current_idx(1)
  })

  observeEvent(input$step_btn, {
    message("step clicked")
    req(run_cache())
    max_w <- run_cache()$n_windows %||% length(unique(run_cache()$data$window_id))
    current_idx(min(current_idx() + 1, max_w))
  })

  observeEvent(input$auto_btn, {
    message("play clicked")
    auto_running(TRUE)
  })
  observeEvent(input$stop_btn, {
    message("pause clicked")
    auto_running(FALSE)
  })

  observe({
    req(run_cache())
    if (!isTRUE(auto_running())) return()
    if (current_idx() >= run_cache()$n_windows) {
      auto_running(FALSE)
      return()
    }
    invalidateLater(800, session)
    max_w <- run_cache()$n_windows %||% length(unique(run_cache()$data$window_id))
    current_idx(min(current_idx() + 1, max_w))
  })

  observeEvent(input$export_btn, {
    message("export clicked")
    req(run_cache())
    df <- run_cache()$data
    if (!dir.exists("results")) dir.create("results", recursive = TRUE)
    readr::write_csv(df, "results/shiny_stream.csv")
    showNotification("Exported to results/shiny_stream.csv", type = "message")
  })

  current_data <- reactive({
    req(run_cache())
    i <- current_idx()
    df <- run_cache()$data
    df %>% filter(as.numeric(window_id) <= i)
  })

  output$plot_market <- renderPlot({
    rc <- run_cache()
    i  <- current_idx()

    validate(
      need(!is.null(rc), "Cache not ready. Click Reset to generate a run."),
      need(!is.null(rc$data), "Cache data missing. Click Reset."),
      need(nrow(rc$data) > 0, "No stream data available yet.")
    )

    required_cols <- c("timestamp", "window_id")
    alt_price_cols <- c("price","idxprice","price_idx","home_value","median_price")
    alt_anom_cols <- c("is_anomaly_ensemble", "is_anomaly", "is_anomaly_true")

    price_col <- alt_price_cols[alt_price_cols %in% names(rc$data)][1]
    anom_col  <- alt_anom_cols[alt_anom_cols %in% names(rc$data)][1]

    validate(
      need(all(required_cols %in% names(rc$data)),
           paste("Missing fields for Market & Anomalies plot:",
                 paste(setdiff(required_cols, names(rc$data)), collapse = ", "))),
      need(!is.null(price_col),
           paste("Missing price-like column. Available cols:", paste(names(rc$data), collapse = ", "))),
      need(!is.null(anom_col),
           paste("Missing anomaly flag column. Available cols:", paste(names(rc$data), collapse = ", ")))
    )

    df <- rc$data |>
      dplyr::mutate(window_id = as.numeric(window_id)) |>
      dplyr::filter(window_id <= i)

    tryCatch({
      ggplot(df, aes(x = timestamp, y = .data[[price_col]], color = .data[[anom_col]])) +
        geom_line() +
        geom_point(alpha = 0.7) +
        labs(title = "Market & Anomalies",
             subtitle = paste("Up to window", i),
             color = "Anomaly") +
        theme_minimal(base_size = 14)
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0, y = 0,
                 label = paste("Market plot failed:", conditionMessage(e))) +
        theme_void()
    })
  })

  output$plot_drift <- renderPlot({
    validate(
      need(!is.null(run_cache()), "Cache not ready. Click Reset to generate a run."),
      need(nrow(current_data()) > 0, "Waiting for data")
    )
    i <- current_idx()
    df <- run_cache()$data %>% filter(as.numeric(window_id) <= i)
    ggplot(df, aes(as.numeric(window_id), ks_metric, color = drift_detected)) +
      geom_line() +
      geom_point() +
      labs(title = "Drift metric (KS)", x = "Window", y = "KS") +
      theme_minimal() +
      scale_color_manual(values = c("TRUE" = "#e67e22", "FALSE" = "#2980b9"), guide = guide_legend(title = "Drift"))
  })

  output$plot_weights <- renderPlot({
    validate(
      need(!is.null(run_cache()), "Cache not ready. Click Reset to generate a run."),
      need(nrow(current_data()) > 0, "Waiting for data")
    )
    i <- current_idx()
    df <- run_cache()$data %>% filter(as.numeric(window_id) <= i)
    plt <- ggplot(df, aes(as.numeric(window_id)))
    plt <- plt + geom_line(aes(y = weight_baseline, color = "baseline"))
    if ("weight_iso" %in% names(df)) plt <- plt + geom_line(aes(y = weight_iso, color = "iso"))
    if ("weight_ae" %in% names(df) && any(!is.na(df$weight_ae))) plt <- plt + geom_line(aes(y = weight_ae, color = "ae"))
    plt + labs(title = "Ensemble weights", x = "Window", y = "Weight") +
      scale_color_manual(values = c(baseline = "#2c3e50", iso = "#2980b9", ae = "#8e44ad")) +
      theme_minimal()
  })

  output$plot_thresholds <- renderPlot({
    validate(
      need(!is.null(run_cache()), "Cache not ready. Click Reset to generate a run."),
      need(nrow(current_data()) > 0, "Waiting for data")
    )
    i <- current_idx()
    df <- run_cache()$data %>% filter(as.numeric(window_id) <= i)
    ggplot(df, aes(as.numeric(window_id))) +
      geom_line(aes(y = score_ensemble, color = "score")) +
      geom_hline(yintercept = 1.0, linetype = "dashed", color = "#7f8c8d") +
      labs(title = "Ensemble score vs threshold", x = "Window", y = "Score") +
      scale_color_manual(values = c(score = "#16a085"), guide = "none") +
      theme_minimal()
  })

  output$table_head <- renderTable({
    req(run_cache())
    req(current_data())
    i <- current_idx()
    df <- run_cache()$data %>% filter(as.numeric(window_id) <= i)
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
    exp_results(run_experiment(file.path(proj_dir, "configs/demo.yaml"), drift_types = c("abrupt_rate_hike","gradual_seasonal"), anomaly_rates = c(0.05), target_dims = c(100,300), root_dir = proj_dir))
  }, ignoreInit = TRUE)

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

