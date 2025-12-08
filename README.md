# Scalable, Adaptive Anomaly Detection for Housing Streams

An R implementation for streaming, drift-aware anomaly detection on housing/real-estate signals, including windowed processing, adaptive ensemble scoring, and explainability mapped back to original features.

## What this project does
- Windowed streaming with drift/anomaly injection
- High-dimensional feature expansion
- Rolling PCA with refit schedule
- Drift detection (KS/mean/variance) and drift-triggered recalibration
- Detectors: baseline MAD, Isolation Forest, optional torch autoencoder
- Adaptive ensemble with weight trajectories
- Explainability with PCA back-mapping to original/expanded housing features

## Quickstart
```r
install.packages("renv")
renv::restore()
source("scripts/run_stream.R")
run_stream("configs/demo.yaml")
```

## Outputs
Default: `results/baseline_anomalies.csv` (or `logging$output_name`), containing:
- Per-detector scores/flags (baseline, isolation forest, optional torch AE)
- Ensemble score/flag and weights
- Drift metrics (KS/mean/variance) and drift flag
- `top_features` and `explanation_text`
- `is_anomaly_true` when using injected truth

## Experiment sweeps
```r
source("scripts/run_experiment.R")
run_experiment("configs/experiment_small.yaml")
```

## Shiny demo
```r
shiny::runApp("apps")
```
Tabs:
- Live Stream: adjust drift/anomaly/dim/window; toggle detectors/ensemble; step or auto-play; plots for market + anomalies, drift metric, ensemble weights, thresholds.
- Explanations: select a window to view top_features, explanation_text, and per-detector scores.
- Experiment Viewer: run preset sweeps and view summary table/plot.

## Project structure
- `configs/` (e.g., `demo.yaml`, `experiment_small.yaml`)
- `R/streaming/` simulators + windowing
- `R/features/` expansion + PCA
- `R/drift/` drift detection
- `R/detectors/` detectors
- `R/ensemble/` adaptive ensemble
- `R/explain/` explanation helpers
- `scripts/` run_stream, run_experiment, make_figures
- `apps/` Shiny app
- `tests/` testthat suite
- `results/` outputs and figures

## Config overview
- `configs/demo.yaml`: main demo run.
- `configs/experiment_small.yaml`: quicker sweep for CI/smoke tests.

## Torch autoencoder (optional)
- Install if desired:
  ```r
  install.packages("torch")
  torch::install_torch()
  ```
- Enable via `detectors$autoencoder_torch$enabled: true` in config.
- CI does not require torch; baseline + ISO always run.

Autoplay note: the app generates a single run per settings and plays back window-by-window; Play/Pause/Reset controls advance the cached run without rerunning the full pipeline. Export saves the current run to `results/shiny_stream.csv`.

## CI
- GitHub Actions: renv::restore(), testthat, and a small smoke experiment (`experiment_small.yaml`, AE off).

## Figures
```r
source("scripts/make_figures.R")
```
Saves plots to `results/figures/` (precision vs dimensionality, time-to-detection proxy, ensemble weights, top explanation features).

## Write-up
See `WRITEUP.md` for a short blog-style overview (problem, design, results, lessons).