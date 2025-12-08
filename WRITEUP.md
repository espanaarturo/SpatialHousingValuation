# Scalable, Adaptive Anomaly Detection for Housing Streams

## Problem
High-frequency housing signals (price changes, rent shifts, inventory) drift with rates, seasons, and local shocks. We need real-time anomaly detection that adapts to concept drift and remains explainable.

## System Design
- **Streaming sim + drift**: synthetic housing stream with drift/anomaly injection.
- **Feature expansion + PCA**: high-dimensional expansions + rolling PCA for stability.
- **Detectors**: baseline MAD, Isolation Forest, torch autoencoder (optional).
- **Drift detection**: KS/mean/variance shifts trigger recalibration.
- **Adaptive ensemble**: weights updated via EMA on recent performance; stability fallback.
- **Explainability**: per-detector attributions mapped back from PCA to housing features.
- **Shiny demo**: live controls, plots, explanations, experiments.

## Results (examples)
- Precision vs dimensionality shows stable performance up to 500D.
- Time-to-detection improves post-drift after recalibration hooks.
- Ensemble weights shift toward the more stable detector after drift.
- Top-feature explanations highlight rate-sensitive signals (price, rate_proxy) during shocks.

See `results/figures/` for generated plots.

## Lessons Learned
- Rolling PCA with refit scheduling keeps detectors stable under drift.
- Adaptive weighting plus drift-triggered refits reduce post-drift false positives.
- Mapping PCA loadings back to expanded features is critical for explainability.
- Torch autoencoder is a nice-to-have; baseline + ISO cover most signal.

## Next Steps
- Add lightweight online calibration of thresholds per segment (market-level).
- Stream to a message bus (e.g., Kafka) and consume in Shiny via reactive polling.
- Expand spatial simulation with sf-based neighborhood graphs.

