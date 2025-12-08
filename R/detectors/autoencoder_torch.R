#' Torch autoencoder detector
#'
#' Note: requires torch installed; run torch::install_torch() once if needed.

ae_build_model <- function(input_dim, hidden = c(128, 64), latent = 16) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("Package 'torch' is required. Install with install.packages('torch') and torch::install_torch().")
  }
  encoder_sizes <- c(input_dim, hidden, latent)
  decoder_sizes <- c(latent, rev(hidden), input_dim)

  model <- torch::nn_module(
    initialize = function() {
      self$enc_layers <- torch::nn_module_list()
      for (i in seq_len(length(encoder_sizes) - 1)) {
        self$enc_layers$append(torch::nn_linear(encoder_sizes[i], encoder_sizes[i + 1]))
      }
      self$dec_layers <- torch::nn_module_list()
      for (i in seq_len(length(decoder_sizes) - 1)) {
        self$dec_layers$append(torch::nn_linear(decoder_sizes[i], decoder_sizes[i + 1]))
      }
      self$activation <- torch::nn_relu()
    },
    encode = function(x) {
      for (layer in self$enc_layers) {
        x <- self$activation(layer(x))
      }
      x
    },
    decode = function(z) {
      for (i in seq_along(self$dec_layers)) {
        layer <- self$dec_layers[[i]]
        if (i < length(self$dec_layers)) {
          z <- self$activation(layer(z))
        } else {
          z <- layer(z)
        }
      }
      z
    },
    forward = function(x) {
      self$decode(self$encode(x))
    }
  )
  model
}

ae_train <- function(data, config) {
  device <- torch::torch_device(if (torch::cuda_is_available()) "cuda" else "cpu")
  x <- torch::torch_tensor(as.matrix(data), dtype = torch::torch_float(), device = device)
  ds <- torch::tensor_dataset(x)
  dl <- torch::dataloader(ds, batch_size = config$batch_size %||% 64, shuffle = TRUE)

  model <- ae_build_model(
    input_dim = ncol(data),
    hidden = config$hidden %||% c(128, 64),
    latent = config$latent %||% 16
  )
  model$to(device = device)

  optimizer <- torch::optim_adam(model$parameters, lr = config$lr %||% 1e-3)
  epochs <- config$epochs %||% 10

  for (epoch in seq_len(epochs)) {
    coro::loop(for (batch in dl) {
      optimizer$zero_grad()
      output <- model(batch[[1]])
      loss <- torch::nnf_mse_loss(output, batch[[1]])
      loss$backward()
      optimizer$step()
    })
  }

  model$eval()
  preds <- model(x)$to(device = "cpu")
  recon_err <- torch::nnf_mse_loss(preds, torch::torch_tensor(as.matrix(data)), reduction = "none")
  err_vec <- as.numeric(torch::torch_mean(recon_err, dim = 2))
  thr <- stats::quantile(err_vec, probs = config$threshold_quantile %||% 0.99, na.rm = TRUE)

  list(
    model = model,
    device = device,
    center = colMeans(data),
    scale = apply(data, 2, sd),
    threshold = thr
  )
}

ae_score <- function(data, state) {
  x <- scale(data, center = state$center, scale = state$scale + 1e-6)
  tens <- torch::torch_tensor(as.matrix(x), dtype = torch::torch_float(), device = state$device)
  preds <- state$model(tens)$to(device = "cpu")
  recon_err <- torch::nnf_mse_loss(preds, torch::torch_tensor(as.matrix(x)), reduction = "none")
  as.numeric(torch::torch_mean(recon_err, dim = 2))
}

ae_fit_initial <- function(data, config) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    warning("torch not installed; skipping autoencoder fit")
    return(NULL)
  }
  state_raw <- ae_train(data, config)
  new_detector_state(
    name = "autoencoder_torch",
    meta = state_raw
  )
}

ae_partial_fit <- function(data, state, config, force_refit = FALSE) {
  if (is.null(state)) return(state)
  if (!force_refit) return(state)
  ae_fit_initial(data, config)
}

ae_detect <- function(data, state, config, force_refit = FALSE) {
  if (is.null(state)) {
    return(list(results = tibble::tibble(score_ae = NA_real_, is_anomaly_ae = FALSE), state = state))
  }
  if (force_refit) {
    state <- ae_partial_fit(data, state, config, force_refit = TRUE)
  }
  scores <- ae_score(data, state$meta)
  thr <- state$meta$threshold
  is_anom <- scores > thr
  list(
    results = tibble::tibble(
      score_ae = scores,
      is_anomaly_ae = is_anom
    ),
    state = state
  )
}

