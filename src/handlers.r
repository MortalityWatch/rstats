# Request Handlers
# Functions for handling forecast and cumulative forecast requests

# Load custom MEDIAN model
# Use dirname to find path relative to this script
handlers_dir <- if (exists("ofile") && !is.null(sys.frames()[[1]]$ofile)) {
  dirname(sys.frames()[[1]]$ofile)
} else {
  # Fallback: try multiple paths for different execution contexts
  paths <- c("src/median_model.r", "median_model.r", "../src/median_model.r")
  found_path <- NULL
  for (p in paths) {
    if (file.exists(p)) {
      found_path <- dirname(p)
      break
    }
  }
  if (is.null(found_path)) {
    stop("Could not find median_model.r in any expected location")
  }
  found_path
}

median_model_path <- file.path(handlers_dir, "median_model.r")
if (!file.exists(median_model_path)) {
  # Final fallback: try relative paths
  for (path in c("src/median_model.r", "median_model.r", "../src/median_model.r")) {
    if (file.exists(path)) {
      median_model_path <- path
      break
    }
  }
}

if (!file.exists(median_model_path)) {
  stop("Could not find median_model.r")
}

source(median_model_path)

#' Calculate z-scores for baseline and post-baseline periods
#'
#' @param y_full Full dataset including both baseline and post-baseline
#' @param baseline_length Length of baseline period
#' @param baseline_residuals Residuals from baseline model fit
#' @param mdl Fitted model object
#' @param result_length Total length of result vector (includes forecast)
#' @param leading_NA Number of leading NA values
#' @param h Forecast horizon
#' @return Vector of z-scores
calculate_zscores <- function(y_full, baseline_length, baseline_residuals,
                               mdl, result_length, leading_NA, h) {
  residual_sd <- sd(baseline_residuals, na.rm = TRUE)
  zscores <- rep(NA, result_length)

  # Baseline period z-scores (from model residuals)
  n_baseline_values <- length(baseline_residuals)
  zscores[(leading_NA + 1):(leading_NA + n_baseline_values)] <-
    round(baseline_residuals / residual_sd, 3)

  # Post-baseline observed data z-scores
  n_post_baseline_total <- 0
  if (baseline_length < length(y_full)) {
    post_baseline_data <- y_full[(baseline_length + 1):length(y_full)]
    n_post_baseline_total <- length(post_baseline_data)

    # Track which positions have non-NA values for correct z-score alignment
    post_baseline_non_na_idx <- which(!is.na(post_baseline_data))
    post_baseline_clean <- post_baseline_data[post_baseline_non_na_idx]

    if (length(post_baseline_clean) > 0) {
      # Generate fitted values for post-baseline period using baseline model
      fc_post <- mdl |> forecast(h = length(post_baseline_clean))
      post_baseline_fitted <- as_tibble(fc_post) |> pull(.mean)

      # Calculate z-scores for post-baseline
      post_baseline_residuals <- post_baseline_clean - post_baseline_fitted
      post_baseline_zscores <- post_baseline_residuals / residual_sd

      # Assign z-scores to correct positions (vectorized)
      post_baseline_start <- leading_NA + n_baseline_values
      zscore_indices <- post_baseline_start + post_baseline_non_na_idx
      zscores[zscore_indices] <- round(post_baseline_zscores, 3)
    }
  }

  # Forecast period z-scores are 0 (by definition)
  forecast_start <- leading_NA + n_baseline_values + n_post_baseline_total + 1
  zscores[forecast_start:length(zscores)] <- rep(0, h)

  zscores
}

#' Handle standard forecast request
#'
#' Performs time series forecasting using various methods (naive, mean, linear regression, exponential smoothing)
#'
#' @param y Numeric vector of observed values (full dataset)
#' @param h Integer horizon for forecasting
#' @param m Character method: "naive", "mean", "lin_reg", "exp", "median"
#' @param s Integer seasonality type: 1=year, 2=quarter, 3=month, 4=week
#' @param t Boolean whether to include trend
#' @param baseline_length Integer number of data points in baseline period (default: use all data)
#'
#' @details
#' Z-score Calculation:
#' When baseline_length is specified, z-scores measure how much each observation
#' deviates from what the baseline model predicts:
#' - Baseline period: z = (observed - baseline_fitted) / baseline_sd
#' - Post-baseline period: z = (observed - post_baseline_fitted) / baseline_sd
#'   where post_baseline_fitted is the baseline model's prediction for that timepoint
#' - Forecast period: z = 0 (by definition, no observed deviation)
#'
#' This means post-baseline z-scores measure deviation from the baseline model's
#' prediction, NOT from the baseline period's mean. A high z-score in the post-baseline
#' period indicates the observed value differs significantly from what the baseline
#' model would have predicted.
#'
#' @return List with y (fitted + forecast), lower, and upper bounds, and zscore
handleForecast <- function(y, h, m, s, t, baseline_length = NULL) {
  # If baseline_length is specified, split the data
  # Use first baseline_length points for fitting, but calculate z-scores for all observed data
  y_full <- y
  if (!is.null(baseline_length) && baseline_length > 0 && baseline_length < length(y)) {
    y_baseline <- y[1:baseline_length]
  } else {
    y_baseline <- y
    baseline_length <- length(y)
  }

  df <- tibble(year = seq.int(1, length(y_baseline)), asmr = y_baseline)

  # Convert to appropriate time series index based on seasonality
  if (s == 2) {
    df$year <- make_yearquarter(2000, 1) + 0:(length(y_baseline) - 1)
  } else if (s == 3) {
    df$year <- make_yearmonth(2000, 1) + 0:(length(y_baseline) - 1)
  } else if (s == 4) {
    df$year <- make_yearweek(2000, 1) + 0:(length(y_baseline) - 1)
  }

  # Count leading NAs
  leading_NA <- nrow(df |> filter(is.na(asmr)))

  # Convert to tsibble and remove NAs
  df <- df |>
    as_tsibble(index = year) |>
    filter(!is.na(asmr))

  # Fit model based on method
  if (m == "naive") {
    mdl <- df |> model(NAIVE(asmr))
  } else if (m == "mean") {
    if (s > 1) {
      mdl <- df |> model(TSLM(asmr ~ season()))
    } else {
      mdl <- df |> model(TSLM(asmr))
    }
  } else if (m == "median") {
    if (s > 1) {
      mdl <- df |> model(MEDIAN(asmr ~ season()))
    } else {
      mdl <- df |> model(MEDIAN(asmr))
    }
  } else if (m == "lin_reg") {
    if (t) {
      if (s > 1) {
        mdl <- df |> model(TSLM(asmr ~ trend() + season()))
      } else {
        mdl <- df |> model(TSLM(asmr ~ trend()))
      }
    } else {
      if (s > 1) {
        mdl <- df |> model(TSLM(asmr ~ season()))
      } else {
        mdl <- df |> model(TSLM(asmr))
      }
    }
  } else if (m == "exp") {
    if (s > 1) {
      mdl <- df |> model(ETS(asmr ~ error() + trend() + season()))
    } else {
      mdl <- df |> model(ETS(asmr ~ error("A") + trend("Ad")))
    }
  } else {
    stop(paste("Unknown method:", m))
  }

  # Generate forecast
  fc <- mdl |> forecast(h = h)

  # Get baseline (fitted values)
  bl <- mdl |>
    augment() |>
    rename(.mean = .fitted)

  # Extract forecast with confidence intervals
  result <- fabletools::hilo(fc, 95) |>
    unpack_hilo(cols = `95%`) |>
    as_tibble() |>
    select(.mean, "95%_lower", "95%_upper") |>
    setNames(c("y", "lower", "upper"))

  # Combine baseline fitted values, post-baseline observed values, and forecast
  # For post-baseline period: use actual observed values (not fitted)
  post_baseline_observed <- if (baseline_length < length(y_full)) {
    y_full[(baseline_length + 1):length(y_full)]
  } else {
    numeric(0)
  }

  result <- bind_rows(
    tibble(y = rep(NA, leading_NA)),
    tibble(y = bl$.mean),
    tibble(y = post_baseline_observed),
    result
  ) |>
    mutate_if(is.numeric, round, 1)

  # Calculate z-scores using helper function
  baseline_residuals <- y_baseline[!is.na(y_baseline)] - bl$.mean
  zscores <- calculate_zscores(
    y_full = y_full,
    baseline_length = baseline_length,
    baseline_residuals = baseline_residuals,
    mdl = mdl,
    result_length = length(result$y),
    leading_NA = leading_NA,
    h = h
  )

  list(y = result$y, lower = result$lower, upper = result$upper, zscore = zscores)
}

#' Cumulative forecast for single horizon
#'
#' @param df_train Training data
#' @param df_test Test data
#' @param mdl Fitted model
#' @return Tibble with cumulative mean, lower, and upper bounds
cumForecastN <- function(df_train, df_test, mdl) {
  oo <- lm_predict_tslm(model = mdl, newdata = df_test, FALSE)

  fc_sum_mean <- sum(oo$fit)
  fc_sum_variance <- sum(oo$var.fit)

  n <- ncol(lengths(oo$var.fit))
  res <- agg_pred(rep.int(x = 1, length(oo$fit)), oo, alpha = .95)

  tibble(
    asmr = round(fc_sum_mean, 1),
    lower = round(res$PI[1], 1),
    upper = round(res$PI[2], 1)
  )
}

#' Handle cumulative forecast request
#'
#' Performs cumulative forecasting for annual data with trend or mean baseline
#'
#' @param y Numeric vector of cumulative observed values (full dataset)
#' @param h Integer horizon for forecasting
#' @param t Boolean whether to include trend
#' @param baseline_length Integer number of data points in baseline period (default: use all data)
#'
#' @details
#' Z-score calculation follows the same semantics as handleForecast():
#' measures deviation from the baseline model's prediction using baseline period's
#' standard deviation. See handleForecast() documentation for details.
#'
#' @return List with y (fitted + forecast), lower, and upper bounds, and zscore
handleCumulativeForecast <- function(y, h, t, baseline_length = NULL) {
  y_full <- y

  # If baseline_length is specified, use only that portion for fitting
  if (!is.null(baseline_length) && baseline_length > 0 && baseline_length < length(y)) {
    y_baseline <- y[1:baseline_length]
  } else {
    y_baseline <- y
    baseline_length <- length(y)
  }

  n <- length(y_baseline)

  df <- tibble(year = seq.int(1, n), asmr = y_baseline) |>
    as_tsibble(index = year)

  # Use ALL input data for training
  df_train <- df

  # Fit model with or without trend
  if (t) {
    mdl <- df_train |> model(lm = TSLM(asmr ~ trend()))
  } else {
    mdl <- df_train |> model(lm = TSLM(asmr))
  }

  # Get baseline (fitted values)
  bl <- mdl |>
    augment() |>
    rename(.mean = .fitted)

  # Generate cumulative forecasts for each horizon
  result <- tibble()
  for (h_ in 1:h) {
    # Create future time periods using fabletools::new_data
    # Use placeholder value 0 for asmr (not used in predictions, just needed for model.matrix)
    df_test <- new_data(df_train, n = h_) |>
      mutate(asmr = 0)
    result <- rbind(result, cumForecastN(df_train, df_test, mdl))
  }

  # Get post-baseline observed values
  post_baseline_observed <- if (baseline_length < length(y_full)) {
    y_full[(baseline_length + 1):length(y_full)]
  } else {
    numeric(0)
  }

  # Calculate total result length
  result_length <- nrow(bl) + length(post_baseline_observed) + h

  # Calculate z-scores using helper function
  baseline_residuals <- df_train$asmr - bl$.mean
  zscores <- calculate_zscores(
    y_full = y_full,
    baseline_length = baseline_length,
    baseline_residuals = baseline_residuals,
    mdl = mdl,
    result_length = result_length,
    leading_NA = 0,  # No leading NAs in cumulative forecast
    h = h
  )

  # Convert cumulative forecasts back to incremental values
  list(
    y = c(bl$.mean, post_baseline_observed, uncumulate(result$asmr)),
    lower = c(rep(NA, nrow(bl) + length(post_baseline_observed)), uncumulate(result$lower)),
    upper = c(rep(NA, nrow(bl) + length(post_baseline_observed)), uncumulate(result$upper)),
    zscore = zscores
  )
}
