# Request Handlers
# Functions for handling forecast and cumulative forecast requests

# Load custom MEDIAN model
# Try multiple paths to support different execution contexts (main app, tests, etc.)
median_model_paths <- c(
  "src/median_model.r",          # Running from project root
  "median_model.r",              # Running from src/
  "../src/median_model.r"        # Running from tests/
)

median_model_loaded <- FALSE
for (path in median_model_paths) {
  if (file.exists(path)) {
    source(path)
    median_model_loaded <- TRUE
    break
  }
}

if (!median_model_loaded) {
  stop("Could not find median_model.r. Tried paths: ", paste(median_model_paths, collapse = ", "))
}

#' Handle standard forecast request
#'
#' Performs time series forecasting using various methods (naive, mean, linear regression, exponential smoothing)
#'
#' @param y Numeric vector of observed values (full dataset)
#' @param h Integer horizon for forecasting
#' @param m Character method: "naive", "mean", "lin_reg", "exp"
#' @param s Integer seasonality type: 1=year, 2=quarter, 3=month, 4=week
#' @param t Boolean whether to include trend
#' @param baseline_length Integer number of data points in baseline period (default: use all data)
#' @return List with y (fitted + forecast), lower, and upper bounds
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

  # Calculate z-scores from standardized residuals
  # Z-score formula: z = (observed - fitted) / sd(baseline_residuals)
  # - Baseline period: Use residuals from model fit
  # - Post-baseline period: Generate fitted values using baseline model, then calculate residuals
  # - Forecast period: z = 0 (by definition, no deviation)

  baseline_residuals <- y_baseline[!is.na(y_baseline)] - bl$.mean
  residual_sd <- sd(baseline_residuals, na.rm = TRUE)

  zscores <- rep(NA, length(result$y))

  # Baseline period z-scores (from model residuals)
  n_baseline_values <- length(baseline_residuals)
  zscores[(leading_NA + 1):(leading_NA + n_baseline_values)] <-
    round(baseline_residuals / residual_sd, 3)

  # Post-baseline observed data z-scores
  # Generate fitted values for post-baseline period using the baseline model
  n_post_baseline_values <- 0
  if (baseline_length < length(y_full)) {
    post_baseline_data <- y_full[(baseline_length + 1):length(y_full)]
    n_post_baseline <- length(post_baseline_data)

    # Create a tsibble for post-baseline period with appropriate time index
    post_baseline_years <- seq.int(baseline_length + 1, length(y_full))
    if (s == 2) {
      post_baseline_index <- make_yearquarter(2000, 1) + (baseline_length:length(y_full) - 1)
    } else if (s == 3) {
      post_baseline_index <- make_yearmonth(2000, 1) + (baseline_length:length(y_full) - 1)
    } else if (s == 4) {
      post_baseline_index <- make_yearweek(2000, 1) + (baseline_length:length(y_full) - 1)
    } else {
      post_baseline_index <- post_baseline_years
    }

    # Create new_data for post-baseline period
    df_post <- tibble(year = post_baseline_index, asmr = post_baseline_data) |>
      as_tsibble(index = year)

    # Generate fitted values for post-baseline period using baseline model
    df_post_clean <- df_post |> filter(!is.na(asmr))
    post_baseline_fitted <- mdl |>
      augment(new_data = df_post_clean) |>
      pull(.fitted)

    # Calculate z-scores for post-baseline (only for non-NA values)
    post_baseline_clean <- post_baseline_data[!is.na(post_baseline_data)]
    n_post_baseline_values <- length(post_baseline_clean)

    if (n_post_baseline_values > 0) {
      post_baseline_residuals <- post_baseline_clean - post_baseline_fitted
      post_baseline_zscores <- post_baseline_residuals / residual_sd
      zscores[(leading_NA + n_baseline_values + 1):(leading_NA + n_baseline_values + n_post_baseline_values)] <-
        round(post_baseline_zscores, 3)
    }
  }

  # Forecast period z-scores are 0 (by definition)
  forecast_start <- leading_NA + n_baseline_values + n_post_baseline_values + 1
  zscores[forecast_start:length(zscores)] <- rep(0, h)

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
#' @return List with y (fitted + forecast), lower, and upper bounds
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

  # Calculate z-scores from standardized residuals
  # Z-score formula: z = (observed - fitted) / sd(baseline_residuals)
  baseline_residuals <- df_train$asmr - bl$.mean
  residual_sd <- sd(baseline_residuals, na.rm = TRUE)

  # Z-scores for baseline period
  zscores_baseline <- round(baseline_residuals / residual_sd, 3)
  zscores <- zscores_baseline

  # Post-baseline z-scores if we have more data
  # Generate fitted values for post-baseline period using the baseline model
  if (baseline_length < length(y_full)) {
    post_baseline_data <- y_full[(baseline_length + 1):length(y_full)]
    post_baseline_data_clean <- post_baseline_data[!is.na(post_baseline_data)]

    if (length(post_baseline_data_clean) > 0) {
      # For cumulative forecasts, use the same linear model to predict post-baseline values
      # Generate forecast for post-baseline period
      n_post <- length(post_baseline_data_clean)
      fc_post <- mdl |> forecast(h = n_post)
      post_baseline_fitted <- as_tibble(fc_post) |> pull(.mean)

      # Calculate residuals and z-scores
      post_baseline_residuals <- post_baseline_data_clean - post_baseline_fitted
      post_baseline_zscores <- post_baseline_residuals / residual_sd
      zscores <- c(zscores, round(post_baseline_zscores, 3))
    }
  }

  # For forecast period, z-scores are 0 (no deviation from model)
  zscores_fc <- rep(0, h)

  # Get post-baseline observed values
  post_baseline_observed <- if (baseline_length < length(y_full)) {
    y_full[(baseline_length + 1):length(y_full)]
  } else {
    numeric(0)
  }

  # Convert cumulative forecasts back to incremental values
  list(
    y = c(bl$.mean, post_baseline_observed, uncumulate(result$asmr)),
    lower = c(rep(NA, nrow(bl) + length(post_baseline_observed)), uncumulate(result$lower)),
    upper = c(rep(NA, nrow(bl) + length(post_baseline_observed)), uncumulate(result$upper)),
    zscore = c(zscores, zscores_fc)
  )
}
