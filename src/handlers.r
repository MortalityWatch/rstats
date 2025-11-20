# Request Handlers
# Functions for handling forecast and cumulative forecast requests

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
    # Median forecast: use median of historical values (seasonal or overall)
    if (s > 1) {
      # Calculate seasonal medians using row numbers for season index
      df_with_season <- df |>
        mutate(season_idx = (row_number() - 1) %% s)

      seasonal_medians <- df_with_season |>
        group_by(season_idx) |>
        summarise(
          med = median(asmr, na.rm = TRUE),
          lower = quantile(asmr, 0.025, na.rm = TRUE),
          upper = quantile(asmr, 0.975, na.rm = TRUE),
          .groups = "drop"
        )

      # Create forecast by repeating seasonal pattern
      forecast_seasons <- (nrow(df) + 0:(h - 1)) %% s
      fc_values <- seasonal_medians$med[match(forecast_seasons, seasonal_medians$season_idx)]
      fc_lower <- seasonal_medians$lower[match(forecast_seasons, seasonal_medians$season_idx)]
      fc_upper <- seasonal_medians$upper[match(forecast_seasons, seasonal_medians$season_idx)]

      # Create fitted values
      fitted_seasons <- (0:(nrow(df) - 1)) %% s
      fitted_values <- seasonal_medians$med[match(fitted_seasons, seasonal_medians$season_idx)]

      result <- tibble(
        y = c(rep(NA, leading_NA), fitted_values, fc_values),
        lower = c(rep(NA, leading_NA + nrow(df)), fc_lower),
        upper = c(rep(NA, leading_NA + nrow(df)), fc_upper)
      ) |>
        mutate_if(is.numeric, round, 1)

      # Calculate z-scores from standardized residuals for seasonal median
      baseline_residuals <- df$asmr - fitted_values
      residual_sd <- sd(baseline_residuals, na.rm = TRUE)
      baseline_med <- median(fitted_values, na.rm = TRUE)

      # Calculate z-scores for all observed data
      zscores_baseline <- round(baseline_residuals / residual_sd, 3)
      zscores <- c(rep(NA, leading_NA), zscores_baseline)

      # Post-baseline z-scores if we have more data
      if (baseline_length < length(y_full)) {
        post_baseline_data <- y_full[(baseline_length + 1):length(y_full)]
        post_baseline_data_clean <- post_baseline_data[!is.na(post_baseline_data)]
        if (length(post_baseline_data_clean) > 0) {
          post_baseline_zscores <- (post_baseline_data_clean - baseline_med) / residual_sd
          zscores <- c(zscores, round(post_baseline_zscores, 3))
        }
      }

      # Forecast period z-scores are 0
      zscores <- c(zscores, rep(0, h))

      return(list(y = result$y, lower = result$lower, upper = result$upper, zscore = zscores))
    } else {
      # Overall median
      med_val <- median(df$asmr, na.rm = TRUE)
      lower_val <- quantile(df$asmr, 0.025, na.rm = TRUE)
      upper_val <- quantile(df$asmr, 0.975, na.rm = TRUE)

      result <- tibble(
        y = c(rep(NA, leading_NA), rep(med_val, nrow(df)), rep(med_val, h)),
        lower = c(rep(NA, leading_NA + nrow(df)), rep(lower_val, h)),
        upper = c(rep(NA, leading_NA + nrow(df)), rep(upper_val, h))
      ) |>
        mutate_if(is.numeric, round, 1)

      # Calculate z-scores from standardized residuals for overall median
      baseline_residuals <- df$asmr - med_val
      residual_sd <- sd(baseline_residuals, na.rm = TRUE)

      # Calculate z-scores for all observed data
      zscores_baseline <- round(baseline_residuals / residual_sd, 3)
      zscores <- c(rep(NA, leading_NA), zscores_baseline)

      # Post-baseline z-scores if we have more data
      if (baseline_length < length(y_full)) {
        post_baseline_data <- y_full[(baseline_length + 1):length(y_full)]
        post_baseline_data_clean <- post_baseline_data[!is.na(post_baseline_data)]
        if (length(post_baseline_data_clean) > 0) {
          post_baseline_zscores <- (post_baseline_data_clean - med_val) / residual_sd
          zscores <- c(zscores, round(post_baseline_zscores, 3))
        }
      }

      # Forecast period z-scores are 0
      zscores <- c(zscores, rep(0, h))

      return(list(y = result$y, lower = result$lower, upper = result$upper, zscore = zscores))
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

  # Combine baseline and forecast, add leading NAs back
  result <- bind_rows(
    tibble(y = rep(NA, leading_NA)),
    tibble(y = bl$.mean),
    result
  ) |>
    mutate_if(is.numeric, round, 1)

  # Calculate z-scores from standardized residuals
  # Use baseline period to calculate mean and SD, then apply to ALL observed data
  baseline_residuals <- y_baseline[!is.na(y_baseline)] - bl$.mean
  residual_sd <- sd(baseline_residuals, na.rm = TRUE)
  baseline_mean <- mean(bl$.mean, na.rm = TRUE)

  # Calculate z-scores for all observed data (including post-baseline)
  # Formula: z = (observed - baseline_mean) / baseline_sd
  zscores <- rep(NA, length(result$y))

  # Baseline period z-scores (from residuals)
  zscores[(leading_NA + 1):(leading_NA + baseline_length)] <-
    round(baseline_residuals / residual_sd, 3)

  # Post-baseline observed data z-scores
  if (baseline_length < length(y_full)) {
    post_baseline_data <- y_full[(baseline_length + 1):length(y_full)]
    post_baseline_data_clean <- post_baseline_data[!is.na(post_baseline_data)]
    if (length(post_baseline_data_clean) > 0) {
      post_baseline_zscores <- (post_baseline_data_clean - baseline_mean) / residual_sd
      zscores[(leading_NA + baseline_length + 1):(leading_NA + baseline_length + length(post_baseline_data_clean))] <-
        round(post_baseline_zscores, 3)
    }
  }

  # Forecast period z-scores are 0 (by definition)
  zscores[(leading_NA + length(y_full) + 1):length(zscores)] <- rep(0, h)

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
  baseline_residuals <- df_train$asmr - bl$.mean
  residual_sd <- sd(baseline_residuals, na.rm = TRUE)
  baseline_mean <- mean(bl$.mean, na.rm = TRUE)

  # Z-scores for baseline period
  zscores_baseline <- round(baseline_residuals / residual_sd, 3)
  zscores <- zscores_baseline

  # Post-baseline z-scores if we have more data
  if (baseline_length < length(y_full)) {
    post_baseline_data <- y_full[(baseline_length + 1):length(y_full)]
    post_baseline_data_clean <- post_baseline_data[!is.na(post_baseline_data)]
    if (length(post_baseline_data_clean) > 0) {
      post_baseline_zscores <- (post_baseline_data_clean - baseline_mean) / residual_sd
      zscores <- c(zscores, round(post_baseline_zscores, 3))
    }
  }

  # For forecast period, z-scores are 0 (no deviation from model)
  zscores_fc <- rep(0, h)

  # Convert cumulative forecasts back to incremental values
  list(
    y = c(bl$.mean, uncumulate(result$asmr)),
    lower = c(rep(NA, nrow(bl)), uncumulate(result$lower)),
    upper = c(rep(NA, nrow(bl)), uncumulate(result$upper)),
    zscore = c(zscores, zscores_fc)
  )
}
