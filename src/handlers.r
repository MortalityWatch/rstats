# Request Handlers
# Functions for handling forecast and cumulative forecast requests

#' Handle standard forecast request
#'
#' Performs time series forecasting using various methods (naive, mean, linear regression, exponential smoothing)
#'
#' @param y Numeric vector of observed values
#' @param h Integer horizon for forecasting
#' @param m Character method: "naive", "mean", "lin_reg", "exp"
#' @param s Integer seasonality type: 1=year, 2=quarter, 3=month, 4=week
#' @param t Boolean whether to include trend
#' @return List with y (fitted + forecast), lower, and upper bounds
handleForecast <- function(y, h, m, s, t) {
  df <- tibble(year = seq.int(1, length(y)), asmr = y)

  # Convert to appropriate time series index based on seasonality
  if (s == 2) {
    df$year <- make_yearquarter(2000, 1) + 0:(length(y) - 1)
  } else if (s == 3) {
    df$year <- make_yearmonth(2000, 1) + 0:(length(y) - 1)
  } else if (s == 4) {
    df$year <- make_yearweek(2000, 1) + 0:(length(y) - 1)
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
          .groups = 'drop'
        )

      # Create forecast by repeating seasonal pattern
      forecast_seasons <- (nrow(df) + 0:(h-1)) %% s
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

      return(list(y = result$y, lower = result$lower, upper = result$upper))
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

      return(list(y = result$y, lower = result$lower, upper = result$upper))
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

  list(y = result$y, lower = result$lower, upper = result$upper)
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
#' @param y Numeric vector of cumulative observed values
#' @param h Integer horizon for forecasting
#' @param t Boolean whether to include trend
#' @return List with y (fitted + forecast), lower, and upper bounds
handleCumulativeForecast <- function(y, h, t) {
  n <- length(y)

  df <- tibble(year = seq.int(1, n), asmr = y) |>
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

  # Convert cumulative forecasts back to incremental values
  list(
    y = c(bl$.mean, uncumulate(result$asmr)),
    lower = c(rep(NA, nrow(bl)), uncumulate(result$lower)),
    upper = c(rep(NA, nrow(bl)), uncumulate(result$upper))
  )
}
