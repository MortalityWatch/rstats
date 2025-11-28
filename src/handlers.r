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

#' Calculate z-scores for all periods (pre-baseline, baseline, post-baseline, forecast)
#'
#' @param y_full Full dataset including all observed data
#' @param bs Baseline start index (1-indexed)
#' @param be Baseline end index (1-indexed)
#' @param baseline_residuals Residuals from baseline model fit
#' @param mdl Fitted model object
#' @param result_length Total length of result vector (includes forecast)
#' @param h Forecast horizon
#' @param df_baseline Baseline tsibble (for generating pre-baseline predictions)
#' @return Vector of z-scores
calculate_zscores <- function(y_full, bs, be, baseline_residuals,
                               mdl, result_length, h, df_baseline) {
  residual_sd <- sd(baseline_residuals, na.rm = TRUE)
  zscores <- rep(NA, result_length)

  # Pre-baseline z-scores (if bs > 1)
  if (bs > 1) {
    pre_baseline_data <- y_full[1:(bs - 1)]
    pre_baseline_non_na_idx <- which(!is.na(pre_baseline_data))
    pre_baseline_clean <- pre_baseline_data[pre_baseline_non_na_idx]

    if (length(pre_baseline_clean) > 0) {
      # Create new_data for pre-baseline period by going backwards from baseline start
      # We need to forecast "backwards" using new_data
      first_baseline_idx <- df_baseline$year[1]

      # Create pre-baseline time indices
      if (inherits(first_baseline_idx, "yearquarter")) {
        pre_indices <- first_baseline_idx - (bs - 1):1
      } else if (inherits(first_baseline_idx, "yearmonth")) {
        pre_indices <- first_baseline_idx - (bs - 1):1
      } else if (inherits(first_baseline_idx, "yearweek")) {
        pre_indices <- first_baseline_idx - (bs - 1):1
      } else {
        # Annual/integer index
        pre_indices <- (first_baseline_idx - (bs - 1)):(first_baseline_idx - 1)
      }

      pre_baseline_df <- tibble(year = pre_indices, asmr = pre_baseline_data) |>
        as_tsibble(index = year)

      # Use forecast with new_data to get predictions for pre-baseline
      pre_baseline_pred <- mdl |> forecast(new_data = pre_baseline_df)
      pre_baseline_fitted <- as_tibble(pre_baseline_pred) |> pull(.mean)

      # Calculate z-scores for pre-baseline (only non-NA positions)
      pre_baseline_residuals <- pre_baseline_clean - pre_baseline_fitted[pre_baseline_non_na_idx]
      pre_baseline_zscores <- pre_baseline_residuals / residual_sd

      zscores[pre_baseline_non_na_idx] <- round(pre_baseline_zscores, 3)
    }
  }

  # Baseline period z-scores (from model residuals)
  n_baseline_values <- length(baseline_residuals)
  zscores[bs:(bs + n_baseline_values - 1)] <-
    round(baseline_residuals / residual_sd, 3)

  # Post-baseline observed data z-scores
  if (be < length(y_full)) {
    post_baseline_data <- y_full[(be + 1):length(y_full)]
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

      # Assign z-scores to correct positions
      zscore_indices <- be + post_baseline_non_na_idx
      zscores[zscore_indices] <- round(post_baseline_zscores, 3)
    }
  }

  # Forecast period z-scores are NA (no observed data to compare)
  # They are already NA from initialization, no need to set them

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
#' @param bs Baseline start index (1-indexed, optional). If NULL, defaults to 1.
#' @param be Baseline end index (1-indexed, optional). If NULL, defaults to length(y).
#'
#' @details
#' Z-score Calculation:
#' Z-scores measure how much each observation deviates from what the baseline model predicts:
#' - Pre-baseline period: z = (observed - predicted) / baseline_sd (model extrapolates backwards)
#' - Baseline period: z = (observed - fitted) / baseline_sd (standard residuals)
#' - Post-baseline period: z = (observed - predicted) / baseline_sd (model forecasts forward)
#' - Forecast period: z = NA (no observed data to compare)
#'
#' This means z-scores measure deviation from the baseline model's prediction,
#' NOT from the baseline period's mean. A high z-score indicates the observed
#' value differs significantly from what the baseline model would have predicted.
#'
#' @return List with y (fitted + forecast), lower, and upper bounds, and zscore
handleForecast <- function(y, h, m, s, t, bs = NULL, be = NULL) {
  y_full <- y

  # Set defaults for baseline start/end if not provided
  if (is.null(bs)) bs <- 1L
  if (is.null(be)) be <- length(y)

  # Extract baseline data
  y_baseline <- y[bs:be]

  # Count leading NAs in the full input (for output alignment)
  # Leading NAs are NA values before the first non-NA value in the input
  first_non_na_idx <- which(!is.na(y))[1]
  leading_NA <- if (is.na(first_non_na_idx)) 0 else first_non_na_idx - 1

  # Create time series index starting from baseline start
  # Adjust for leading NAs when bs=1 (backwards compatible behavior)
  actual_bs <- if (bs == 1 && leading_NA > 0) leading_NA + 1 else bs
  y_baseline_clean <- if (bs == 1 && leading_NA > 0) y[(leading_NA + 1):be] else y_baseline

  df_baseline <- tibble(year = seq.int(actual_bs, be), asmr = y_baseline_clean)

  # Convert to appropriate time series index based on seasonality
  if (s == 2) {
    df_baseline$year <- make_yearquarter(2000, 1) + 0:(length(y_baseline_clean) - 1)
  } else if (s == 3) {
    df_baseline$year <- make_yearmonth(2000, 1) + 0:(length(y_baseline_clean) - 1)
  } else if (s == 4) {
    df_baseline$year <- make_yearweek(2000, 1) + 0:(length(y_baseline_clean) - 1)
  }

  # Convert to tsibble and remove any remaining interspersed NAs
  df_baseline <- df_baseline |>
    as_tsibble(index = year) |>
    filter(!is.na(asmr))

  # Fit model based on method
  if (m == "naive") {
    mdl <- df_baseline |> model(NAIVE(asmr))
  } else if (m == "mean") {
    if (s > 1) {
      mdl <- df_baseline |> model(TSLM(asmr ~ season()))
    } else {
      mdl <- df_baseline |> model(TSLM(asmr))
    }
  } else if (m == "median") {
    if (s > 1) {
      mdl <- df_baseline |> model(MEDIAN(asmr ~ season()))
    } else {
      mdl <- df_baseline |> model(MEDIAN(asmr))
    }
  } else if (m == "lin_reg") {
    if (t) {
      if (s > 1) {
        mdl <- df_baseline |> model(TSLM(asmr ~ trend() + season()))
      } else {
        mdl <- df_baseline |> model(TSLM(asmr ~ trend()))
      }
    } else {
      if (s > 1) {
        mdl <- df_baseline |> model(TSLM(asmr ~ season()))
      } else {
        mdl <- df_baseline |> model(TSLM(asmr))
      }
    }
  } else if (m == "exp") {
    if (s > 1) {
      mdl <- df_baseline |> model(ETS(asmr ~ error() + trend() + season()))
    } else {
      mdl <- df_baseline |> model(ETS(asmr ~ error("A") + trend("Ad")))
    }
  } else {
    stop(paste("Unknown method:", m))
  }

  # Get baseline (fitted values)
  bl <- mdl |>
    augment() |>
    rename(.mean = .fitted)

  # Calculate predictions for all periods
  # Account for leading NAs when bs=1
  n_pre_baseline <- if (bs == 1 && leading_NA > 0) 0 else (bs - 1)
  n_post_baseline <- length(y_full) - be

  # Generate predictions for pre-baseline period (if any) - no PI
  pre_baseline_result <- if (n_pre_baseline > 0) {
    first_baseline_idx <- df_baseline$year[1]
    # Create pre-baseline time indices
    if (inherits(first_baseline_idx, "yearquarter")) {
      pre_indices <- first_baseline_idx - n_pre_baseline:1
    } else if (inherits(first_baseline_idx, "yearmonth")) {
      pre_indices <- first_baseline_idx - n_pre_baseline:1
    } else if (inherits(first_baseline_idx, "yearweek")) {
      pre_indices <- first_baseline_idx - n_pre_baseline:1
    } else {
      pre_indices <- (first_baseline_idx - n_pre_baseline):(first_baseline_idx - 1)
    }
    pre_df <- tibble(year = pre_indices, asmr = y_full[1:n_pre_baseline]) |>
      as_tsibble(index = year)
    fc_pre <- mdl |> forecast(new_data = pre_df)
    tibble(y = as_tibble(fc_pre) |> pull(.mean), lower = NA, upper = NA)
  } else {
    tibble(y = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Generate predictions for post-baseline period with PI (this is the "forecast" region)
  # When bs/be are explicitly provided, post-baseline gets PI
  # h is only used for extending beyond observed data (legacy mode or explicit extension)
  post_baseline_result <- if (n_post_baseline > 0) {
    fc_post <- mdl |> forecast(h = n_post_baseline)
    fc_post_hilo <- fabletools::hilo(fc_post, 95) |>
      unpack_hilo(cols = `95%`) |>
      as_tibble() |>
      select(.mean, "95%_lower", "95%_upper") |>
      setNames(c("y", "lower", "upper"))
    fc_post_hilo
  } else {
    tibble(y = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Generate additional forecast beyond observed data (if h > 0)
  fc_beyond_result <- if (h > 0) {
    # Forecast h periods beyond the last observed data point
    fc_beyond <- mdl |> forecast(h = n_post_baseline + h)
    # Take only the last h periods (beyond observed data)
    fc_beyond_hilo <- fabletools::hilo(fc_beyond, 95) |>
      unpack_hilo(cols = `95%`) |>
      as_tibble() |>
      select(.mean, "95%_lower", "95%_upper") |>
      setNames(c("y", "lower", "upper")) |>
      tail(h)
    fc_beyond_hilo
  } else {
    tibble(y = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Combine all predictions: leading NAs, pre-baseline, baseline, post-baseline (with PI), forecast beyond (with PI)
  result <- bind_rows(
    tibble(y = rep(NA, leading_NA), lower = NA, upper = NA),
    pre_baseline_result,
    tibble(y = bl$.mean, lower = NA, upper = NA),
    post_baseline_result,
    fc_beyond_result
  ) |>
    mutate_if(is.numeric, round, 1)

  # Calculate z-scores using helper function
  # Use actual_bs for z-score calculation when there are leading NAs
  effective_bs <- if (bs == 1 && leading_NA > 0) actual_bs else bs
  baseline_residuals <- y_baseline_clean[!is.na(y_baseline_clean)] - bl$.mean
  zscores <- calculate_zscores(
    y_full = y_full,
    bs = effective_bs,
    be = be,
    baseline_residuals = baseline_residuals,
    mdl = mdl,
    result_length = length(result$y),
    h = h,
    df_baseline = df_baseline
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
#' @param bs Baseline start index (1-indexed, optional). If NULL, defaults to 1.
#' @param be Baseline end index (1-indexed, optional). If NULL, defaults to length(y).
#'
#' @details
#' Z-score calculation follows the same semantics as handleForecast():
#' measures deviation from the baseline model's prediction using baseline period's
#' standard deviation. See handleForecast() documentation for details.
#'
#' @return List with y (fitted + forecast), lower, and upper bounds, and zscore
handleCumulativeForecast <- function(y, h, t, bs = NULL, be = NULL) {
  y_full <- y

  # Set defaults for baseline start/end if not provided
  if (is.null(bs)) bs <- 1L
  if (is.null(be)) be <- length(y)

  # Extract baseline data
  y_baseline <- y[bs:be]
  n <- length(y_baseline)

  df_baseline <- tibble(year = seq.int(bs, be), asmr = y_baseline) |>
    as_tsibble(index = year)

  # Fit model with or without trend
  if (t) {
    mdl <- df_baseline |> model(lm = TSLM(asmr ~ trend()))
  } else {
    mdl <- df_baseline |> model(lm = TSLM(asmr))
  }

  # Get baseline (fitted values)
  bl <- mdl |>
    augment() |>
    rename(.mean = .fitted)

  # Calculate pre-baseline and post-baseline lengths
  n_pre_baseline <- bs - 1
  n_post_baseline <- length(y_full) - be

  # Generate predictions for pre-baseline period (if any) - no PI
  pre_baseline_predicted <- if (n_pre_baseline > 0) {
    first_baseline_idx <- df_baseline$year[1]
    pre_indices <- (first_baseline_idx - n_pre_baseline):(first_baseline_idx - 1)
    pre_df <- tibble(year = pre_indices, asmr = y_full[1:n_pre_baseline]) |>
      as_tsibble(index = year)
    fc_pre <- mdl |> forecast(new_data = pre_df)
    as_tibble(fc_pre) |> pull(.mean)
  } else {
    numeric(0)
  }

  # Generate cumulative forecasts for post-baseline period (with PI)
  post_baseline_result <- if (n_post_baseline > 0) {
    post_result <- tibble()
    for (h_ in 1:n_post_baseline) {
      df_test <- new_data(df_baseline, n = h_) |>
        mutate(asmr = 0)
      post_result <- rbind(post_result, cumForecastN(df_baseline, df_test, mdl))
    }
    post_result
  } else {
    tibble(asmr = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Generate cumulative forecasts for h periods beyond observed data (if h > 0)
  fc_beyond_result <- if (h > 0) {
    beyond_result <- tibble()
    for (h_ in (n_post_baseline + 1):(n_post_baseline + h)) {
      df_test <- new_data(df_baseline, n = h_) |>
        mutate(asmr = 0)
      beyond_result <- rbind(beyond_result, cumForecastN(df_baseline, df_test, mdl))
    }
    beyond_result
  } else {
    tibble(asmr = numeric(0), lower = numeric(0), upper = numeric(0))
  }

  # Calculate total result length
  result_length <- n_pre_baseline + nrow(bl) + n_post_baseline + h

  # Calculate z-scores using helper function
  baseline_residuals <- df_baseline$asmr - bl$.mean
  zscores <- calculate_zscores(
    y_full = y_full,
    bs = bs,
    be = be,
    baseline_residuals = baseline_residuals,
    mdl = mdl,
    result_length = result_length,
    h = h,
    df_baseline = df_baseline
  )

  # Combine all results with PI for post-baseline and beyond
  # Convert cumulative forecasts back to incremental values
  post_y <- if (nrow(post_baseline_result) > 0) uncumulate(post_baseline_result$asmr) else numeric(0)
  post_lower <- if (nrow(post_baseline_result) > 0) uncumulate(post_baseline_result$lower) else numeric(0)
  post_upper <- if (nrow(post_baseline_result) > 0) uncumulate(post_baseline_result$upper) else numeric(0)

  beyond_y <- if (nrow(fc_beyond_result) > 0) uncumulate(fc_beyond_result$asmr) else numeric(0)
  beyond_lower <- if (nrow(fc_beyond_result) > 0) uncumulate(fc_beyond_result$lower) else numeric(0)
  beyond_upper <- if (nrow(fc_beyond_result) > 0) uncumulate(fc_beyond_result$upper) else numeric(0)

  list(
    y = c(pre_baseline_predicted, bl$.mean, post_y, beyond_y),
    lower = c(rep(NA, n_pre_baseline + nrow(bl)), post_lower, beyond_lower),
    upper = c(rep(NA, n_pre_baseline + nrow(bl)), post_upper, beyond_upper),
    zscore = zscores
  )
}
