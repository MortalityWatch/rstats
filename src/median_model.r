# Custom MEDIAN model for fabletools
# Similar to MEAN but uses median instead

library(fabletools)
library(rlang)

#' MEDIAN forecast model
#' @param formula Model formula (e.g., y ~ 1 or y ~ season())
#' @param ... Additional arguments
#' @export
MEDIAN <- function(formula, ...) {
  median_specials <- fabletools::new_specials(
    season = function(period = NULL) {
      period
    },
    .required_specials = character(0)
  )

  median_model <- fabletools::new_model_class(
    "median",
    train = train_median,
    specials = median_specials
  )
  fabletools::new_model_definition(median_model, !!enquo(formula), ...)
}

# Training function for MEDIAN model
train_median <- function(.data, specials, ...) {
  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by MEDIAN.")
  }

  y <- unclass(.data)[[measured_vars(.data)]]

  if (all(is.na(y))) {
    abort("All observations are missing, a model cannot be estimated without data.")
  }

  n <- length(y)

  # Check if seasonality was specified
  has_season <- !is.null(specials$season) && length(specials$season) > 0

  if (has_season) {
    # Seasonal median: calculate median for each season
    # Determine season period from the time series
    # (e.g., 4 for quarterly, 12 for monthly, 52 for weekly)
    # Note: When called from API with s > 1, the time index is
    # always yearquarter/yearmonth/yearweek, so period will be
    # 4, 12, or 52 respectively (never 1)
    period <- get_frequencies(NULL, .data, .auto = "smallest")

    # Calculate seasonal medians
    season_idx <- seq_len(n) %% period
    season_idx[season_idx == 0] <- period

    seasonal_stats <- lapply(seq_len(period), function(s) {
      y_season <- y[season_idx == s]
      list(
        median = median(y_season, na.rm = TRUE),
        sd = sd(y_season, na.rm = TRUE)
      )
    })

    # Create fitted values using seasonal medians
    fits <- sapply(season_idx, function(s) seasonal_stats[[s]]$median)

    # Calculate residuals and overall sigma
    res <- y - fits
    sigma <- sd(res, na.rm = TRUE)

    structure(
      list(
        fitted = fits,
        resid = res,
        seasonal_medians = seasonal_stats,
        period = period,
        sigma = sigma,
        nobs = sum(!is.na(y)),
        seasonal = TRUE
      ),
      class = "model_median"
    )
  } else {
    # Non-seasonal median
    y_median <- median(y, na.rm = TRUE)
    fits <- rep(y_median, n)

    # Calculate residuals and sigma
    res <- y - fits
    sigma <- sd(res, na.rm = TRUE)

    structure(
      list(
        fitted = fits,
        resid = res,
        median = y_median,
        sigma = sigma,
        nobs = sum(!is.na(y)),
        seasonal = FALSE
      ),
      class = "model_median"
    )
  }
}

# Forecast method for MEDIAN model
#' @export
forecast.model_median <- function(object, new_data, specials = NULL, ...) {
  h <- nrow(new_data)

  if (object$seasonal) {
    # Seasonal forecast: repeat seasonal pattern
    n_obs <- object$nobs
    period <- object$period

    # Determine which season each forecast point belongs to
    forecast_seasons <- ((n_obs + seq_len(h) - 1) %% period) + 1

    # Get median and sd for each forecast season
    fc_medians <- sapply(forecast_seasons, function(s) {
      object$seasonal_medians[[s]]$median
    })

    # Use per-season standard deviation for more accurate confidence intervals
    fc_sds <- sapply(forecast_seasons, function(s) {
      object$seasonal_medians[[s]]$sd
    })

    # Return distributions for each forecast point with per-season uncertainty
    distributional::dist_normal(fc_medians, fc_sds)
  } else {
    # Non-seasonal forecast: constant median
    distributional::dist_normal(object$median, object$sigma)[rep(1, h)]
  }
}

# Fitted values method
#' @export
fitted.model_median <- function(object, ...) {
  object$fitted
}

# Residuals method
#' @export
residuals.model_median <- function(object, ...) {
  object$resid
}
