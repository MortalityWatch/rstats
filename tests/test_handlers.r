# Unit tests for handler functions
library(testthat)
library(tibble)
library(fable)
library(tidyverse)
library(tsibble)

# Source required files
source("../src/utils.r")
source("../src/handlers.r")

# Helper function to check result structure
check_forecast_result <- function(result, expected_length) {
  expect_true("y" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("zscore" %in% names(result))
  expect_equal(length(result$y), expected_length)
  expect_equal(length(result$lower), expected_length)
  expect_equal(length(result$upper), expected_length)
  expect_equal(length(result$zscore), expected_length)
}

# ============================================================================
# handleForecast() tests
# ============================================================================

test_that("handleForecast with mean method works", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "mean"
  s <- 1 # Annual
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)

  # Mean forecast should be constant
  forecast_values <- tail(result$y, h)
  expect_true(all(forecast_values == forecast_values[1]))
})

test_that("handleForecast with linear regression works", {
  y <- c(100, 105, 110, 115, 120, 125, 130)
  h <- 3
  m <- "lin_reg"
  s <- 1
  t <- TRUE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)

  # With trend, forecast should generally increase
  forecast_values <- tail(result$y, h)
  expect_true(all(!is.na(forecast_values)))
})

test_that("handleForecast with naive method works", {
  y <- c(100, 105, 110, 115, 120)
  h <- 2
  m <- "naive"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with exponential smoothing works", {
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  h <- 3
  m <- "exp"
  s <- 1
  t <- TRUE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with median method (non-seasonal) works", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "median"
  s <- 1 # Non-seasonal
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)

  # Median forecast should be constant (the median value)
  forecast_values <- tail(result$y, h)
  expect_true(all(forecast_values == forecast_values[1]))
  expect_equal(forecast_values[1], median(y))
})

test_that("handleForecast with median method (seasonal) works", {
  # 12 months of data (s=3 means monthly)
  y <- c(100, 110, 105, 102, 112, 107, 104, 114, 109, 106, 116, 111)
  h <- 3
  m <- "median"
  s <- 3 # Monthly seasonality
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast handles leading NAs correctly", {
  y <- c(NA, NA, 100, 105, 110, 115, 120)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)

  # First two values should be NA
  expect_true(is.na(result$y[1]))
  expect_true(is.na(result$y[2]))
  expect_true(is.na(result$zscore[1]))
  expect_true(is.na(result$zscore[2]))
})

test_that("handleForecast with different seasonality types works", {
  y <- rep(100:107, 2) # 16 values
  h <- 2
  m <- "mean"
  t <- FALSE

  # Test quarterly (s=2)
  result_q <- handleForecast(y, h, m, s = 2, t)
  check_forecast_result(result_q, length(y) + h)

  # Test monthly (s=3)
  result_m <- handleForecast(y, h, m, s = 3, t)
  check_forecast_result(result_m, length(y) + h)

  # Test weekly (s=4)
  result_w <- handleForecast(y, h, m, s = 4, t)
  check_forecast_result(result_w, length(y) + h)
})

# ============================================================================
# Z-score calculation tests
# ============================================================================

test_that("Z-scores are calculated correctly for mean method", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  # Z-scores for observed data should be calculated
  observed_zscores <- result$zscore[1:length(y)]
  expect_true(all(!is.na(observed_zscores)))

  # Z-scores for forecast period should be NA (no observed data)
  forecast_zscores <- tail(result$zscore, h)
  expect_true(all(is.na(forecast_zscores)))
})

test_that("Z-scores have mean ~0 and sd ~1 for observed data", {
  y <- c(100, 105, 110, 108, 112, 115, 120, 118, 125, 122)
  h <- 2
  m <- "lin_reg"
  s <- 1
  t <- TRUE

  result <- handleForecast(y, h, m, s, t)

  observed_zscores <- result$zscore[1:length(y)]

  # Z-scores should have mean close to 0 and sd close to 1
  expect_true(abs(mean(observed_zscores)) < 0.1)
  expect_true(abs(sd(observed_zscores) - 1) < 0.1)
})

# ============================================================================
# Baseline parameters tests (bs/be for PR, b for legacy backwards compat)
# ============================================================================

test_that("bs/be parameters split data correctly", {
  # 10 years of data, use indices 1-5 for baseline (same as legacy b=5)
  y <- c(100, 105, 110, 108, 112, 115, 120, 125, 130, 135)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 1, be = 5)

  check_forecast_result(result, length(y) + h)
})

test_that("bs/be calculates z-scores for all data using baseline stats", {
  # Create data where post-baseline has higher values
  y_baseline <- c(100, 105, 110, 108, 112) # Mean ~107
  y_post <- c(200, 205, 210) # Much higher
  y <- c(y_baseline, y_post)

  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 1, be = 5)

  check_forecast_result(result, length(y) + h)

  # Post-baseline z-scores should be very high (positive)
  # since post-baseline values are much higher than baseline mean
  post_baseline_zscores <- result$zscore[6:8]
  expect_true(all(post_baseline_zscores > 2))
})

test_that("bs/be NULL uses all data (backwards compatible)", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "mean"
  s <- 1
  t <- FALSE

  result_with_null <- handleForecast(y, h, m, s, t, bs = NULL, be = NULL)
  result_without <- handleForecast(y, h, m, s, t)

  # Results should be identical
  expect_equal(result_with_null$y, result_without$y)
  expect_equal(result_with_null$zscore, result_without$zscore)
})

test_that("bs/be works with different methods", {
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  h <- 2
  s <- 1
  t <- TRUE

  # Test with linear regression
  result_lr <- handleForecast(y, h, "lin_reg", s, t, bs = 1, be = 5)
  check_forecast_result(result_lr, length(y) + h)

  # Test with exponential smoothing
  result_exp <- handleForecast(y, h, "exp", s, t, bs = 1, be = 5)
  check_forecast_result(result_exp, length(y) + h)

  # Test with median
  result_med <- handleForecast(y, h, "median", s, FALSE, bs = 1, be = 5)
  check_forecast_result(result_med, length(y) + h)
})

test_that("bs/be handles interspersed NAs in post-baseline period", {
  # Create data with baseline period and post-baseline with interspersed NAs
  y <- c(100, 105, 110, 108, 112, 115, NA, 120, NA, 125)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 1, be = 5)

  check_forecast_result(result, length(y) + h)

  # Z-scores should be NA at positions 7 and 9 (where data is NA)
  expect_true(is.na(result$zscore[7]))
  expect_true(is.na(result$zscore[9]))

  # Z-scores should be calculated for non-NA positions 6, 8, 10
  expect_true(!is.na(result$zscore[6]))  # Value 115
  expect_true(!is.na(result$zscore[8]))  # Value 120
  expect_true(!is.na(result$zscore[10])) # Value 125

  # Forecast z-scores should be NA (no observed data)
  expect_true(is.na(result$zscore[11]))
  expect_true(is.na(result$zscore[12]))
})

# ============================================================================
# Pre-baseline z-score tests (new bs/be feature)
# ============================================================================

test_that("bs > 1 calculates pre-baseline z-scores", {
  # 10 years of data, baseline is years 3-5
  y <- c(100, 105, 110, 108, 112, 200, 205, 210, 215, 220)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 3, be = 5)

  check_forecast_result(result, length(y) + h)

  # Pre-baseline z-scores (indices 1-2) should be calculated
  expect_true(!is.na(result$zscore[1]))
  expect_true(!is.na(result$zscore[2]))

  # Baseline z-scores (indices 3-5) should be calculated
  expect_true(!is.na(result$zscore[3]))
  expect_true(!is.na(result$zscore[4]))
  expect_true(!is.na(result$zscore[5]))

  # Post-baseline z-scores (indices 6-10) should be calculated
  expect_true(!is.na(result$zscore[6]))
  expect_true(!is.na(result$zscore[10]))

  # Forecast z-scores should be NA
  expect_true(is.na(result$zscore[11]))
  expect_true(is.na(result$zscore[12]))
})

test_that("Pre-baseline z-scores use model prediction (not baseline mean)", {
  # Data where pre-baseline is similar to baseline (mean model)
  # With mean model, pre-baseline prediction = baseline mean
  y <- c(108, 112, 100, 105, 110, 200, 205, 210)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 3, be = 5)

  # Baseline mean is (100+105+110)/3 = 105
  # Pre-baseline values are 108 and 112
  # Pre-baseline z-scores should be positive (above mean)
  expect_true(result$zscore[1] > 0)  # 108 > 105
  expect_true(result$zscore[2] > 0)  # 112 > 105
})

test_that("Full example from spec works correctly", {
  # From spec: 10 years of data, baseline 3-5, forecast 3
  y <- c(100, 105, 110, 115, 120, 150, 180, 160, 140, 130)
  h <- 3
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 3, be = 5)

  # Result should have 13 values (10 observed + 3 forecast)
  expect_equal(length(result$y), 13)
  expect_equal(length(result$zscore), 13)

  # Z-scores for all observed data should be calculated
  expect_true(all(!is.na(result$zscore[1:10])))

  # Z-scores for forecast should be NA
  expect_true(all(is.na(result$zscore[11:13])))

  # Confidence intervals only for post-baseline and forecast periods
  # Pre-baseline (1-2) and baseline (3-5) should have NA for lower/upper
  expect_true(all(is.na(result$lower[1:5])))
  # Post-baseline (6-10) and forecast (11-13) should have PI
  expect_true(all(!is.na(result$lower[6:13])))
})

# ============================================================================
# handleCumulativeForecast() tests
# ============================================================================

test_that("handleCumulativeForecast works with trend", {
  y <- c(1000, 2100, 3300, 4600)
  h <- 2
  t <- TRUE

  result <- handleCumulativeForecast(y, h, t)

  expect_true("y" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("zscore" %in% names(result))

  # Should return cumulative values
  expect_true(all(!is.na(result$y[1:length(y)])))
})

test_that("handleCumulativeForecast works without trend", {
  y <- c(1000, 2000, 3000, 4000)
  h <- 2
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t)

  expect_true("y" %in% names(result))
  expect_equal(length(result$zscore), length(y) + h)
})

test_that("handleCumulativeForecast returns cumulative baseline values", {
  # Issue #14: Baseline should accumulate, not be flat
  # With a constant mean baseline of 466.175, output should be:
  # Period 1: 466.175, Period 2: 932.35, Period 3: 1398.53, etc.
  y <- c(470.7, 471.5, 464.1, 458.4, 520.7, 571.2, 494.7, 456.3, 434.1)
  h <- 1
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  # Baseline values (first 4) should be cumulative (strictly increasing)
  baseline_y <- result$y[1:4]
  expect_true(all(diff(baseline_y) > 0), info = "Baseline values should be strictly increasing (cumulative)")

  # Each subsequent baseline value should be roughly the first + increments
  # (not flat/repeated values)
  expect_true(baseline_y[2] > baseline_y[1] * 1.5, info = "Second baseline should be > 1.5x first (cumulative)")
  expect_true(baseline_y[4] > baseline_y[1] * 3, info = "Fourth baseline should be > 3x first (cumulative)")
})

test_that("handleCumulativeForecast returns cumulative post-baseline values", {
  # Post-baseline values should continue cumulating from baseline total
  y <- c(100, 100, 100, 100, 100, 100)  # Constant values
  h <- 2
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  # With constant baseline of 100:
  # Baseline cumulative: 100, 200, 300, 400
  # Post-baseline (periods 5,6): should be ~500, ~600
  # Forecast: should be ~700, ~800
  expect_equal(result$y[1], 100, tolerance = 1)
  expect_equal(result$y[2], 200, tolerance = 1)
  expect_equal(result$y[3], 300, tolerance = 1)
  expect_equal(result$y[4], 400, tolerance = 1)
  expect_equal(result$y[5], 500, tolerance = 1)  # First post-baseline
  expect_equal(result$y[6], 600, tolerance = 1)  # Second post-baseline
  expect_equal(result$y[7], 700, tolerance = 1)  # First forecast
  expect_equal(result$y[8], 800, tolerance = 1)  # Second forecast
})

test_that("handleCumulativeForecast prediction intervals widen over time", {
  # Issue #14: Prediction intervals should widen as uncertainty accumulates
  y <- c(100, 100, 100, 100, 120, 110, 130, 140)
  h <- 3
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  # Post-baseline lower/upper bounds should exist
  post_lower <- result$lower[5:length(result$lower)]
  post_upper <- result$upper[5:length(result$upper)]

  expect_true(all(!is.na(post_lower)), info = "Post-baseline lower bounds should not be NA")
  expect_true(all(!is.na(post_upper)), info = "Post-baseline upper bounds should not be NA")

  # Prediction interval width should increase over time
  interval_widths <- post_upper - post_lower
  expect_true(all(diff(interval_widths) >= 0), info = "PI widths should increase or stay same over time")
})

test_that("handleCumulativeForecast y values are within prediction interval bounds", {
  # Ensure y is always within [lower, upper] where bounds exist
  y <- c(470.7, 471.5, 464.1, 458.4, 520.7, 571.2, 494.7, 456.3, 434.1)
  h <- 2
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  # Check that y is within bounds where they exist
  has_bounds <- !is.na(result$lower) & !is.na(result$upper)
  y_with_bounds <- result$y[has_bounds]
  lower_bounds <- result$lower[has_bounds]
  upper_bounds <- result$upper[has_bounds]

  expect_true(all(y_with_bounds >= lower_bounds),
    info = "y values must be >= lower bound")
  expect_true(all(y_with_bounds <= upper_bounds),
    info = "y values must be <= upper bound")
})

test_that("handleCumulativeForecast with bs/be", {
  y <- c(1000, 2100, 3300, 4600, 6000, 7500)
  h <- 2
  t <- TRUE

  result <- handleCumulativeForecast(y, h, t, bs = 1, be = 4)

  expect_equal(length(result$zscore), length(y) + h)

  # Post-baseline z-scores should be calculated
  post_baseline_zscores <- result$zscore[5:6]
  expect_true(all(!is.na(post_baseline_zscores)))

  # Forecast z-scores should be NA
  expect_true(all(is.na(result$zscore[7:8])))
})

test_that("handleCumulativeForecast with pre-baseline (bs > 1)", {
  # 8 years of data, baseline is years 3-5 (need at least 3 points)
  y <- c(1000, 2100, 3300, 4600, 6000, 7500, 9100, 10800)
  h <- 2
  t <- TRUE

  result <- handleCumulativeForecast(y, h, t, bs = 3, be = 5)

  expect_equal(length(result$zscore), length(y) + h)

  # Pre-baseline z-scores should be calculated
  expect_true(!is.na(result$zscore[1]))
  expect_true(!is.na(result$zscore[2]))

  # Baseline z-scores should be calculated
  expect_true(!is.na(result$zscore[3]))
  expect_true(!is.na(result$zscore[4]))
  expect_true(!is.na(result$zscore[5]))

  # Post-baseline z-scores should be calculated
  expect_true(!is.na(result$zscore[6]))
  expect_true(!is.na(result$zscore[7]))
  expect_true(!is.na(result$zscore[8]))

  # Forecast z-scores should be NA
  expect_true(all(is.na(result$zscore[9:10])))
})

test_that("handleCumulativeForecast preserves leading NAs in output", {
  # Issue: /cum endpoint was stripping leading nulls causing index mismatch
  # Input: 10 leading NAs, then 9 actual values
  # bs=11, be=14 refer to positions in the FULL 19-element array
  y <- c(rep(NA, 10), 402.3, 413.9, 390.5, 400.7, 394.7, 446.1, 459.2, 413.7, 404.3)
  h <- 1  # h must be >= 1
  t <- FALSE

  result <- handleCumulativeForecast(y, h, t, bs = 11, be = 14)

  # Output should have length = input + h (19 + 1 = 20 elements)
  expect_equal(length(result$y), length(y) + h)
  expect_equal(length(result$lower), length(y) + h)
  expect_equal(length(result$upper), length(y) + h)
  expect_equal(length(result$zscore), length(y) + h)

  # Leading positions (1-10) should be NA in y
  expect_true(all(is.na(result$y[1:10])), info = "Leading NA positions should remain NA in output")

  # Baseline positions (11-14) should have cumulative values
  expect_true(all(!is.na(result$y[11:14])), info = "Baseline positions should have values")
  expect_true(result$y[11] > 0)
  expect_true(result$y[14] > result$y[11])  # Cumulative should increase

  # Post-baseline positions (15-19) should have values
  expect_true(all(!is.na(result$y[15:19])), info = "Post-baseline positions should have values")

  # Z-scores for leading NA positions should be NA
  expect_true(all(is.na(result$zscore[1:10])), info = "Z-scores for leading NAs should be NA")

  # Z-scores for baseline and post-baseline should be calculated
  expect_true(all(!is.na(result$zscore[11:19])), info = "Z-scores should be calculated for non-NA data")

  # Forecast z-score should be NA
  expect_true(is.na(result$zscore[20]), info = "Forecast z-score should be NA")
})

test_that("pre-baseline predictions don't inflate cumulative offset", {
  # Regression test for issue #310: pre-baseline was incorrectly added to

  # cumulative offset, causing excess mortality to show ~-55% instead of
  # correct values
  y <- c(100, 100, 100, 100, 100, 100)  # Constant values
  h <- 0
  t <- FALSE

  # With bs=3, pre-baseline is years 1-2, baseline is 3-4
  result <- handleCumulativeForecast(y, h, t, bs = 3, be = 4)

  # Post-baseline (periods 5,6) should continue from baseline_total only
  # Baseline total for 2 periods of ~100 = ~200
  # So period 5 should be ~300, period 6 should be ~400
  expect_equal(result$y[5], 300, tolerance = 10)
  expect_equal(result$y[6], 400, tolerance = 10)

  # NOT ~500 and ~600 (which would happen if pre_total was included)
})

# ============================================================================
# Edge cases and error handling
# ============================================================================

test_that("handleForecast rejects unknown method", {
  y <- c(100, 105, 110, 115)
  h <- 2
  m <- "unknown_method"
  s <- 1
  t <- FALSE

  expect_error(
    handleForecast(y, h, m, s, t),
    "Unknown method"
  )
})

test_that("Z-score precision is 3 decimals", {
  y <- c(100.1234, 105.5678, 110.9012, 108.3456, 112.7890)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  # Check that z-scores are rounded to 3 decimals
  observed_zscores <- result$zscore[1:length(y)]
  for (z in observed_zscores) {
    if (!is.na(z)) {
      # Count decimal places
      z_str <- as.character(z)
      if (grepl("\\.", z_str)) {
        decimals <- nchar(strsplit(z_str, "\\.")[[1]][2])
        expect_true(decimals <= 3)
      }
    }
  }
})

test_that("Forecast values are rounded to 2 decimals", {
  y <- c(100.123, 105.456, 110.789)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  # Check forecast values are rounded to 2 decimals
  for (val in result$y) {
    if (!is.na(val)) {
      val_str <- as.character(val)
      if (grepl("\\.", val_str)) {
        decimals <- nchar(strsplit(val_str, "\\.")[[1]][2])
        expect_true(decimals <= 2)
      }
    }
  }
})

# ============================================================================
# xs (start time index) parameter tests
# ============================================================================

test_that("parse_xs correctly parses weekly format", {
  # Test various weekly formats
  result <- parse_xs("2020W10", 4)
  expect_true(inherits(result, "yearweek"))
  expect_equal(format(result), "2020 W10")

  # With hyphen
  result2 <- parse_xs("2020-W10", 4)
  expect_equal(format(result2), "2020 W10")

  # Week 1
  result3 <- parse_xs("2020W01", 4)
  expect_equal(format(result3), "2020 W01")

  # Week 53
  result4 <- parse_xs("2020W53", 4)
  expect_equal(format(result4), "2020 W53")
})

test_that("parse_xs correctly parses monthly format", {
  result <- parse_xs("2020-01", 3)
  expect_true(inherits(result, "yearmonth"))
  expect_equal(format(result), "2020 Jan")

  # Without hyphen
  result2 <- parse_xs("202012", 3)
  expect_equal(format(result2), "2020 Dec")
})

test_that("parse_xs correctly parses quarterly format", {
  result <- parse_xs("2020Q1", 2)
  expect_true(inherits(result, "yearquarter"))
  expect_equal(format(result), "2020 Q1")

  # With hyphen
  result2 <- parse_xs("2020-Q4", 2)
  expect_equal(format(result2), "2020 Q4")
})

test_that("parse_xs correctly parses yearly format", {
  result <- parse_xs("2020", 1)
  expect_equal(result, 2020L)
})

test_that("parse_xs returns NULL for NULL input", {
  expect_null(parse_xs(NULL, 1))
  expect_null(parse_xs(NULL, 4))
})

test_that("handleForecast with xs uses correct weekly time indices", {
  # Create 8 weeks of data starting from week 50 of 2020
  # This crosses year boundary and includes potential week 53
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  h <- 4
  m <- "mean"
  s <- 4  # Weekly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2020W50")

  check_forecast_result(result, length(y) + h)

  # Should have forecasts
  expect_true(all(!is.na(result$y)))
})

test_that("handleForecast with xs uses correct monthly time indices", {
  # 12 months of data starting from October 2020
  y <- c(100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155)
  h <- 3
  m <- "mean"
  s <- 3  # Monthly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2020-10")

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with xs uses correct quarterly time indices", {
  # 8 quarters of data starting from Q3 2020
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  h <- 2
  m <- "mean"
  s <- 2  # Quarterly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2020Q3")

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with xs uses correct yearly time indices", {
  # 5 years of data starting from 2018
  y <- c(100, 105, 110, 115, 120)
  h <- 2
  m <- "mean"
  s <- 1  # Yearly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2018")

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast without xs falls back to synthetic indices", {
  # This should work the same as before (backwards compatible)
  y <- c(100, 105, 110, 115, 120)
  h <- 2
  m <- "mean"
  s <- 4  # Weekly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  check_forecast_result(result, length(y) + h)
})

test_that("handleForecast with xs and bs/be works correctly", {
  # 10 weeks of data, baseline is weeks 3-6, starting from week 10
  y <- c(100, 105, 110, 108, 112, 115, 200, 205, 210, 215)
  h <- 2
  m <- "mean"
  s <- 4  # Weekly
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, bs = 3, be = 6, xs = "2020W10")

  check_forecast_result(result, length(y) + h)

  # Pre-baseline z-scores should be calculated
  expect_true(!is.na(result$zscore[1]))
  expect_true(!is.na(result$zscore[2]))

  # Post-baseline z-scores (higher values) should be positive
  expect_true(result$zscore[7] > 0)
  expect_true(result$zscore[10] > 0)
})

test_that("handleForecast with xs handles week 53 correctly", {
  # Data spanning a year with week 53 (2020 had 53 weeks)
  # Start at week 51 of 2020, which should go: W51, W52, W53, then 2021 W01, W02, etc.
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)  # 8 weeks
  h <- 4
  m <- "mean"
  s <- 4
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, xs = "2020W51")

  check_forecast_result(result, length(y) + h)

  # Model should handle week 53 without error
  expect_true(all(!is.na(result$y)))
})

test_that("handleForecast seasonal patterns align with actual calendar weeks", {
  # Create synthetic weekly data with clear seasonal pattern
  # Weekly pattern: higher values at end of each "year" (week 52)
  # 104 weeks = 2 years of weekly data starting from week 1
  set.seed(42)
  weekly_pattern <- rep(c(rep(100, 51), 150), 2)  # Week 52 is higher
  y <- weekly_pattern + rnorm(104, 0, 5)
  h <- 52
  m <- "mean"
  s <- 4
  t <- FALSE

  # With xs starting at week 1, the seasonal pattern should be learned correctly
  result <- handleForecast(y, h, m, s, t, xs = "2020W01")

  check_forecast_result(result, length(y) + h)

  # The forecast for week 52 positions should be higher than other weeks
  # Forecast starts at position 105, so week 52 of year 3 would be at position 156
  forecast_values <- tail(result$y, h)
  # Week 52 values (positions 52 in forecast) should be elevated
  expect_true(forecast_values[52] > mean(forecast_values[1:51]))
})

# ============================================================================
# handleASD() tests - Age-Standardized Deaths
# ============================================================================

# Helper function to check ASD result structure
check_asd_result <- function(result, expected_length) {
  expect_true("asd" %in% names(result))
  expect_true("asd_bl" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("zscore" %in% names(result))
  expect_equal(length(result$asd), expected_length)
  expect_equal(length(result$asd_bl), expected_length)
  expect_equal(length(result$lower), expected_length)
  expect_equal(length(result$upper), expected_length)
  expect_equal(length(result$zscore), expected_length)
}

test_that("handleASD with mean method works", {
  # Simple case: deaths increase with population
  deaths <- c(1000, 1050, 1100, 1080, 1120, 1150, 1200)
  population <- c(100000, 101000, 102000, 103000, 104000, 105000, 106000)
  h <- 0  # No forecast beyond data
  m <- "mean"
  t <- FALSE

  result <- handleASD(deaths, population, h, m, t)

  check_asd_result(result, length(deaths))

  # asd should equal input deaths
  expect_equal(result$asd, deaths)

  # asd_bl should be calculated (rate * population)
  expect_true(all(!is.na(result$asd_bl)))
})

test_that("handleASD with lin_reg and trend works", {
  deaths <- c(1000, 1100, 1200, 1300, 1400, 1500, 1600)
  population <- c(100000, 100000, 100000, 100000, 100000, 100000, 100000)
  h <- 0
  m <- "lin_reg"
  t <- TRUE

  result <- handleASD(deaths, population, h, m, t)

  check_asd_result(result, length(deaths))

  # With linear trend in deaths and constant population,
  # the fitted values should follow the trend
  expect_true(all(!is.na(result$asd_bl)))
})

test_that("handleASD with median method works", {
  deaths <- c(1000, 1050, 1100, 1080, 1120, 1150, 1200)
  population <- c(100000, 100000, 100000, 100000, 100000, 100000, 100000)
  h <- 0
  m <- "median"
  t <- FALSE

  result <- handleASD(deaths, population, h, m, t)

  check_asd_result(result, length(deaths))

  # Median baseline should produce constant expected rate
  # asd_bl should be calculated (median rate * population)
  expect_true(all(!is.na(result$asd_bl)))
})

test_that("handleASD with naive method works", {
  # Use with baseline params to test post-baseline forecasting
  deaths <- c(1000, 1050, 1100, 1500, 1550, 1600)
  population <- c(100000, 100000, 100000, 100000, 100000, 100000)
  h <- 0
  m <- "naive"
  t <- FALSE
  bs <- 1
  be <- 3

  result <- handleASD(deaths, population, h, m, t, bs, be)

  check_asd_result(result, length(deaths))

  # Naive uses last baseline rate for forecasting
  # Post-baseline asd_bl should be calculated (periods 4-6)
  expect_true(all(!is.na(result$asd_bl[4:6])))

  # Post-baseline z-scores should be positive (deaths higher than expected)
  expect_true(result$zscore[4] > 0)
})

test_that("handleASD with exp (ETS) method works", {
  # ETS needs more data points
  deaths <- c(1000, 1050, 1100, 1080, 1120, 1150, 1200, 1180, 1220, 1250)
  population <- c(100000, 100000, 100000, 100000, 100000, 100000, 100000, 100000, 100000, 100000)
  h <- 0
  m <- "exp"
  t <- FALSE
  bs <- 1
  be <- 7

  result <- handleASD(deaths, population, h, m, t, bs, be)

  check_asd_result(result, length(deaths))

  # ETS should produce smoothed forecasts
  # Post-baseline asd_bl should be calculated
  expect_true(all(!is.na(result$asd_bl[8:10])))
})

test_that("handleASD with baseline parameters works", {
  # Baseline period has lower death rate, post-baseline has higher
  deaths <- c(1000, 1020, 1010, 1030, 1500, 1550, 1600)
  population <- c(100000, 100000, 100000, 100000, 100000, 100000, 100000)
  h <- 0
  m <- "mean"
  t <- FALSE
  bs <- 1
  be <- 4  # Baseline is first 4 periods

  result <- handleASD(deaths, population, h, m, t, bs, be)

  check_asd_result(result, length(deaths))

  # Post-baseline z-scores should be positive (deaths higher than expected)
  expect_true(result$zscore[5] > 0)
  expect_true(result$zscore[6] > 0)
  expect_true(result$zscore[7] > 0)
})

test_that("handleASD correctly calculates expected deaths with population changes", {
  # Key test: same mortality rate, but population doubles
  # Baseline period has rate = 0.01 (1%)
  # Expected deaths should scale with population
  deaths <- c(100, 100, 100, 200, 200)  # Rate stays at 1%
  population <- c(10000, 10000, 10000, 20000, 20000)  # Population doubles
  h <- 0
  m <- "mean"
  t <- FALSE
  bs <- 1
  be <- 3

  result <- handleASD(deaths, population, h, m, t, bs, be)

  check_asd_result(result, length(deaths))

  # Expected deaths for period 4 and 5 should be ~200 (rate 0.01 * pop 20000)
  # This is because we apply baseline rate to current population
  expect_equal(result$asd_bl[4], 200, tolerance = 1)
  expect_equal(result$asd_bl[5], 200, tolerance = 1)

  # Z-scores should be close to 0 since deaths match expected
  expect_true(abs(result$zscore[4]) < 0.5)
  expect_true(abs(result$zscore[5]) < 0.5)
})

test_that("handleASD detects excess mortality due to rate change", {
  # Baseline: rate = 0.01, Post-baseline: rate increases to 0.015
  deaths <- c(100, 100, 100, 150, 150)  # Rate increases from 1% to 1.5%
  population <- c(10000, 10000, 10000, 10000, 10000)  # Population constant
  h <- 0
  m <- "mean"
  t <- FALSE
  bs <- 1
  be <- 3

  result <- handleASD(deaths, population, h, m, t, bs, be)

  # Post-baseline expected deaths should still be ~100 (baseline rate * pop)
  expect_equal(result$asd_bl[4], 100, tolerance = 1)

  # But actual deaths are 150, so z-score should be strongly positive
  expect_true(result$zscore[4] > 2)  # More than 2 SDs above expected
  expect_true(result$zscore[5] > 2)
})

test_that("handleASD with forecast horizon extends output", {
  deaths <- c(1000, 1050, 1100, 1080, 1120)
  population <- c(100000, 101000, 102000, 103000, 104000)
  h <- 3  # Forecast 3 periods beyond data
  m <- "mean"
  t <- FALSE

  result <- handleASD(deaths, population, h, m, t)

  # Output should be extended by h periods
  check_asd_result(result, length(deaths) + h)

  # Forecast deaths should be NA
  expect_true(all(is.na(tail(result$asd, h))))

  # Forecast expected deaths should use last known population
  expect_true(all(!is.na(tail(result$asd_bl, h))))

  # Forecast z-scores should be NA (no observed data to compare)
  expect_true(all(is.na(tail(result$zscore, h))))
})

test_that("handleASD with lin_reg extrapolates trend correctly", {
  # Declining mortality rate
  # Rate: 0.012, 0.011, 0.010, 0.009, 0.008 (linear decline)
  deaths <- c(120, 110, 100, 90, 80)
  population <- c(10000, 10000, 10000, 10000, 10000)
  h <- 0
  m <- "lin_reg"
  t <- TRUE
  bs <- 1
  be <- 3  # Fit trend on first 3

  result <- handleASD(deaths, population, h, m, t, bs, be)

  check_asd_result(result, length(deaths))

  # Expected deaths for period 4 should follow the trend (~90)
  # and period 5 should follow trend (~80)
  expect_true(result$asd_bl[4] < result$asd_bl[3])
  expect_true(result$asd_bl[5] < result$asd_bl[4])
})

test_that("handleASD prediction intervals are valid", {
  deaths <- c(1000, 1050, 1100, 1080, 1120, 1150, 1200)
  population <- c(100000, 100000, 100000, 100000, 100000, 100000, 100000)
  h <- 0
  m <- "mean"
  t <- FALSE
  bs <- 1
  be <- 4

  result <- handleASD(deaths, population, h, m, t, bs, be)

  # PI should be NA for baseline period
  expect_true(all(is.na(result$lower[1:4])))
  expect_true(all(is.na(result$upper[1:4])))

  # PI should exist for post-baseline
  expect_true(all(!is.na(result$lower[5:7])))
  expect_true(all(!is.na(result$upper[5:7])))

  # Lower should be less than upper
  for (i in 5:7) {
    expect_true(result$lower[i] < result$upper[i])
  }

  # asd_bl should be within PI bounds
  for (i in 5:7) {
    expect_true(result$asd_bl[i] >= result$lower[i])
    expect_true(result$asd_bl[i] <= result$upper[i])
  }
})

test_that("handleASD handles leading NAs correctly", {
  deaths <- c(NA, NA, 1000, 1050, 1100, 1150, 1200)
  population <- c(100000, 100000, 100000, 100000, 100000, 100000, 100000)
  h <- 0
  m <- "mean"
  t <- FALSE

  result <- handleASD(deaths, population, h, m, t)

  check_asd_result(result, length(deaths))

  # Leading NAs should be preserved
  expect_true(is.na(result$asd_bl[1]))
  expect_true(is.na(result$asd_bl[2]))

  # Non-NA values should be calculated
  expect_true(all(!is.na(result$asd_bl[3:7])))
})

test_that("handleASD rounds output correctly", {
  deaths <- c(1000.123, 1050.456, 1100.789, 1080.111, 1120.222)
  population <- c(100000.5, 100100.5, 100200.5, 100300.5, 100400.5)
  h <- 0
  m <- "mean"
  t <- FALSE

  result <- handleASD(deaths, population, h, m, t)

  # asd and asd_bl should be rounded to 2 decimals
  for (val in result$asd) {
    if (!is.na(val)) {
      val_str <- as.character(val)
      if (grepl("\\.", val_str)) {
        decimals <- nchar(strsplit(val_str, "\\.")[[1]][2])
        expect_true(decimals <= 2)
      }
    }
  }

  # zscore should be rounded to 3 decimals
  for (z in result$zscore) {
    if (!is.na(z)) {
      z_str <- as.character(z)
      if (grepl("\\.", z_str)) {
        decimals <- nchar(strsplit(z_str, "\\.")[[1]][2])
        expect_true(decimals <= 3)
      }
    }
  }
})

test_that("handleASD errors on invalid method", {
  deaths <- c(1000, 1050, 1100, 1080, 1120)
  population <- c(100000, 100000, 100000, 100000, 100000)

  expect_error(
    handleASD(deaths, population, 0, "invalid_method", FALSE),
    "Unknown method for ASD"
  )
})

message("\nHandler tests completed!")
