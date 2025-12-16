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

  # Should return incremental values (uncumulated)
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
# handleLifeTable() tests
# ============================================================================

# Helper to check life table result structure
check_life_table_result <- function(result, n_periods = 1, n_ages = NULL) {
  expect_true("ages" %in% names(result))
  expect_true("trend" %in% names(result))
  expect_true("seasonal" %in% names(result))
  expect_true("adjusted" %in% names(result))

  if (n_periods == 1) {
    # Single period returns ex array for all ages
    expect_true("ex" %in% names(result))
    if (!is.null(n_ages)) {
      expect_equal(length(result$ex), n_ages)
    }
  } else {
    # Multiple periods returns e0 series
    expect_true("e0" %in% names(result))
    expect_equal(length(result$e0), n_periods)
  }
}

test_that("handleLifeTable calculates ex for single period", {
  # Simplified age groups matching typical data
  ages <- c(0, 15, 65, 85)

  # Realistic-ish deaths and population
  deaths <- c(500, 1000, 5000, 10000)
  population <- c(1000000, 3000000, 800000, 200000)

  result <- handleLifeTable(deaths, population, ages, period = "yearly")

  check_life_table_result(result, 1, n_ages = 4)

  # ex[1] is e0, should be a reasonable value
  expect_true(result$ex[1] > 50 && result$ex[1] < 105)

  # Should return ex for all ages

  expect_equal(length(result$ex), length(ages))
  expect_equal(result$ages, ages)

  # No STL for single period
  expect_null(result$trend)
  expect_null(result$seasonal)
})

test_that("handleLifeTable works with different age group formats", {
  # 10-year age groups (eurostat style)
  ages_10yr <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
  deaths_10yr <- c(200, 50, 100, 150, 300, 800, 2000, 4000, 8000)
  pop_10yr <- c(500000, 500000, 600000, 600000, 550000, 500000, 400000, 300000, 150000)

  result_10yr <- handleLifeTable(deaths_10yr, pop_10yr, ages_10yr)
  check_life_table_result(result_10yr, 1, n_ages = 9)
  expect_true(result_10yr$ex[1] > 50 && result_10yr$ex[1] < 95)

  # 5-year age groups (mortality.org style)
  ages_5yr <- c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85)
  deaths_5yr <- c(150, 30, 50, 100, 150, 400, 1000, 2500, 4000, 8000)
  pop_5yr <- c(300000, 300000, 600000, 700000, 650000, 600000, 550000, 400000, 250000, 100000)

  result_5yr <- handleLifeTable(deaths_5yr, pop_5yr, ages_5yr)
  check_life_table_result(result_5yr, 1, n_ages = 10)
  expect_true(result_5yr$ex[1] > 50 && result_5yr$ex[1] < 95)
})

test_that("handleLifeTable returns ex at specific ages", {
  ages <- c(0, 15, 45, 65, 75, 85)
  deaths <- c(400, 500, 1500, 3000, 5000, 10000)
  population <- c(800000, 2500000, 2000000, 600000, 400000, 150000)

  result <- handleLifeTable(deaths, population, ages)

  check_life_table_result(result, 1, n_ages = 6)

  # Find e65 from the ex array
  e65_idx <- which(result$ages == 65)
  e65 <- result$ex[e65_idx]

  # e65 should be around 15-25 years for typical mortality
  expect_true(!is.na(e65))
  expect_true(e65 > 5 && e65 < 40)

  # ex should decrease with age (generally)
  expect_true(result$ex[1] > result$ex[e65_idx])
})

test_that("handleLifeTable handles multiple periods as list", {
  ages <- c(0, 15, 65, 85)
  n_periods <- 6

  # Create list of deaths/population for 6 periods
  deaths_list <- lapply(1:n_periods, function(i) c(500, 1000, 5000, 10000) * (1 + (i - 1) * 0.02))
  pop_list <- lapply(1:n_periods, function(i) c(1000000, 3000000, 800000, 200000))

  result <- handleLifeTable(deaths_list, pop_list, ages, period = "yearly")

  check_life_table_result(result, n_periods)

  # All e0 values should be reasonable
  expect_true(all(result$e0 > 50 & result$e0 < 105))

  # No STL for yearly data
  expect_null(result$trend)
})

test_that("handleLifeTable applies STL for monthly data with sufficient periods", {
  ages <- c(0, 15, 65, 85)
  n_periods <- 36  # 3 years of monthly data

  # Create monthly data with some seasonal variation
  set.seed(123)
  base_deaths <- c(500, 1000, 5000, 10000)
  deaths_list <- lapply(1:n_periods, function(i) {
    # Add seasonal variation (higher mortality in winter months)
    seasonal_factor <- 1 + 0.1 * sin(2 * pi * i / 12)
    base_deaths * seasonal_factor + rnorm(4, 0, 10)
  })
  pop_list <- lapply(1:n_periods, function(i) c(1000000, 3000000, 800000, 200000))

  result <- handleLifeTable(deaths_list, pop_list, ages, period = "monthly")

  check_life_table_result(result, n_periods)

  # STL components should be present for monthly data with 3 years
  expect_true(!is.null(result$trend))
  expect_true(!is.null(result$seasonal))
  expect_true(!is.null(result$adjusted))

  # Trend should have same length as e0
  expect_equal(length(result$trend), n_periods)
  expect_equal(length(result$seasonal), n_periods)
})

test_that("handleLifeTable handles sex parameter", {
  ages <- c(0, 15, 65, 85)
  deaths <- c(500, 1000, 5000, 10000)
  population <- c(1000000, 3000000, 800000, 200000)

  result_m <- handleLifeTable(deaths, population, ages, sex = "m")
  result_f <- handleLifeTable(deaths, population, ages, sex = "f")
  result_t <- handleLifeTable(deaths, population, ages, sex = "t")

  # All should produce valid results (ex[1] is e0)
  expect_true(result_m$ex[1] > 50 && result_m$ex[1] < 105)
  expect_true(result_f$ex[1] > 50 && result_f$ex[1] < 105)
  expect_true(result_t$ex[1] > 50 && result_t$ex[1] < 105)
})

test_that("fallback_life_table produces reasonable results", {
  # Test the fallback directly
  ages <- c(0, 1, 5, 15, 45, 65, 75, 85)
  nMx <- c(0.005, 0.0005, 0.0002, 0.001, 0.005, 0.02, 0.05, 0.15)
  AgeInt <- c(1, 4, 10, 30, 20, 10, 10, NA)

  result <- fallback_life_table(nMx, ages, AgeInt)

  # Check structure
  expect_true("ex" %in% names(result))
  expect_true("lx" %in% names(result))

  # e0 should be reasonable
  e0 <- result$ex[1]
  expect_true(e0 > 50 && e0 < 95)

  # lx should start at 100000 and decrease
  expect_equal(result$lx[1], 100000)
  expect_true(all(diff(result$lx) <= 0))
})

message("\nHandler tests completed!")
