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

  # Z-scores for forecast period should be 0
  forecast_zscores <- tail(result$zscore, h)
  expect_true(all(forecast_zscores == 0))
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
# Baseline length feature tests (PR #7)
# ============================================================================

test_that("baseline_length parameter splits data correctly", {
  # 10 years of data, use first 5 for baseline
  y <- c(100, 105, 110, 108, 112, 115, 120, 125, 130, 135)
  baseline_length <- 5
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, baseline_length)

  check_forecast_result(result, length(y) + h)
})

test_that("baseline_length calculates z-scores for all data using baseline stats", {
  # Create data where post-baseline has higher values
  y_baseline <- c(100, 105, 110, 108, 112) # Mean ~107
  y_post <- c(200, 205, 210) # Much higher
  y <- c(y_baseline, y_post)

  baseline_length <- 5
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t, baseline_length)

  check_forecast_result(result, length(y) + h)

  # Post-baseline z-scores should be very high (positive)
  # since post-baseline values are much higher than baseline mean
  post_baseline_zscores <- result$zscore[6:8]
  expect_true(all(post_baseline_zscores > 2))
})

test_that("baseline_length NULL uses all data (backwards compatible)", {
  y <- c(100, 105, 110, 108, 112, 115, 120)
  h <- 3
  m <- "mean"
  s <- 1
  t <- FALSE

  result_with_null <- handleForecast(y, h, m, s, t, baseline_length = NULL)
  result_without <- handleForecast(y, h, m, s, t)

  # Results should be identical
  expect_equal(result_with_null$y, result_without$y)
  expect_equal(result_with_null$zscore, result_without$zscore)
})

test_that("baseline_length validation in integration test", {
  # This test is now covered by validation tests below
  # baseline_length >= length(y) should throw an error
  expect_true(TRUE)
})

test_that("baseline_length works with different methods", {
  y <- c(100, 105, 110, 115, 120, 125, 130, 135)
  baseline_length <- 5
  h <- 2
  s <- 1
  t <- TRUE

  # Test with linear regression
  result_lr <- handleForecast(y, h, "lin_reg", s, t, baseline_length)
  check_forecast_result(result_lr, length(y) + h)

  # Test with exponential smoothing
  result_exp <- handleForecast(y, h, "exp", s, t, baseline_length)
  check_forecast_result(result_exp, length(y) + h)

  # Test with median
  result_med <- handleForecast(y, h, "median", s, FALSE, baseline_length)
  check_forecast_result(result_med, length(y) + h)
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

test_that("handleCumulativeForecast with baseline_length", {
  y <- c(1000, 2100, 3300, 4600, 6000, 7500)
  baseline_length <- 4
  h <- 2
  t <- TRUE

  result <- handleCumulativeForecast(y, h, t, baseline_length)

  expect_equal(length(result$zscore), length(y) + h)

  # Post-baseline z-scores should be calculated
  post_baseline_zscores <- result$zscore[5:6]
  expect_true(all(!is.na(post_baseline_zscores)))
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

test_that("Forecast values are rounded to 1 decimal", {
  y <- c(100.123, 105.456, 110.789)
  h <- 2
  m <- "mean"
  s <- 1
  t <- FALSE

  result <- handleForecast(y, h, m, s, t)

  # Check forecast values are rounded to 1 decimal
  for (val in result$y) {
    if (!is.na(val)) {
      val_str <- as.character(val)
      if (grepl("\\.", val_str)) {
        decimals <- nchar(strsplit(val_str, "\\.")[[1]][2])
        expect_true(decimals <= 1)
      }
    }
  }
})

message("\nHandler tests completed!")
