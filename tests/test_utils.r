# Unit tests for utility functions
library(testthat)

# Source the utils file
source("../src/utils.r")

test_that("uncumulate converts cumulative to incremental values", {
  cumulative <- c(10, 25, 45, 70)
  expected <- c(10, 15, 20, 25)

  result <- uncumulate(cumulative)

  expect_equal(result, expected)
})

test_that("uncumulate handles empty vector", {
  result <- uncumulate(numeric(0))
  expect_equal(result, numeric(0))
})

test_that("uncumulate handles single value", {
  result <- uncumulate(c(42))
  expect_equal(result, c(42))
})

test_that("null-coalescing operator returns first value when not NULL", {
  result <- "foo" %||% "bar"
  expect_equal(result, "foo")
})

test_that("null-coalescing operator returns second value when first is NULL", {
  result <- NULL %||% "bar"
  expect_equal(result, "bar")
})

test_that("lm_predict works with simple linear model", {
  # Create simple training data
  train_data <- data.frame(x = 1:10, y = 2 * (1:10) + 3)
  model <- lm(y ~ x, data = train_data)

  # Predict for new data
  new_data <- data.frame(x = 11:15)
  result <- lm_predict(model, new_data, diag = TRUE)

  # Check structure
  expect_true("fit" %in% names(result))
  expect_true("var.fit" %in% names(result))
  expect_true("df" %in% names(result))
  expect_true("residual.var" %in% names(result))

  # Check predictions are reasonable (slope = 2, intercept = 3)
  expected_fit <- 2 * (11:15) + 3
  expect_equal(result$fit, expected_fit, tolerance = 0.01)
})

test_that("lm_predict rejects non-lm objects", {
  expect_error(
    lm_predict(list(), data.frame(x = 1)),
    "'lmObject' is not a valid 'lm' object!"
  )
})

test_that("agg_pred computes aggregated predictions", {
  # Simple mock prediction object
  pred_obj <- list(
    fit = c(10, 20, 30),
    var.fit = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3),
    df = 10,
    residual.var = 2
  )

  weights <- c(1, 1, 1)
  result <- agg_pred(weights, pred_obj, alpha = 0.95)

  # Check structure
  expect_true("mean" %in% names(result))
  expect_true("var" %in% names(result))
  expect_true("CI" %in% names(result))
  expect_true("PI" %in% names(result))

  # Check mean is sum of fits
  expect_equal(result$mean, sum(pred_obj$fit))
})

test_that("agg_pred rejects mismatched weight length", {
  pred_obj <- list(
    fit = c(10, 20, 30),
    var.fit = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3),
    df = 10,
    residual.var = 2
  )

  expect_error(
    agg_pred(c(1, 1), pred_obj),
    "'w' has wrong length!"
  )
})

test_that("agg_pred requires variance-covariance matrix", {
  pred_obj <- list(
    fit = c(10, 20, 30),
    var.fit = c(1, 1, 1), # Not a matrix
    df = 10,
    residual.var = 2
  )

  expect_error(
    agg_pred(c(1, 1, 1), pred_obj),
    "'predObject' has no variance-covariance matrix!"
  )
})

# Run all tests
test_dir(".")
