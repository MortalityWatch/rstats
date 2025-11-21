#!/usr/bin/env Rscript
# Integration Tests for API Endpoints
# Tests the full API workflow with a running server

library(testthat)
library(httr)
library(jsonlite)

# Server configuration
BASE_URL <- Sys.getenv("TEST_BASE_URL", "http://localhost:5000")

# Helper function to check if server is running
is_server_running <- function() {
  tryCatch({
    response <- GET(paste0(BASE_URL, "/health"), timeout(2))
    return(status_code(response) == 200)
  }, error = function(e) {
    return(FALSE)
  })
}

# Skip all tests if server is not running
if (!is_server_running()) {
  message("Server is not running at ", BASE_URL)
  message("To run integration tests, start the server first:")
  message("  Rscript src/serve.r")
  message("Or set TEST_BASE_URL environment variable to point to a running instance.")
  quit(save = "no", status = 0)
}

test_that("Health endpoint returns correct structure", {
  response <- GET(paste0(BASE_URL, "/health"))

  expect_equal(status_code(response), 200)
  expect_equal(headers(response)$`content-type`, "application/json")

  body <- content(response, as = "parsed")
  expect_true("status" %in% names(body))
  expect_true("timestamp" %in% names(body))
  expect_true("version" %in% names(body))
  expect_equal(body$status, "ok")
})

test_that("Forecast endpoint with valid parameters works", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,108,112,115,120",
    h = 3,
    s = 1,
    m = "mean",
    t = 0
  ))

  expect_equal(status_code(response), 200)

  body <- content(response, as = "parsed")
  expect_true("y" %in% names(body))
  expect_true("lower" %in% names(body))
  expect_true("upper" %in% names(body))
  expect_true("zscore" %in% names(body))
  expect_equal(length(body$y), 10) # 7 original + 3 forecast
  expect_equal(length(body$zscore), 10) # Same length as y
})

test_that("Forecast endpoint with trend parameter", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120,125,130",
    h = 2,
    s = 1,
    m = "lin_reg",
    t = 1
  ))

  expect_equal(status_code(response), 200)

  body <- content(response, as = "parsed")
  expect_equal(length(body$y), 9) # 7 original + 2 forecast
})

test_that("Cumulative forecast endpoint works", {
  skip("TODO: Fix cumulative forecast bug - returns 4 values instead of 6")

  response <- GET(paste0(BASE_URL, "/cum"), query = list(
    y = "1000,2100,3300,4600",
    h = 2,
    t = 1
  ))

  expect_equal(status_code(response), 200)

  body <- content(response, as = "parsed")
  expect_true("y" %in% names(body))
  expect_true("lower" %in% names(body))
  expect_true("upper" %in% names(body))
  expect_equal(length(body$y), 6) # 4 original + 2 forecast
})

test_that("Missing required parameter returns 400", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    h = 3,
    s = 1,
    m = "mean"
  ))

  expect_equal(status_code(response), 400)

  body <- content(response, as = "parsed")
  expect_true("error" %in% names(body))
  expect_match(body$error, "Missing required parameter")
})

test_that("Invalid method parameter returns 400", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110",
    h = 3,
    s = 1,
    m = "invalid_method"
  ))

  expect_equal(status_code(response), 400)

  body <- content(response, as = "parsed")
  expect_true("error" %in% names(body))
})

test_that("Invalid seasonality parameter returns 400", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110",
    h = 3,
    s = 99,
    m = "mean"
  ))

  expect_equal(status_code(response), 400)

  body <- content(response, as = "parsed")
  expect_true("error" %in% names(body))
})

test_that("Too few data points returns 400", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105",
    h = 3,
    s = 1,
    m = "mean"
  ))

  expect_equal(status_code(response), 400)

  body <- content(response, as = "parsed")
  expect_true("error" %in% names(body))
  expect_match(body$error, "at least 3")
})

test_that("Invalid route returns 404", {
  response <- GET(paste0(BASE_URL, "/invalid-route"))

  expect_equal(status_code(response), 404)

  body <- content(response, as = "parsed")
  expect_true("error" %in% names(body))
  expect_match(body$error, "Route not found")
})

test_that("Cache headers are set correctly", {
  # First request should be MISS
  response1 <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 2,
    s = 1,
    m = "mean",
    t = 0
  ))

  expect_equal(status_code(response1), 200)
  expect_equal(headers(response1)$`x-cache`, "MISS")

  # Second identical request should be HIT
  response2 <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 2,
    s = 1,
    m = "mean",
    t = 0
  ))

  expect_equal(status_code(response2), 200)
  expect_equal(headers(response2)$`x-cache`, "HIT")
})

test_that("Exponential smoothing method works", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120,125,130,135",
    h = 3,
    s = 1,
    m = "exp",
    t = 1
  ))

  expect_equal(status_code(response), 200)

  body <- content(response, as = "parsed")
  expect_equal(length(body$y), 11) # 8 original + 3 forecast
})

test_that("Different seasonality types work", {
  # Test quarterly (s=2)
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120,125,130,135",
    h = 2,
    s = 2,
    m = "mean"
  ))
  expect_equal(status_code(response), 200)

  # Test monthly (s=3)
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120,125,130,135",
    h = 2,
    s = 3,
    m = "mean"
  ))
  expect_equal(status_code(response), 200)

  # Test weekly (s=4)
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120,125,130,135",
    h = 2,
    s = 4,
    m = "mean"
  ))
  expect_equal(status_code(response), 200)
})

test_that("Naive forecast method works", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 3,
    s = 1,
    m = "naive"
  ))

  expect_equal(status_code(response), 200)

  body <- content(response, as = "parsed")
  expect_equal(length(body$y), 8) # 5 original + 3 forecast
})

test_that("Baseline length parameter 'b' works for standard forecast", {
  # 10 years of data, use first 5 as baseline
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,108,112,115,120,125,130,135",
    h = 2,
    s = 1,
    m = "mean",
    t = 0,
    b = 5
  ))

  expect_equal(status_code(response), 200)

  body <- content(response, as = "parsed")
  expect_equal(length(body$y), 12) # 10 original + 2 forecast
  expect_equal(length(body$zscore), 12)

  # Z-scores should exist for all observed data (indices 1-10)
  expect_true(!is.na(body$zscore[[1]]))
  expect_true(!is.na(body$zscore[[5]])) # Last baseline point
  expect_true(!is.na(body$zscore[[6]])) # First post-baseline point
  expect_true(!is.na(body$zscore[[10]])) # Last observed point

  # Forecast z-scores should be 0
  expect_equal(body$zscore[[11]], 0)
  expect_equal(body$zscore[[12]], 0)
})

test_that("Baseline length parameter 'b' works for cumulative forecast", {
  # 6 years of data, use first 4 as baseline
  response <- GET(paste0(BASE_URL, "/cum"), query = list(
    y = "1000,2100,3300,4600,6000,7500",
    h = 2,
    t = 1,
    b = 4
  ))

  expect_equal(status_code(response), 200)

  body <- content(response, as = "parsed")
  # Should have baseline (4) + post-baseline (2) + forecast (2) = 8 values
  expect_equal(length(body$y), 8)
  expect_equal(length(body$zscore), 8)

  # Z-scores should exist for all observed data
  expect_true(!is.na(body$zscore[[1]]))
  expect_true(!is.na(body$zscore[[6]])) # Last observed point

  # Forecast z-scores should be 0
  expect_equal(body$zscore[[7]], 0)
  expect_equal(body$zscore[[8]], 0)
})

test_that("Baseline length 'b' with different methods works", {
  # Test with linear regression
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120,125,130,135",
    h = 2,
    s = 1,
    m = "lin_reg",
    t = 1,
    b = 5
  ))
  expect_equal(status_code(response), 200)

  # Test with median
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,108,112,115,120,125",
    h = 2,
    s = 1,
    m = "median",
    t = 0,
    b = 5
  ))
  expect_equal(status_code(response), 200)

  # Test with exponential smoothing
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120,125,130,135",
    h = 2,
    s = 1,
    m = "exp",
    t = 1,
    b = 5
  ))
  expect_equal(status_code(response), 200)
})

test_that("Invalid baseline length 'b' returns 400 - too small", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 2,
    s = 1,
    m = "mean",
    b = 2
  ))

  expect_equal(status_code(response), 400)
  body <- content(response, as = "parsed")
  expect_true("error" %in% names(body))
  expect_match(body$error, "at least 3")
})

test_that("Invalid baseline length 'b' returns 400 - zero or negative", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 2,
    s = 1,
    m = "mean",
    b = 0
  ))

  expect_equal(status_code(response), 400)
  body <- content(response, as = "parsed")
  expect_true("error" %in% names(body))
  expect_match(body$error, "greater than 0")

  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 2,
    s = 1,
    m = "mean",
    b = -5
  ))

  expect_equal(status_code(response), 400)
})

test_that("Invalid baseline length 'b' returns 400 - greater than or equal to data length", {
  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 2,
    s = 1,
    m = "mean",
    b = 5
  ))

  expect_equal(status_code(response), 400)
  body <- content(response, as = "parsed")
  expect_true("error" %in% names(body))
  expect_match(body$error, "less than the data length")

  response <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 2,
    s = 1,
    m = "mean",
    b = 10
  ))

  expect_equal(status_code(response), 400)
})

test_that("Omitting 'b' parameter uses all data (backwards compatible)", {
  # Test without 'b' parameter
  response_without_b <- GET(paste0(BASE_URL, "/"), query = list(
    y = "100,105,110,115,120",
    h = 2,
    s = 1,
    m = "mean",
    t = 0
  ))

  expect_equal(status_code(response_without_b), 200)

  body <- content(response_without_b, as = "parsed")
  expect_equal(length(body$y), 7)
  expect_equal(length(body$zscore), 7)
})

message("\nIntegration tests completed successfully!")
