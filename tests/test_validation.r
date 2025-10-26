# Unit tests for request validation and server functions
library(testthat)

# Source the serve file to get validation functions
# Note: This will load but not start the server
source("../src/serve.r", local = TRUE)

test_that("validate_request accepts valid standard forecast request", {
  query <- list(
    y = "10,20,30,40,50",
    h = "5",
    s = "1",
    m = "mean",
    t = "0"
  )

  result <- validate_request(query, "/")

  expect_true(result$valid)
})

test_that("validate_request accepts valid cumulative forecast request", {
  query <- list(
    y = "100,200,300,400",
    h = "3",
    t = "1"
  )

  result <- validate_request(query, "/cum")

  expect_true(result$valid)
})

test_that("validate_request rejects missing y parameter", {
  query <- list(h = "5", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "Missing required parameter 'y'")
})

test_that("validate_request rejects non-numeric y parameter", {
  query <- list(y = "abc,def,ghi", h = "5", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "comma-separated numeric values")
})

test_that("validate_request rejects insufficient data points", {
  query <- list(y = "10,20", h = "5", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "at least 3 valid data points")
})

test_that("validate_request rejects oversized array", {
  large_array <- paste(rep("10", 10001), collapse = ",")
  query <- list(y = large_array, h = "5", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "exceeds maximum length")
})

test_that("validate_request rejects invalid horizon", {
  query <- list(y = "10,20,30,40", h = "0", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "positive integer")
})

test_that("validate_request rejects horizon over 1000", {
  query <- list(y = "10,20,30,40", h = "1001", s = "1", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
})

test_that("validate_request rejects invalid seasonality", {
  query <- list(y = "10,20,30,40", h = "5", s = "5", m = "mean")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "must be 1 \\(year\\), 2 \\(quarter\\), 3 \\(month\\), or 4 \\(week\\)")
})

test_that("validate_request rejects invalid method", {
  query <- list(y = "10,20,30,40", h = "5", s = "1", m = "invalid")

  result <- validate_request(query, "/")

  expect_false(result$valid)
  expect_equal(result$status, 400)
  expect_match(result$message, "naive, mean, lin_reg, exp")
})

test_that("generate_cache_key creates consistent keys", {
  query1 <- list(y = "10,20,30", h = "5", s = "1", m = "mean")
  query2 <- list(m = "mean", s = "1", h = "5", y = "10,20,30") # Different order

  key1 <- generate_cache_key("/", query1)
  key2 <- generate_cache_key("/", query2)

  expect_equal(key1, key2)
})

test_that("generate_cache_key creates different keys for different params", {
  query1 <- list(y = "10,20,30", h = "5", s = "1", m = "mean")
  query2 <- list(y = "10,20,30", h = "10", s = "1", m = "mean")

  key1 <- generate_cache_key("/", query1)
  key2 <- generate_cache_key("/", query2)

  expect_false(key1 == key2)
})

test_that("cache stores and retrieves responses", {
  # Clear cache
  rm(list = ls(envir = cache_store), envir = cache_store)

  cache_key <- "test_key"
  response <- list(y = c(1, 2, 3), lower = c(0, 1, 2), upper = c(2, 3, 4))

  # Store
  set_cached_response(cache_key, response)

  # Retrieve
  cached <- get_cached_response(cache_key)

  expect_equal(cached, response)
})

test_that("cache returns NULL for non-existent keys", {
  cached <- get_cached_response("nonexistent_key_12345")
  expect_null(cached)
})

test_that("rate limiting allows requests under limit", {
  # Clear rate limit store
  rm(list = ls(envir = rate_limit_store), envir = rate_limit_store)

  ip <- "192.168.1.100"

  # First request should be allowed
  result <- check_rate_limit(ip)
  expect_true(result)

  # Second request should be allowed
  result <- check_rate_limit(ip)
  expect_true(result)
})

test_that("rate limiting blocks requests over limit", {
  # Clear rate limit store
  rm(list = ls(envir = rate_limit_store), envir = rate_limit_store)

  ip <- "192.168.1.101"

  # Manually set to max
  rate_limit_store[[ip]] <- list(count = RATE_LIMIT_MAX_REQUESTS, window_start = as.numeric(Sys.time()))

  # Next request should be blocked
  result <- check_rate_limit(ip)
  expect_false(result)
})

test_that("rate limiting resets after window expires", {
  # Clear rate limit store
  rm(list = ls(envir = rate_limit_store), envir = rate_limit_store)

  ip <- "192.168.1.102"

  # Set to max but with expired window
  rate_limit_store[[ip]] <- list(
    count = RATE_LIMIT_MAX_REQUESTS,
    window_start = as.numeric(Sys.time()) - RATE_LIMIT_WINDOW - 1
  )

  # Should be allowed (window reset)
  result <- check_rate_limit(ip)
  expect_true(result)

  # Verify count was reset
  expect_equal(rate_limit_store[[ip]]$count, 1)
})

# Run all tests
test_dir(".")
