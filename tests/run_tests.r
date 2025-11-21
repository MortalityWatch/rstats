#!/usr/bin/env Rscript
# Test runner for R stats microservice

library(testthat)

# Run all tests
cat("Running unit tests...\n\n")

# Test utils
cat("=== Testing utility functions ===\n")
test_file("test_utils.r", reporter = "progress")

# Test validation and server functions
cat("\n=== Testing validation and caching ===\n")
test_file("test_validation.r", reporter = "progress")

# Test handler functions
cat("\n=== Testing forecast handlers ===\n")
test_file("test_handlers.r", reporter = "progress")

cat("\n✓ All tests completed\n")
