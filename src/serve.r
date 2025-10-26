# Main Server
# HTTP server for statistical forecasting API

# Load required libraries
library(tibble)
library(fable)
library(tidyverse)
library(fiery)
library(tsibble)

# Load utility functions and handlers
source("utils.r")
source("handlers.r")

# Server configuration
port <- ifelse(Sys.getenv("PORT") != "", Sys.getenv("PORT"), "5000")
app <- Fire$new(host = "0.0.0.0", port = as.integer(port))

# CORS configuration
ALLOWED_ORIGINS <- strsplit(
  Sys.getenv("ALLOWED_ORIGINS", "https://www.mortality.watch,https://mortality.watch"),
  ","
)[[1]]

# Rate limiting state
rate_limit_store <- new.env()
RATE_LIMIT_WINDOW <- 60 # seconds
RATE_LIMIT_MAX_REQUESTS <- 100 # max requests per window

# Response cache
cache_store <- new.env()
CACHE_TTL <- 3600 # 1 hour in seconds

#' Generate cache key from request parameters
#'
#' @param path Request path
#' @param query Query parameters
#' @return String cache key
generate_cache_key <- function(path, query) {
  # Sort query parameters for consistent keys
  sorted_params <- query[order(names(query))]
  param_str <- paste(sprintf("%s=%s", names(sorted_params), unlist(sorted_params)), collapse = "&")
  paste0(path, "?", param_str)
}

#' Get cached response if valid
#'
#' @param cache_key Cache key string
#' @return Cached response or NULL if not found/expired
get_cached_response <- function(cache_key) {
  if (!exists(cache_key, envir = cache_store)) {
    return(NULL)
  }

  cache_entry <- cache_store[[cache_key]]
  current_time <- as.numeric(Sys.time())

  if (current_time - cache_entry$timestamp > CACHE_TTL) {
    # Cache expired
    rm(list = cache_key, envir = cache_store)
    return(NULL)
  }

  return(cache_entry$response)
}

#' Store response in cache
#'
#' @param cache_key Cache key string
#' @param response Response object to cache
set_cached_response <- function(cache_key, response) {
  cache_store[[cache_key]] <- list(
    response = response,
    timestamp = as.numeric(Sys.time())
  )
}

#' Check rate limit for IP address
#'
#' @param ip IP address string
#' @return TRUE if under limit, FALSE if over limit
check_rate_limit <- function(ip) {
  current_time <- as.numeric(Sys.time())

  if (!exists(ip, envir = rate_limit_store)) {
    rate_limit_store[[ip]] <- list(count = 1, window_start = current_time)
    return(TRUE)
  }

  ip_data <- rate_limit_store[[ip]]
  time_elapsed <- current_time - ip_data$window_start

  if (time_elapsed > RATE_LIMIT_WINDOW) {
    # Reset window
    rate_limit_store[[ip]] <- list(count = 1, window_start = current_time)
    return(TRUE)
  }

  if (ip_data$count >= RATE_LIMIT_MAX_REQUESTS) {
    return(FALSE)
  }

  # Increment counter
  rate_limit_store[[ip]]$count <- ip_data$count + 1
  return(TRUE)
}

#' Log request with structured format
#'
#' @param level Log level: INFO, WARN, ERROR
#' @param message Log message
#' @param details Optional list of additional details
log_message <- function(level, message, details = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, message)

  if (!is.null(details)) {
    detail_str <- paste(sprintf("%s=%s", names(details), unlist(details)), collapse = ", ")
    log_entry <- paste(log_entry, "-", detail_str)
  }

  print(log_entry)
}

#' Validate request parameters
#'
#' @param query Query parameters from request
#' @param path Request path
#' @return List with valid=TRUE/FALSE and error message if invalid
validate_request <- function(query, path) {
  # Check for required 'y' parameter
  if (is.null(query$y)) {
    return(list(valid = FALSE, status = 400, message = "Missing required parameter 'y'"))
  }

  # Validate 'y' can be parsed as numeric
  y <- tryCatch(
    as.double(strsplit(as.character(query$y), ",")[[1]]),
    error = function(e) NULL
  )

  if (is.null(y)) {
    return(list(valid = FALSE, status = 400, message = "Parameter 'y' must be comma-separated numeric values"))
  }

  # Check minimum data points
  valid_y <- y[!is.na(y)]
  if (length(valid_y) < 3) {
    return(list(valid = FALSE, status = 400, message = "Parameter 'y' must contain at least 3 valid data points"))
  }

  # Check maximum array size (prevent DOS)
  if (length(y) > 10000) {
    return(list(valid = FALSE, status = 400, message = "Parameter 'y' exceeds maximum length of 10000"))
  }

  # Validate 'h' (horizon)
  h <- as.integer(query$h %||% 1)
  if (is.na(h) || h < 1 || h > 1000) {
    return(list(valid = FALSE, status = 400, message = "Parameter 'h' must be a positive integer between 1 and 1000"))
  }

  # Path-specific validation
  if (path == "/") {
    # Validate 's' (seasonality)
    s <- as.integer(query$s)
    if (is.na(s) || !s %in% c(1, 2, 3, 4)) {
      return(list(valid = FALSE, status = 400, message = "Parameter 's' must be 1 (year), 2 (quarter), 3 (month), or 4 (week)"))
    }

    # Validate 'm' (method)
    m <- query$m
    if (is.null(m) || !m %in% c("naive", "mean", "lin_reg", "exp")) {
      return(list(valid = FALSE, status = 400, message = "Parameter 'm' must be one of: naive, mean, lin_reg, exp"))
    }
  }

  return(list(valid = TRUE))
}

#' Check if origin is allowed
#'
#' @param origin Origin header from request
#' @return TRUE if allowed, FALSE otherwise
is_origin_allowed <- function(origin) {
  if (is.null(origin) || origin == "") {
    # Allow requests with no origin (e.g., curl, same-origin)
    return(TRUE)
  }

  origin %in% ALLOWED_ORIGINS
}

#' Set CORS headers on response if origin is allowed
#'
#' @param response Response object
#' @param origin Origin header from request
set_cors_headers <- function(response, origin) {
  if (is.null(origin) || origin == "") {
    return()
  }

  if (is_origin_allowed(origin)) {
    response$headers$set("Access-Control-Allow-Origin", origin)
    response$headers$set("Access-Control-Allow-Methods", "GET, OPTIONS")
    response$headers$set("Access-Control-Allow-Headers", "Content-Type")
    response$headers$set("Access-Control-Max-Age", "3600")
  }
}

#' Send error response
#'
#' @param server Fiery server object
#' @param request Request object (for CORS)
#' @param status HTTP status code
#' @param message Error message
send_error <- function(server, request, status, message) {
  response <- request$respond()
  response$body <- jsonlite::toJSON(list(error = message, status = status), auto_unbox = TRUE)
  response$status <- status
  response$type <- "json"

  # Set CORS headers
  origin <- request$headers$get("Origin")
  set_cors_headers(response, origin)

  return(response)
}

# Main request handler
app$on("request", function(server, request, ...) {
  start_time <- Sys.time()

  # Extract origin for CORS
  origin <- request$headers$get("Origin")

  # Extract client IP (consider X-Forwarded-For for proxied requests)
  client_ip <- request$REMOTE_ADDR %||% "unknown"

  # Log incoming request
  log_message("INFO", "Incoming request", list(
    path = request$path,
    method = request$method,
    ip = client_ip,
    origin = origin %||% "none",
    params = paste(sprintf("%s=%s", names(request$query),
                          sapply(request$query, function(x) {
                            s <- as.character(x)
                            if (nchar(s) > 50) paste0(substr(s, 1, 47), "...") else s
                          })), collapse = ", ")
  ))

  # CORS preflight (OPTIONS) request
  if (request$method == "OPTIONS") {
    if (!is_origin_allowed(origin)) {
      log_message("WARN", "CORS rejected", list(ip = client_ip, origin = origin))
      return(send_error(server, request, 403, "Origin not allowed"))
    }

    response <- request$respond()
    response$status <- 204L  # No Content
    set_cors_headers(response, origin)
    log_message("INFO", "CORS preflight", list(origin = origin))
    return(response)
  }

  # Check origin for non-OPTIONS requests
  if (!is_origin_allowed(origin)) {
    log_message("WARN", "CORS rejected", list(ip = client_ip, origin = origin))
    return(send_error(server, request, 403, "Origin not allowed"))
  }

  # Health check endpoint
  if (request$path == "/health") {
    response <- request$respond()
    response$body <- jsonlite::toJSON(list(
      status = "ok",
      timestamp = Sys.time(),
      version = "1.0.0"
    ), auto_unbox = TRUE)
    response$status <- 200L
    response$type <- "json"
    set_cors_headers(response, origin)

    log_message("INFO", "Health check", list(status = "ok"))
    return(response)
  }

  # Rate limiting
  if (!check_rate_limit(client_ip)) {
    log_message("WARN", "Rate limit exceeded", list(ip = client_ip))
    return(send_error(server, request, 429, "Rate limit exceeded. Maximum 100 requests per minute."))
  }

  # Validate request
  validation <- validate_request(request$query, request$path)
  if (!validation$valid) {
    log_message("WARN", "Validation failed", list(
      ip = client_ip,
      error = validation$message
    ))
    return(send_error(server, request, validation$status, validation$message))
  }

  # Check cache for non-health endpoints
  cache_key <- generate_cache_key(request$path, request$query)
  cached_result <- get_cached_response(cache_key)

  if (!is.null(cached_result)) {
    log_message("INFO", "Cache hit", list(
      path = request$path,
      cache_key = substr(cache_key, 1, 100)
    ))

    response <- request$respond()
    response$body <- jsonlite::toJSON(cached_result, auto_unbox = TRUE)
    response$status <- 200L
    response$type <- "json"
    response$headers$set("X-Cache", "HIT")
    set_cors_headers(response, origin)
    return(response)
  }

  # Parse parameters
  y <- as.double(strsplit(as.character(request$query$y), ",")[[1]])
  h <- as.integer(request$query$h %||% 1)
  t <- as.integer(request$query$t %||% 0) == 1

  # Process request with error handling
  res <- tryCatch({
    if (request$path == "/") {
      m <- request$query$m
      s <- as.integer(request$query$s)
      handleForecast(y, h, m, s, t)
    } else if (request$path == "/cum") {
      handleCumulativeForecast(y, h, t)
    } else {
      log_message("WARN", "Route not found", list(path = request$path, ip = client_ip))
      return(send_error(server, request, 404, "Route not found. Available routes: /, /cum, /health"))
    }
  }, error = function(e) {
    log_message("ERROR", "Processing failed", list(
      ip = client_ip,
      path = request$path,
      error = as.character(e)
    ))
    return(send_error(server, request, 500, paste("Internal server error:", as.character(e))))
  })

  # Check if error occurred (send_error returns a response object, not NULL)
  if (inherits(res, "response")) {
    return(res)
  }

  # Store result in cache
  set_cached_response(cache_key, res)

  # Send successful response
  response <- request$respond()
  response$body <- jsonlite::toJSON(res, auto_unbox = TRUE)
  response$status <- 200L
  response$type <- "json"
  response$headers$set("X-Cache", "MISS")
  set_cors_headers(response, origin)

  # Log completion
  duration_ms <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000, 2)
  log_message("INFO", "Request completed", list(
    path = request$path,
    ip = client_ip,
    duration_ms = duration_ms,
    status = 200,
    cached = "yes"
  ))

  response
})

# Start server
log_message("INFO", "Starting server", list(port = port))
app$ignite(showcase = FALSE)
