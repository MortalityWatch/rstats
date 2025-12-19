# Main Server
# HTTP server for statistical forecasting API

# Load required libraries
library(tibble)
library(fable)
library(tidyverse)
library(fiery)
library(tsibble)

# Load utility functions and handlers
# Find the src directory
if (file.exists("utils.r")) {
  # Running from src/ directory
  source("utils.r")
  source("handlers.r")
  source("sentry.r")
} else if (file.exists("src/utils.r")) {
  # Running from project root
  source("src/utils.r")
  source("src/handlers.r")
  source("src/sentry.r")
} else if (file.exists("../src/utils.r")) {
  # Being sourced from tests/
  source("../src/utils.r")
  source("../src/handlers.r")
  source("../src/sentry.r")
} else {
  stop("Cannot find utils.r - make sure you're running from the correct directory")
}

# Initialize Sentry error tracking
init_sentry()

# Server configuration
port <- ifelse(Sys.getenv("PORT") != "", Sys.getenv("PORT"), "5000")
app <- Fire$new(host = "0.0.0.0", port = as.integer(port))

# Rate limiting state
rate_limit_store <- new.env()
RATE_LIMIT_WINDOW <- as.integer(Sys.getenv("RATE_LIMIT_WINDOW", "60")) # seconds
RATE_LIMIT_MAX_REQUESTS <- as.integer(Sys.getenv("RATE_LIMIT_MAX_REQUESTS", "1000")) # max requests per window

# Response cache
cache_store <- new.env()
CACHE_TTL <- 3600 # 1 hour in seconds

#' Generate cache key from request parameters
#'
#' @param path Request path
#' @param query Query parameters (may include arrays from POST body)
#' @return String cache key
generate_cache_key <- function(path, query) {
  # Sort query parameters for consistent keys
  sorted_params <- query[order(names(query))]
  # Handle NULL values and arrays (convert arrays to comma-separated strings)
  param_values <- sapply(sorted_params, function(x) {
    if (is.null(x)) {
      ""
    } else if (is.list(x) || is.numeric(x)) {
      paste(as.character(unlist(x)), collapse = ",")
    } else {
      as.character(x)
    }
  })
  param_str <- paste(sprintf("%s=%s", names(sorted_params), param_values), collapse = "&")
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
#' @param query Query parameters from request (may include POST body params)
#' @param path Request path
#' @return List with valid=TRUE/FALSE and error message if invalid
validate_request <- function(query, path) {
  # Check for required 'y' parameter
  if (is.null(query$y)) {
    return(list(valid = FALSE, status = 400, message = "Missing required parameter 'y'"))
  }

  # Validate 'y' can be parsed as numeric
  # Handle both comma-separated string (GET) and numeric array (POST)
  y_param <- query$y
  y <- tryCatch({
    if (is.character(y_param)) {
      as.double(strsplit(as.character(y_param), ",")[[1]])
    } else if (is.list(y_param) || is.numeric(y_param)) {
      as.double(unlist(y_param))
    } else {
      as.double(y_param)
    }
  }, error = function(e) NULL)

  if (is.null(y)) {
    return(list(
      valid = FALSE, status = 400,
      message = "Parameter 'y' must be comma-separated numeric values or a numeric array"
    ))
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

  # Validate baseline parameters: bs/be (new) or b (legacy)
  if (!is.null(query$bs) || !is.null(query$be)) {
    # New bs/be parameter mode
    if (is.null(query$bs) || is.null(query$be)) {
      return(list(
        valid = FALSE,
        status = 400,
        message = "Parameters 'bs' and 'be' must both be provided together"
      ))
    }

    bs <- tryCatch(as.integer(query$bs), error = function(e) NA)
    be <- tryCatch(as.integer(query$be), error = function(e) NA)

    if (is.na(bs)) {
      return(list(valid = FALSE, status = 400, message = "Parameter 'bs' must be an integer"))
    }
    if (is.na(be)) {
      return(list(valid = FALSE, status = 400, message = "Parameter 'be' must be an integer"))
    }
    if (bs < 1) {
      return(list(valid = FALSE, status = 400, message = "Parameter 'bs' must be >= 1"))
    }
    if (be <= bs) {
      return(list(valid = FALSE, status = 400, message = "Parameter 'be' must be > 'bs'"))
    }
    if (be > length(y)) {
      msg <- paste0(
        "Parameter 'be' (", be, ") must be <= data length (", length(y), ")"
      )
      return(list(valid = FALSE, status = 400, message = msg))
    }

    # Validate that baseline period has sufficient non-NA values
    baseline_data <- y[bs:be]
    non_na_count <- sum(!is.na(baseline_data))
    if (non_na_count < 3) {
      msg <- paste0(
        "Baseline period (y[", bs, ":", be, "]) contains only ", non_na_count,
        " non-NA values. At least 3 non-NA values are required."
      )
      return(list(valid = FALSE, status = 400, message = msg))
    }
  } else if (!is.null(query$b)) {
    # Legacy 'b' parameter mode (backwards compatibility)
    b <- tryCatch(as.integer(query$b), error = function(e) NA)
    if (is.na(b)) {
      return(list(valid = FALSE, status = 400, message = "Parameter 'b' must be an integer"))
    }
    if (b <= 0) {
      return(list(valid = FALSE, status = 400, message = "Parameter 'b' must be greater than 0"))
    }
    if (b >= length(y)) {
      msg <- paste0(
        "Parameter 'b' (", b, ") must be less than the data length (", length(y), ")"
      )
      return(list(valid = FALSE, status = 400, message = msg))
    }
    if (b < 3) {
      return(list(
        valid = FALSE,
        status = 400,
        message = "Parameter 'b' must be at least 3 to calculate meaningful statistics"
      ))
    }
    # Validate that baseline period has sufficient non-NA values
    baseline_data <- y[1:b]
    non_na_count <- sum(!is.na(baseline_data))
    if (non_na_count < 3) {
      msg <- paste0(
        "Baseline period (first ", b, " values) contains only ", non_na_count,
        " non-NA values. At least 3 non-NA values are required."
      )
      return(list(valid = FALSE, status = 400, message = msg))
    }
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
      return(list(
        valid = FALSE,
        status = 400,
        message = "Parameter 's' must be 1 (year), 2 (quarter), 3 (month), or 4 (week)"
      ))
    }

    # Validate 'm' (method)
    m <- query$m
    if (is.null(m) || !m %in% c("naive", "mean", "median", "lin_reg", "exp")) {
      return(list(
        valid = FALSE,
        status = 400,
        message = "Parameter 'm' must be one of: naive, mean, median, lin_reg, exp"
      ))
    }

    # Validate 'xs' (start time index) if provided
    if (!is.null(query$xs)) {
      xs <- as.character(query$xs)
      xs_valid <- FALSE
      xs_error <- NULL

      if (s == 4) {
        # Weekly: expect format like "2020W10" or "2020-W10"
        if (grepl("^\\d{4}-?W\\d{1,2}$", xs, ignore.case = TRUE)) {
          # Normalize format: remove hyphen if present
          xs_normalized <- gsub("-", "", toupper(xs))
          year <- as.integer(substr(xs_normalized, 1, 4))
          week <- as.integer(sub(".*W", "", xs_normalized))
          if (!is.na(year) && !is.na(week) && week >= 1 && week <= 53) {
            xs_valid <- TRUE
          } else {
            xs_error <- "Invalid week number in 'xs' (must be 1-53)"
          }
        } else {
          xs_error <- "Parameter 'xs' for weekly data must be in format 'YYYYWNN' (e.g., '2020W10')"
        }
      } else if (s == 3) {
        # Monthly: expect format like "2020-01" or "202001"
        if (grepl("^\\d{4}-?\\d{2}$", xs)) {
          xs_normalized <- gsub("-", "", xs)
          year <- as.integer(substr(xs_normalized, 1, 4))
          month <- as.integer(substr(xs_normalized, 5, 6))
          if (!is.na(year) && !is.na(month) && month >= 1 && month <= 12) {
            xs_valid <- TRUE
          } else {
            xs_error <- "Invalid month number in 'xs' (must be 01-12)"
          }
        } else {
          xs_error <- "Parameter 'xs' for monthly data must be in format 'YYYY-MM' (e.g., '2020-01')"
        }
      } else if (s == 2) {
        # Quarterly: expect format like "2020Q1" or "2020-Q1"
        if (grepl("^\\d{4}-?Q[1-4]$", xs, ignore.case = TRUE)) {
          xs_valid <- TRUE
        } else {
          xs_error <- "Parameter 'xs' for quarterly data must be in format 'YYYYQN' (e.g., '2020Q1')"
        }
      } else if (s == 1) {
        # Yearly: expect just a year like "2020"
        if (grepl("^\\d{4}$", xs)) {
          xs_valid <- TRUE
        } else {
          xs_error <- "Parameter 'xs' for yearly data must be in format 'YYYY' (e.g., '2020')"
        }
      }

      if (!xs_valid) {
        return(list(valid = FALSE, status = 400, message = xs_error))
      }
    }
  }

  return(list(valid = TRUE))
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
  response$set_header("Cache-Control", "no-store")
  return(response)
}

#' Parse request body for POST requests
#'
#' @param request Request object (reqres::Request)
#' @return List of parsed body parameters, or NULL if not a POST request
parse_post_body <- function(request) {
  # Method may be lowercase (post) or uppercase (POST) depending on server
  if (toupper(request$method) != "POST") {
    return(NULL)
  }

  # In reqres, must call parse() before accessing body
  # Use reqres::parse_json() with simplifyVector = FALSE to preserve arrays as lists
  tryCatch({
    request$parse(
      `application/json` = reqres::parse_json(simplifyVector = FALSE),
      autofail = FALSE
    )
    body <- request$body
    if (is.null(body) || length(body) == 0 || identical(body, "")) {
      return(NULL)
    }
    body
  }, error = function(e) {
    # Fallback: try to read raw body directly from origin rook object
    tryCatch({
      rook <- request$origin
      if (!is.null(rook) && !is.null(rook$rook.input)) {
        raw_body <- rook$rook.input$read()
        if (!is.null(raw_body) && length(raw_body) > 0) {
          return(jsonlite::fromJSON(rawToChar(raw_body), simplifyVector = FALSE))
        }
      }
      NULL
    }, error = function(e2) {
      NULL
    })
  })
}

#' Merge POST body params with query params (POST body takes precedence for 'y')
#'
#' @param query Query parameters from URL
#' @param body_params Parsed body parameters from POST
#' @return Merged parameters list
merge_request_params <- function(query, body_params) {
  if (is.null(body_params)) {
    return(query)
  }

  # Start with query params
  params <- query

  # Override/add from body params
  for (name in names(body_params)) {
    params[[name]] <- body_params[[name]]
  }

  params
}

# Main request handler
app$on("request", function(server, request, ...) {
  start_time <- Sys.time()

  # Extract client IP (consider X-Forwarded-For for proxied requests)
  client_ip <- request$REMOTE_ADDR %||% "unknown"

  # Parse POST body if present
  body_params <- parse_post_body(request)

  # Merge query params with body params (body takes precedence for large data like 'y')
  merged_params <- merge_request_params(request$query, body_params)

  # Log incoming request
  log_message("INFO", "Incoming request", list(
    path = request$path,
    method = request$method,
    ip = client_ip,
    params = paste(sprintf("%s=%s", names(merged_params),
                          sapply(merged_params, function(x) {
                            if (is.null(x)) return("")
                            # Handle lists/arrays by converting to string representation
                            s <- if (is.list(x) || length(x) > 1) {
                              paste0("[", length(x), " items]")
                            } else {
                              as.character(x)
                            }
                            if (nchar(s) > 50) paste0(substr(s, 1, 47), "...") else s
                          })), collapse = ", ")
  ))

  # Handle CORS preflight (OPTIONS) requests
  # Browsers send OPTIONS before cross-origin requests - skip validation
  if (toupper(request$method) == "OPTIONS") {
    response <- request$respond()
    response$status <- 200L
    response$body <- ""
    # CORS headers are added by fiery/routr middleware or proxy
    log_message("INFO", "CORS preflight", list(path = request$path, ip = client_ip))
    return(response)
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

    log_message("INFO", "Health check", list(status = "ok"))
    return(response)
  }

  # Rate limiting
  if (!check_rate_limit(client_ip)) {
    log_message("WARN", "Rate limit exceeded", list(ip = client_ip))
    error_msg <- paste0(
      "Rate limit exceeded. Maximum ", RATE_LIMIT_MAX_REQUESTS,
      " requests per ", RATE_LIMIT_WINDOW, " seconds."
    )
    return(send_error(server, request, 429, error_msg))
  }

  # Check if route exists (before parameter validation)
  if (!request$path %in% c("/", "/cum")) {
    log_message("WARN", "Route not found", list(path = request$path, ip = client_ip))
    return(send_error(server, request, 404, "Route not found. Available routes: /, /cum, /health"))
  }

  # Validate request (use merged params for POST support)
  validation <- validate_request(merged_params, request$path)
  if (!validation$valid) {
    log_message("WARN", "Validation failed", list(
      ip = client_ip,
      error = validation$message
    ))
    return(send_error(server, request, validation$status, validation$message))
  }

  # Check cache for non-health endpoints (use merged params for cache key)
  cache_key <- generate_cache_key(request$path, merged_params)
  cached_result <- get_cached_response(cache_key)

  if (!is.null(cached_result)) {
    log_message("INFO", "Cache hit", list(
      path = request$path,
      cache_key = substr(cache_key, 1, 100)
    ))

    response <- request$respond()
    response$body <- jsonlite::toJSON(cached_result, auto_unbox = TRUE, na = "null")
    response$status <- 200L
    response$type <- "json"
    response$set_header("X-Cache", "HIT")
    return(response)
  }

  # Parse parameters (use merged_params for POST support)
  # Handle 'y' as either comma-separated string (GET) or numeric array (POST)
  y_param <- merged_params$y
  if (is.character(y_param)) {
    y <- as.double(strsplit(as.character(y_param), ",")[[1]])
  } else if (is.list(y_param) || is.numeric(y_param)) {
    y <- as.double(unlist(y_param))
  } else {
    y <- as.double(y_param)
  }

  h <- as.integer(merged_params$h %||% 1)
  t <- as.integer(merged_params$t %||% 0) == 1

  # Parse baseline parameters: bs/be (new) or b (legacy)
  bs <- NULL
  be <- NULL
  if (!is.null(merged_params$bs) && !is.null(merged_params$be)) {
    bs <- as.integer(merged_params$bs)
    be <- as.integer(merged_params$be)
  } else if (!is.null(merged_params$b)) {
    # Legacy mode: bs=1, be=b
    bs <- 1L
    be <- as.integer(merged_params$b)
  }

  # Parse xs (start time index) - optional
  xs <- merged_params$xs

  # Process request with error handling
  res <- tryCatch({
    if (request$path == "/") {
      m <- merged_params$m
      s <- as.integer(merged_params$s)
      handleForecast(y, h, m, s, t, bs, be, xs)
    } else {
      # request$path == "/cum" (already validated above)
      handleCumulativeForecast(y, h, t, bs, be)
    }
  }, error = function(e) {
    log_message("ERROR", "Processing failed", list(
      ip = client_ip,
      path = request$path,
      error = as.character(e)
    ))

    # Capture exception in Sentry
    capture_exception(e,
      extra = list(
        path = request$path,
        method = request$method,
        query = paste(names(merged_params), unlist(merged_params), sep = "=", collapse = "&"),
        ip = client_ip
      ),
      tags = list(
        endpoint = request$path,
        method = request$method
      )
    )

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
  response$body <- jsonlite::toJSON(res, auto_unbox = TRUE, na = "null")
  response$status <- 200L
  response$type <- "json"
  response$set_header("X-Cache", "MISS")

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

# Start server (only when run directly, not when sourced for tests)
if (sys.nframe() == 0) {
  log_message("INFO", "Starting server", list(port = port))
  app$ignite(showcase = FALSE)
}
