# Sentry Integration for R
# Provides error tracking and monitoring capabilities

library(httr)
library(jsonlite)

# Initialize Sentry configuration
sentry_config <- list(
  dsn = Sys.getenv("SENTRY_DSN", ""),
  environment = Sys.getenv("SENTRY_ENVIRONMENT", "production"),
  traces_sample_rate = as.numeric(Sys.getenv("SENTRY_TRACES_SAMPLE_RATE", "0.1")),
  enabled = FALSE
)

#' Initialize Sentry client
#'
#' Sets up Sentry error tracking if DSN is configured
#' @return TRUE if Sentry is enabled, FALSE otherwise
init_sentry <- function() {
  if (sentry_config$dsn != "") {
    sentry_config$enabled <<- TRUE
    log_message("INFO", "Sentry initialized", list(
      environment = sentry_config$environment,
      traces_sample_rate = sentry_config$traces_sample_rate
    ))
    return(TRUE)
  } else {
    log_message("INFO", "Sentry not configured (SENTRY_DSN not set)")
    return(FALSE)
  }
}

#' Parse Sentry DSN to extract API endpoint
#'
#' @param dsn Sentry DSN string
#' @return List with project_id and endpoint URL
parse_sentry_dsn <- function(dsn) {
  # DSN format: https://[key]@[organization].ingest.sentry.io/[project_id]
  pattern <- "https://([^@]+)@([^/]+)/([0-9]+)"
  matches <- regmatches(dsn, regexec(pattern, dsn))[[1]]

  if (length(matches) != 4) {
    stop("Invalid Sentry DSN format")
  }

  list(
    key = matches[2],
    host = matches[3],
    project_id = matches[4],
    endpoint = sprintf("https://%s/api/%s/store/", matches[3], matches[4])
  )
}

#' Send event to Sentry
#'
#' @param event_data List containing event data
#' @return TRUE if sent successfully, FALSE otherwise
send_to_sentry <- function(event_data) {
  if (!sentry_config$enabled) {
    return(FALSE)
  }

  tryCatch({
    dsn_parts <- parse_sentry_dsn(sentry_config$dsn)

    # Build Sentry auth header
    auth_header <- sprintf(
      "Sentry sentry_version=7, sentry_key=%s, sentry_client=r-custom/1.0.0",
      dsn_parts$key
    )

    # Send POST request to Sentry
    response <- POST(
      url = dsn_parts$endpoint,
      add_headers(
        "X-Sentry-Auth" = auth_header,
        "Content-Type" = "application/json"
      ),
      body = toJSON(event_data, auto_unbox = TRUE),
      timeout(5)
    )

    if (status_code(response) == 200) {
      return(TRUE)
    } else {
      warning(sprintf("Sentry API returned status %d", status_code(response)))
      return(FALSE)
    }
  }, error = function(e) {
    warning(sprintf("Failed to send event to Sentry: %s", as.character(e)))
    return(FALSE)
  })
}

#' Capture exception and send to Sentry
#'
#' @param error Error object
#' @param extra Additional context data (list)
#' @param tags Tags to attach to the event (list)
capture_exception <- function(error, extra = NULL, tags = NULL) {
  if (!sentry_config$enabled) {
    return(FALSE)
  }

  # Build stack trace
  calls <- sys.calls()
  stacktrace <- lapply(seq_along(calls), function(i) {
    call <- calls[[i]]
    list(
      filename = "<stdin>",
      function_name = as.character(call[[1]]),
      lineno = i
    )
  })

  # Build event payload
  event <- list(
    event_id = gsub("-", "", uuid::UUIDgenerate()),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    platform = "r",
    level = "error",
    environment = sentry_config$environment,
    server_name = Sys.info()["nodename"],
    exception = list(
      values = list(
        list(
          type = class(error)[1],
          value = as.character(error),
          stacktrace = list(frames = stacktrace)
        )
      )
    ),
    tags = tags,
    extra = extra,
    contexts = list(
      runtime = list(
        name = "R",
        version = paste(R.version$major, R.version$minor, sep = ".")
      ),
      os = list(
        name = Sys.info()["sysname"],
        version = Sys.info()["release"]
      )
    )
  )

  send_to_sentry(event)
}

#' Capture message and send to Sentry
#'
#' @param message Message string
#' @param level Log level: "info", "warning", "error", "fatal"
#' @param extra Additional context data (list)
#' @param tags Tags to attach to the event (list)
capture_message <- function(message, level = "info", extra = NULL, tags = NULL) {
  if (!sentry_config$enabled) {
    return(FALSE)
  }

  event <- list(
    event_id = gsub("-", "", uuid::UUIDgenerate()),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    platform = "r",
    level = level,
    environment = sentry_config$environment,
    server_name = Sys.info()["nodename"],
    message = list(
      formatted = message
    ),
    tags = tags,
    extra = extra,
    contexts = list(
      runtime = list(
        name = "R",
        version = paste(R.version$major, R.version$minor, sep = ".")
      )
    )
  )

  send_to_sentry(event)
}

#' Wrap a function call with Sentry error tracking
#'
#' @param expr Expression to evaluate
#' @param extra Additional context data (list)
#' @param tags Tags to attach to any errors (list)
#' @return Result of expr or error
with_sentry <- function(expr, extra = NULL, tags = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      capture_exception(e, extra = extra, tags = tags)
      stop(e) # Re-throw the error
    }
  )
}
