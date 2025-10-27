# Sentry Integration for R using sentryR package
# Provides error tracking and monitoring capabilities

library(sentryR)

#' Initialize Sentry client
#'
#' Sets up Sentry error tracking if DSN is configured
#' @return TRUE if Sentry is enabled, FALSE otherwise
init_sentry <- function() {
  dsn <- Sys.getenv("SENTRY_DSN", "")

  if (dsn != "") {
    configure_sentry(
      dsn = dsn,
      app_name = "stats.mortality.watch",
      app_version = Sys.getenv("APP_VERSION", "1.0.0"),
      environment = Sys.getenv("SENTRY_ENVIRONMENT", "production")
    )

    message(sprintf(
      "[%s] INFO: Sentry initialized - environment=%s, traces_sample_rate=%s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Sys.getenv("SENTRY_ENVIRONMENT", "production"),
      Sys.getenv("SENTRY_TRACES_SAMPLE_RATE", "0.1")
    ))
    return(TRUE)
  } else {
    message(sprintf(
      "[%s] INFO: Sentry not configured (SENTRY_DSN not set)",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
    return(FALSE)
  }
}

#' Capture exception and send to Sentry
#'
#' @param error Error object
#' @param extra Additional context data (named list) - currently ignored to avoid R6 serialization issues
#' @param tags Tags to attach to the event (named list) - currently ignored to avoid R6 serialization issues
#' @return TRUE if sent successfully, FALSE otherwise
capture_exception <- function(error, extra = NULL, tags = NULL) {
  tryCatch({
    # Use sentryR's capture_exception without extra context
    # Note: extra and tags are ignored to avoid R6 serialization issues
    # The error message and stacktrace are still captured
    sentryR::capture_exception(error = error)

    return(TRUE)
  }, error = function(e) {
    warning(sprintf("Failed to send exception to Sentry: %s", conditionMessage(e)))
    return(FALSE)
  })
}
