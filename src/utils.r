# Statistical Utility Functions
# Helper functions for prediction, aggregation, and data manipulation

#' Null-coalescing operator
#' @param a First value
#' @param b Default value if a is NULL
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Linear model prediction with variance
#'
#' Computes predictions and variance-covariance matrix for new data
#' Based on: https://stackoverflow.com/a/39338512/2302437
#'
#' @param lmObject An lm object from lm()
#' @param newdata Data frame with new data for prediction
#' @param diag If TRUE, return point-wise prediction variance; if FALSE, return full covariance matrix
#' @return List with fit, var.fit, df, and residual.var
lm_predict <- function(lmObject, newdata, diag = TRUE) {
  # Input checking
  if (!inherits(lmObject, "lm")) stop("'lmObject' is not a valid 'lm' object!")

  # Extract "terms" object from the fitted model, but delete response variable
  tm <- delete.response(terms(lmObject))

  # Linear predictor matrix
  Xp <- model.matrix(tm, newdata)

  # Predicted values by direct matrix-vector multiplication
  pred <- c(Xp %*% coef(lmObject))

  # Efficiently form the complete variance-covariance matrix
  QR <- lmObject$qr # qr object of fitted model
  piv <- QR$pivot # pivoting index
  r <- QR$rank # model rank / numeric rank

  if (is.unsorted(piv)) {
    # Pivoting has been done
    B <- forwardsolve(t(QR$qr), t(Xp[, piv]), r)
  } else {
    # No pivoting is done
    B <- forwardsolve(t(QR$qr), t(Xp), r)
  }

  # Residual variance
  sig2 <- c(crossprod(residuals(lmObject))) / df.residual(lmObject)

  if (diag) {
    # Return point-wise prediction variance
    VCOV <- colSums(B^2) * sig2
  } else {
    # Return full variance-covariance matrix of predicted values
    VCOV <- crossprod(B) * sig2
  }

  list(
    fit = pred,
    var.fit = VCOV,
    df = lmObject$df.residual,
    residual.var = sig2
  )
}

#' Adapted lm_predict to work with fable tslm models
#'
#' @param model A TSLM model from fable
#' @param newdata Data frame with new data for prediction
#' @param diag If TRUE, return point-wise prediction variance
#' @return List with fit, var.fit, df, and residual.var
lm_predict_tslm <- function(model, newdata, diag = TRUE) {
  # 1) Fit
  fc <- model |> forecast(h = nrow(newdata))

  # 2) Residual
  res <- residuals(model)$.resid
  res[is.na(res)] <- 0
  n <- length(res) # number of observations
  p <- nrow(coef(model)) # number of parameters
  df_residual <- n - p

  # 3) Sigma2
  sig2 <- c(crossprod(res)) / df_residual

  # 4) Variance-Covariance
  model_formula <- asmr ~ year
  Xp <- model.matrix(model_formula, newdata)

  tslm_model <- model$lm[[1]]
  QR <- tslm_model$fit$qr # qr object of fitted model
  piv <- tslm_model$fit$qr$pivot # pivoting index
  r <- tslm_model$fit$qr$rank # model rank / numeric rank

  if (is.unsorted(piv)) {
    # Pivoting has been done
    B <- forwardsolve(t(QR$qr), t(Xp[, piv]), r)
  } else {
    # No pivoting is done
    B <- forwardsolve(t(QR$qr), t(Xp), r)
  }

  if (diag) {
    # Return point-wise prediction variance
    VCOV <- colSums(B^2) * sig2
  } else {
    # Return full variance-covariance matrix of predicted values
    VCOV <- crossprod(B) * sig2
  }

  # Result
  list(
    fit = fc$.mean,
    var.fit = VCOV,
    df = df_residual,
    residual.var = sig2
  )
}

#' Aggregate prediction with confidence and prediction intervals
#'
#' Based on: https://stackoverflow.com/a/39338512/2302437
#'
#' @param w Weight vector for aggregation
#' @param predObject Prediction object from lm_predict
#' @param alpha Confidence level (default 0.95)
#' @return List with mean, variance, CI, and PI
agg_pred <- function(w, predObject, alpha = 0.95) {
  # Input checking
  if (length(w) != length(predObject$fit)) stop("'w' has wrong length!")
  if (!is.matrix(predObject$var.fit)) {
    stop("'predObject' has no variance-covariance matrix!")
  }

  # Mean of the aggregation
  agg_mean <- c(crossprod(predObject$fit, w))

  # Variance of the aggregation
  agg_variance <- c(crossprod(w, predObject$var.fit %*% w))

  # Adjusted variance-covariance matrix
  VCOV_adj <- with(predObject, var.fit + diag(residual.var, nrow(var.fit)))

  # Adjusted variance of the aggregation
  agg_variance_adj <- c(crossprod(w, VCOV_adj %*% w))

  # t-distribution quantiles
  Qt <- c(-1, 1) * qt((1 - alpha) / 2, predObject$df, lower.tail = FALSE)

  # Names of CI and PI
  NAME <- c("lower", "upper")

  # CI
  CI <- setNames(agg_mean + Qt * sqrt(agg_variance), NAME)

  # PI
  PI <- setNames(agg_mean + Qt * sqrt(agg_variance_adj), NAME)

  # Return
  list(mean = agg_mean, var = agg_variance, CI = CI, PI = PI)
}

#' Convert cumulative vector to incremental values
#'
#' @param cumulative_vector Vector of cumulative values
#' @return Vector of incremental (uncumulated) values
uncumulate <- function(cumulative_vector) {
  # Check if the vector is empty or single value
  if (length(cumulative_vector) <= 1) {
    return(cumulative_vector)
  }

  # Initialize the uncumulated vector with the first element
  uncumulated_vector <- numeric(length(cumulative_vector))
  uncumulated_vector[1] <- cumulative_vector[1]

  # Calculate the uncumulated values
  for (i in 2:length(cumulative_vector)) {
    uncumulated_vector[i] <- cumulative_vector[i] - cumulative_vector[i - 1]
  }

  return(uncumulated_vector)
}
