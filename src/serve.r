library(tibble)
library(fable)
library(tidyverse)
library(fiery)
library(tsibble)

# https://stackoverflow.com/a/39338512/2302437
lm_predict <- function(lmObject, newdata, diag = TRUE) {
  # input checking
  if (!inherits(lmObject, "lm")) stop("'lmObject' is not a valid 'lm' object!")
  # extract "terms" object from the fitted model, but delete response variable
  tm <- delete.response(terms(lmObject))
  # linear predictor matrix
  Xp <- model.matrix(tm, newdata)
  # predicted values by direct matrix-vector multiplication
  pred <- c(Xp %*% coef(lmObject))

  # efficiently form the complete variance-covariance matrix
  QR <- lmObject$qr # qr object of fitted model
  piv <- QR$pivot # pivoting index
  r <- QR$rank # model rank / numeric rank
  if (is.unsorted(piv)) {
    # pivoting has been done
    B <- forwardsolve(t(QR$qr), t(Xp[, piv]), r)
  } else {
    # no pivoting is done
    B <- forwardsolve(t(QR$qr), t(Xp), r)
  }
  # residual variance
  sig2 <- c(crossprod(residuals(lmObject))) / df.residual(lmObject)
  if (diag) {
    # return point-wise prediction variance
    VCOV <- colSums(B^2) * sig2
  } else {
    # return full variance-covariance matrix of predicted values
    VCOV <- crossprod(B) * sig2
  }
  list(
    fit = pred,
    var.fit = VCOV,
    df = lmObject$df.residual,
    residual.var = sig2
  )
}

# Adapted lm_predict to work with fable tslm models.
lm_predict_tslm <- function(model, newdata, diag = TRUE) {
  # 1) Fit
  fc <- model |> forecast(h = nrow(newdata))

  # 3) Residual
  res <- residuals(model)$.resid
  res[is.na(res)] <- 0
  n <- length(res) # number of observations
  p <- nrow(coef(model)) # number of parameters
  df_residual <- n - p

  # 4) Sigma2
  sig2 <- c(crossprod(res)) / df_residual

  # 2) Variance-Covariance
  model_formula <- asmr ~ year
  Xp <- model.matrix(model_formula, newdata)

  tslm_model <- model$lm[[1]]
  QR <- tslm_model$fit$qr # qr object of fitted model
  piv <- tslm_model$fit$qr$pivot # pivoting index
  r <- tslm_model$fit$qr$rank # model rank / numeric rank
  if (is.unsorted(piv)) {
    # pivoting has been done
    B <- forwardsolve(t(QR$qr), t(Xp[, piv]), r)
  } else {
    # no pivoting is done
    B <- forwardsolve(t(QR$qr), t(Xp), r)
  }

  if (diag) {
    # return point-wise prediction variance
    VCOV <- colSums(B^2) * sig2
  } else {
    # return full variance-covariance matrix of predicted values
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

# https://stackoverflow.com/a/39338512/2302437
agg_pred <- function(w, predObject, alpha = 0.95) {
  # input checing
  if (length(w) != length(predObject$fit)) stop("'w' has wrong length!")
  if (!is.matrix(predObject$var.fit)) {
    stop("'predObject' has no variance-covariance matrix!")
  }
  # mean of the aggregation
  agg_mean <- c(crossprod(predObject$fit, w))
  # variance of the aggregation
  agg_variance <- c(crossprod(w, predObject$var.fit %*% w))
  # adjusted variance-covariance matrix
  VCOV_adj <- with(predObject, var.fit + diag(residual.var, nrow(var.fit)))
  # adjusted variance of the aggregation
  agg_variance_adj <- c(crossprod(w, VCOV_adj %*% w))
  # t-distribution quantiles
  Qt <- c(-1, 1) * qt((1 - alpha) / 2, predObject$df, lower.tail = FALSE)
  # names of CI and PI
  NAME <- c("lower", "upper")
  # CI
  CI <- setNames(agg_mean + Qt * sqrt(agg_variance), NAME)
  # PI
  PI <- setNames(agg_mean + Qt * sqrt(agg_variance_adj), NAME)
  # return
  list(mean = agg_mean, var = agg_variance, CI = CI, PI = PI)
}

port <- ifelse(Sys.getenv("PORT") != "", Sys.getenv("PORT"), "3000")
app <- Fire$new(host = "0.0.0.0", port = as.integer(port))

handleForecast <- function(y, h, m, s, t) {
  # y <- c(756.7, 733.9, 696.9, 713.7, 707.7, 678.3, 708.5, 681.8, 684)
  # h <- 5
  # m <- "exp"
  # s <- 1
  # t <- TRUE

  df <- tibble(year = seq.int(1, length(y)), asmr = y)
  if (s == 2) {
    df$year <- make_yearquarter(2000, 1) + 0:(length(y) - 1)
  } else if (s == 3) {
    df$year <- make_yearmonth(2000, 1) + 0:(length(y) - 1)
  } else if (s == 4) {
    df$year <- make_yearweek(2000, 1) + 0:(length(y) - 1)
  }

  leading_NA <- nrow(df |> filter(is.na(asmr)))

  df <- df |>
    as_tsibble(index = year) |>
    filter(!is.na(asmr))

  if (m == "naive") {
    mdl <- df |> model(NAIVE(asmr))
  } else if (m == "mean") {
    if (s > 1) {
      mdl <- df |> model(TSLM(asmr ~ season()))
    } else {
      mdl <- df |> model(TSLM(asmr))
    }
  } else if (m == "lin_reg") {
    if (t) {
      if (s > 1) {
        mdl <- df |> model(TSLM(asmr ~ trend() + season()))
      } else {
        mdl <- df |> model(TSLM(asmr ~ trend()))
      }
    } else {
      if (s > 1) {
        mdl <- df |> model(TSLM(asmr ~ season()))
      } else {
        mdl <- df |> model(TSLM(asmr))
      }
    }
  } else if (m == "exp") {
    if (s > 1) {
      mdl <- df |> model(ETS(asmr ~ error() + trend() + season()))
    } else {
      mdl <- df |> model(ETS(asmr ~ error("A") + trend("Ad")))
    }
  }

  fc <- mdl |> forecast(h = h)
  bl <- mdl |>
    augment() |>
    rename(.mean = .fitted)

  result <- fabletools::hilo(fc, 95) |>
    unpack_hilo(cols = `95%`) |>
    as_tibble() |>
    select(.mean, "95%_lower", "95%_upper") |>
    setNames(c("y", "lower", "upper"))
  result <- bind_rows(
    tibble(y = rep(NA, leading_NA)),
    tibble(y = bl$.mean), result
  ) |>
    mutate_if(is.numeric, round, 1)

  list(y = result$y, lower = result$lower, upper = result$upper)
}

cumForecastN <- function(df_train, df_test, mdl) {
  oo <- lm_predict_tslm(model = mdl, newdata = df_test, FALSE)

  fc_sum_mean <- sum(oo$fit)
  fc_sum_variance <- sum(oo$var.fit)

  n <- ncol(lengths(oo$var.fit))
  res <- agg_pred(rep.int(x = 1, length(oo$fit)), oo, alpha = .95)
  tibble(
    asmr = round(fc_sum_mean, 1),
    lower = round(res$PI[1], 1),
    upper = round(res$PI[2], 1)
  )
}

uncumulate <- function(cumulative_vector) {
  # Check if the vector is empty
  if (length(cumulative_vector) == 0) {
    return(cumulative_vector)
  }

  # Initialize the uncumulated vector with the first element of the cumulative vector
  uncumulated_vector <- numeric(length(cumulative_vector))
  uncumulated_vector[1] <- cumulative_vector[1]

  # Calculate the uncumulated values
  for (i in 2:length(cumulative_vector)) {
    uncumulated_vector[i] <- cumulative_vector[i] - cumulative_vector[i - 1]
  }

  return(uncumulated_vector)
}

handleCumulativeForecast <- function(y, h, t) {
  # USA
  # y <- c(878.6, 866.4, 864.9, 1017.8, 1029.5, 961.5, 896.2)
  # h <- 4
  # t <- FALSE

  z <- length(y) - h

  df <- tibble(year = seq.int(1, length(y)), asmr = y) |>
    as_tsibble(index = year)
  df_train <- df |> filter(year <= z)

  if (t) {
    mdl <- df_train |> model(lm = TSLM(asmr ~ trend()))
  } else {
    mdl <- df_train |> model(lm = TSLM(asmr))
  }

  bl <- mdl |>
    augment() |>
    rename(.mean = .fitted)

  result <- tibble()
  for (h_ in 1:h) {
    df_test <- df |> filter(year %in% (z + 1):(z + h_))
    result <- rbind(result, cumForecastN(df_train, df_test, mdl))
  }

  list(
    y = c(bl$.mean, uncumulate(result$asmr)),
    lower = c(rep(NA, nrow(bl)), uncumulate(result$lower)),
    upper = c(rep(NA, nrow(bl)), uncumulate(result$upper))
  )
}

app$on("request", function(server, request, ...) {
  print(sprintf("Processing request: %s", paste(request$query, collapse = ", ")))
  
  y <- as.double(strsplit(request$query$y, ",")[[1]])
  h <- as.integer(request$query$h)
  t <- as.logical(request$query$t)

  if (request$path == "/") {
    m <- request$query$m # Method
    s <- as.integer(request$query$s) # Year = 1, Quarter = 2, ...
    res <- handleForecast(y, h, m, s, t)
  } else if (request$path == "/cum") {
    res <- handleCumulativeForecast(y, h, t)
  } else {
    # Handle other routes
    server$status(404)
    server$send("Route not found")
  }

  # Response
  response <- request$respond()
  response$body <- jsonlite::toJSON(res)
  response$status <- 200L
  response$type <- "json"
  response
})

app$ignite(showcase = FALSE)

# source("./src/serve.r")
