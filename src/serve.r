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
  model_formula <- y ~ x
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
app$header("Access-Control-Allow-Origin", "*")
app$header("Cache-Control", "max-age=86400") # Cache 1d

handleForecast <- function(y, h, s, t, n) {
  # y <- c(756.7, 733.9, 696.9, 713.7, 707.7, 678.3, 708.5, 681.8, 684)
  # h <- 5
  # s <- 1
  # t <- TRUE
  # n <- FALSE

  df <- tibble(x = seq.int(1, length(y)), y = y)
  if (s == 2) {
    df$x <- make_yearquarter(2000, 1) + 0:(length(y) - 1)
  } else if (s == 3) {
    df$x <- make_yearmonth(2000, 1) + 0:(length(y) - 1)
  } else if (s == 4) {
    df$x <- make_yearweek(2000, 1) + 0:(length(y) - 1)
  }

  fun <- ifelse(n, NAIVE, TSLM)
  df <- df |> as_tsibble(index = x)
  if (t) {
    if (!n && s > 1) {
      m <- df |> model(fun(y ~ trend() + season()))
    } else {
      m <- df |> model(fun(y ~ trend()))
    }
  } else {
    if (!n && s > 1) {
      m <- df |> model(fun(y ~ season()))
    } else {
      m <- df |> model(fun(y))
    }
  }

  fc <- m |> forecast(h = h)
  bl <- m |> forecast(new_data = df)

  result <- fabletools::hilo(fc, 95) |>
    unpack_hilo(cols = `95%`) |>
    as_tibble() |>
    select(.mean, "95%_lower", "95%_upper") |>
    setNames(c("y", "lower", "upper"))
  result <- bind_rows(tibble(y = bl$.mean), result) |>
    mutate_if(is.numeric, round, 1)

  list(y = result$y, lower = result$lower, upper = result$upper)
}

cumForecastN <- function(df_train, df_test, t) {
  # Model: Lin. regr.
  # model <- lm(y ~ x, data = df_train)
  if (t) {
    m <- df_train |> model(lm = TSLM(y ~ trend()))
  } else {
    m <- df_train |> model(lm = TSLM(y))
  }
  # model <- df_train |> model(lm = TSLM(y ~ trend()))

  # Forecast
  # oo <- lm_predict(model, df_test, FALSE)
  oo <- lm_predict_tslm(model = m, newdata = df_test, FALSE)

  fc_sum_mean <- sum(oo$fit)
  fc_sum_variance <- sum(oo$var.fit)

  n <- ncol(lengths(oo$var.fit))
  res <- agg_pred(rep.int(x = 1, length(oo$fit)), oo, alpha = .95)
  tibble(
    actual = round(sum(df_test$y), 1),
    y = round(fc_sum_mean, 1),
    lower = round(res$PI[1], 1),
    upper = round(res$PI[2], 1)
  )
}

handleCumulativeForecast <- function(y, h, t) {
  # USA
  # y <- c(878.6, 866.4, 864.9, 1017.8, 1029.5, 961.5, 896.2)
  # h <- 4
  # t <- FALSE

  z <- length(y) - h

  df <- tibble(x = seq.int(1, length(y)), y = y) |> as_tsibble(index = x)
  df_train <- df |> filter(x <= z)
  result <- tibble()
  for (h_ in 1:h) {
    df_test <- df |> filter(x %in% (z + 1):(z + h_))
    result <- rbind(result, cumForecastN(df_train, df_test, t))
  }

  list(
    actual = result$actual,
    y = result$y,
    lower = result$lower,
    upper = result$upper
  )
}

app$on("request", function(server, request, ...) {
  y <- as.double(strsplit(request$query$y, ",")[[1]])
  h <- as.integer(request$query$h)
  t <- as.logical(request$query$t)

  if (request$path == "/") {
    s <- as.integer(request$query$s) # Year = 1, Quarter = 2, ...
    n <- as.logical(request$query$n) # Naive
    res <- handleForecast(y, h, s, t, n)
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
