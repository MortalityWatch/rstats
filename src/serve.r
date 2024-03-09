library(tibble)
library(fable)
library(tidyverse)
library(fiery)
library(tsibble)

port <- ifelse(Sys.getenv("PORT") != "", Sys.getenv("PORT"), "3000")
app <- Fire$new(host = "0.0.0.0", port = as.integer(port))
app$header("Access-Control-Allow-Origin", "*")

app$on("request", function(server, request, ...) {
  y <- as.double(strsplit(request$query$y, ",")[[1]])
  h <- as.integer(request$query$h)
  t <- as.logical(request$query$t)
  s <- as.integer(request$query$s) # Year = 1, Quarter = 2, ...

  # Sample
  # y <- c(709.9, 684.9, 686.9)
  # h <- 5
  # s <- 1
  # t <- FALSE

  df_bl <- tibble(x = seq.int(1, length(y)), y = y)
  if (s == 2) {
    df_bl$x <- make_yearquarter(2000, 1) + 0:(length(y) - 1)
  } else if (s == 3) {
    df_bl$x <- make_yearmonth(2000, 1) + 0:(length(y) - 1)
  } else if (s == 4) {
    df_bl$x <- make_yearweek(2000, 1) + 0:(length(y) - 1)
  }

  df_bl <- df_bl |> as_tsibble(index = x)
  if (t) {
    if (s > 1) {
      m <- df_bl |> model(TSLM(y ~ trend() + season()))
    } else {
      m <- df_bl |> model(TSLM(y ~ trend()))
    }
  } else {
    if (s > 1) {
      m <- df_bl |> model(TSLM(y ~ season()))
    } else {
      m <- df_bl |> model(TSLM(y))
    }
  }

  fc <- m |> forecast(h = h)
  bl <- m |> forecast(new_data = df_bl)

  result <- fabletools::hilo(fc, 95) |>
    unpack_hilo(cols = `95%`) |>
    as_tibble() |>
    select(.mean, "95%_lower", "95%_upper") |>
    setNames(c("y", "lower", "upper")) |>
    mutate_if(is.numeric, round, 1)
  result <- bind_rows(tibble(y = bl$.mean), result)

  response <- request$respond()
  response$body <- jsonlite::toJSON(
    list(y = result$y, lower = result$lower, upper = result$upper)
  )
  response$status <- 200L
  response$type <- "json"
  response
})

app$ignite(showcase = FALSE)

# source("./src/serve.r")
