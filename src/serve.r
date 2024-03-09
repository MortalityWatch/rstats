library(tibble)
library(fable)
library(tidyverse)
library(fiery)

app <- Fire$new(host = "0.0.0.0", port = 3000)

app$on("request", function(server, request, ...) {
  y <- as.double(strsplit(request$query$y, ",")[[1]])
  h <- as.integer(request$query$h)
  t <- as.logical(request$query$t)
  s <- as.logical(request$query$s)

  # Sample
  # y <- c(878.6, 866.4, 864.9)
  # h <- 3
  # t <- FALSE
  # s <- FALSE

  df_bl <- tibble(x = seq.int(1, length(y)), y = y) |> as_tsibble(index = x)

  if (t) {
    if (s) {
      m <- df_bl |> model(TSLM(y ~ trend() + season()))
    } else {
      m <- df_bl |> model(TSLM(y ~ trend()))
    }
  } else {
    if (s) {
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
  response$body <- jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE)
  response$status <- 200L
  response$type <- "json"
  response
})

app$ignite(showcase = FALSE)

# source("./src/serve.r")
