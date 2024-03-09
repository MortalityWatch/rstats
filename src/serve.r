library(tibble)
library(fable)
library(tidyverse)
library(fiery)

app <- Fire$new(host = "0.0.0.0", port = 3000)

app$on("request", function(server, request, ...) {
  y <- as.double(strsplit(request$query$y, ",")[[1]])
  h <- as.integer(request$query$h)

  df_bl <- tibble(x = seq.int(1, length(y)), y = y) |> as_tsibble(index = x)
  fc <- df_bl |>
    model(TSLM(y ~ trend())) |>
    forecast(h = h)
  result <- fabletools::hilo(fc, 95) |>
    unpack_hilo(cols = `95%`) |>
    as_tibble() |>
    select("95%_lower", "95%_upper") |>
    setNames(c("lower", "upper")) |>
    mutate_if(is.numeric, round, 1)

  response <- request$respond()
  response$body <- jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE)
  response$status <- 200L
  response$type <- "json"
  response
})

app$ignite(showcase = FALSE)

# source("./src/serve.r")
