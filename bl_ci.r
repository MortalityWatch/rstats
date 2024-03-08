needs(tibble)
needs(fable)
needs(tidyverse)

x <- input[[1]][[1]]
y <- input[[1]][[2]]
h <- input[[1]][[3]]

df_bl <- tibble(x = x, y = y) |> as_tsibble(index = x)
fc <- df_bl |>
  model(TSLM(y ~ trend())) |>
  forecast(h = h)
fabletools::hilo(fc, 95) |>
  unpack_hilo(cols = `95%`) |>
  select(x, .mean, "95%_lower", "95%_upper") |>
  setNames(c("x", "mean", "lower", "upper")) |>
  mutate_if(is.numeric, round, 1)
