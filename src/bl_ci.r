needs(tibble)
needs(fable)
needs(tidyverse)

y <- input[[1]][[1]]
h <- input[[1]][[2]]

df_bl <- tibble(x = seq.int(1, length(y)), y = y) |> as_tsibble(index = x)
fc <- df_bl |>
  model(TSLM(y ~ trend())) |>
  forecast(h = h)
fabletools::hilo(fc, 95) |>
  unpack_hilo(cols = `95%`) |>
  as_tibble() |>
  select("95%_lower", "95%_upper") |>
  setNames(c("lower", "upper")) |>
  mutate_if(is.numeric, round, 1)
