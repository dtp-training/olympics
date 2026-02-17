library(tidyverse)
library(here)

raw_results <-
  here("data-raw", "results.csv") |>
  read_csv()

youth_games <-
  raw_results |>
  filter(event |> str_detect("YOG")) |>
  distinct(year, type)

results <- 
  raw_results |> 
  drop_na(type) |> 
  anti_join(youth_games)

medals <-
  results |>
  drop_na(medal) |>
  distinct(
    year, type, discipline, event, noc, medal
  ) |>
  write_csv(here("data", "medals.csv"))

medals |> 
  count(year, type) |> 
  ggplot(aes(year, n, colour = type)) +
  geom_line()

medals |>
  filter(year == 2022) |>
  arrange(place) |>
  summarise(value = n(), .by = c(noc, medal)) |> 
  pivot_wider(names_from = medal, values_fill = 0) |>
  mutate(Total = Gold + Silver + Bronze) |>
  arrange(desc(Gold), desc(Silver), desc(Bronze))
