## Advent of Code Day 1

library(tidyverse)
library(here)

input <- read_table(here("day_one/input.txt"), 
                    col_names = FALSE, 
                    skip_empty_rows = FALSE) |> 
  rename(cals = X1)

input |> 
  mutate(group = cumsum(is.na(cals))) |> 
  drop_na() |> 
  group_by(group) |> 
  summarize(total_cals = sum(cals)) |> 
  slice_max(total_cals, n = 3) |> # n = 1 for part 1 answer
  summarize(top_cals = sum(total_cals)) # Part 2 answer
  

