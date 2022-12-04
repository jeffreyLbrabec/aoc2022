## Advent of Code Day 4

library(tidyverse)
library(here)

input <- tibble::tribble(
  ~elf_one, ~elf_two,
  "2-4", "6-8",
  "2-3",  "4-5",
  "5-7",  "7-9",
  "2-8",  "3-7",
  "6-6",  "4-6",
  "2-6",  "4-8"
)

input <- read_csv(here("day_four/input.txt"), col_names = FALSE) |> 
  rename(elf_one = X1, elf_two = X2)


# Part One

input |> 
  separate(elf_one, c("eo_one", "eo_two"), sep = "-") |> 
  separate(elf_two, c("et_one", "et_two"), sep = "-") |> 
  mutate(across(everything(), as.numeric)) |> 
  rowwise() |> 
  mutate(any_inclusive = if_else(sum(between(eo_one:eo_two, et_one, et_two)) == length(eo_one:eo_two) |
                                   sum(between(et_one:et_two, eo_one, eo_two)) == length(et_one:et_two), TRUE, FALSE)) |> 
  ungroup() |> 
  count(any_inclusive)

# Part Two

input |> 
  separate(elf_one, c("eo_one", "eo_two"), sep = "-") |> 
  separate(elf_two, c("et_one", "et_two"), sep = "-") |> 
  mutate(across(everything(), as.numeric)) |> 
  rowwise() |> 
  mutate(any_inclusive = if_else(any(between(eo_one:eo_two, et_one, et_two)), TRUE, FALSE)) |> 
  ungroup() |> 
  count(any_inclusive)
