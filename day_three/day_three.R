## Advent of Code Day 3

library(tidyverse)
library(here)

input <- read_lines(here("day_three/input.txt"))

smol_scores <- c(1:26) 
names(smol_scores) <- letters
BEEG_scores <- c(27:52)
names(BEEG_scores) <- LETTERS


## Part One
num_chars <- map_dbl(input, nchar)
divide_string <- function(x, y) {
  comp_one = substring(x, 1, y/2) 
  comp_two = substring(x, y/2+1, y) 
  tibble(comp_one, comp_two)
}

map2_df(input, num_chars, divide_string) |> 
  mutate(shared_item = map2_chr(comp_one, comp_two, function(x,y) intersect(str_split(x, pattern = "", simplify = TRUE), 
                                                                            str_split(y, pattern = "", simplify = TRUE)))) |> 
  rowwise() |> 
  mutate(priority_score = if_else(!is.na(smol_scores[shared_item]), smol_scores[shared_item], BEEG_scores[shared_item])) |>
  ungroup() |> 
  summarize(total_ruck =  sum(priority_score))

##Part 2

groups <- split(input, ceiling(seq_along(input) / 3))

get_shared <- function(x) {Reduce(intersect, list(str_split(x[1], pattern = "", simplify = TRUE), 
                                                  str_split(x[2], pattern = "", simplify = TRUE),
                                                  str_split(x[3], pattern = "", simplify = TRUE)))}

priority_score <- function(x) {if_else(!is.na(smol_scores[x]), smol_scores[x], BEEG_scores[x])}

shared <- map(groups, get_shared)

map(shared, priority_score) |> 
  unlist() |>
  sum()

