## Advent of Code day 2
library(tidyverse)
library(here)

#score for shape: rock 1, paper 2, scissors 3
#score for outcome: loss 0, tie 3, win 6

#Need to calculate total score if everything in guide is correct

## Part One
input <- read_table(here("day_two/input.txt"),
                    col_names = FALSE) |> 
  rename(opp = X1, me = X2)

input |> 
  mutate(outcome = case_when(opp == "A" & me == "Y" ~ 6,
                             opp == "B" & me == "X" ~ 0,
                             opp == "C" & me == "Z" ~ 3,
                             opp == "A" & me == "X" ~ 3,
                             opp == "B" & me == "Y" ~ 3,
                             opp == "C" & me == "X" ~ 6,
                             opp == "A" & me == "Z" ~ 0,
                             opp == "B" & me == "Z" ~ 6,
                             opp == "C" & me == "Y" ~ 0)) |> 
  mutate(score = case_when(me == "X" ~ 1 + outcome,
                           me == "Y" ~ 2 + outcome,
                           me == "Z" ~ 3 + outcome)) |> 
  summarize(tot = sum(score))


## Part Two

input |> 
  mutate(shape = case_when(opp == "A" & me == "X" ~ 3,
                           opp == "A" & me == "Y" ~ 1,
                           opp == "A" & me == "Z" ~ 2,
                           opp == "B" & me == "X" ~ 1,
                           opp == "B" & me == "Y" ~ 2,
                           opp == "B" & me == "Z" ~ 3,
                           opp == "C" & me == "X" ~ 2,
                           opp == "C" & me == "Y" ~ 3,
                           opp == "C" & me == "Z" ~ 1)) |> 
  mutate(score = case_when(me == "X" ~ 0 + shape,
                           me == "Y" ~ 3 + shape,
                           me == "Z" ~ 6 + shape)) |> 
  summarize(tot = sum(score))
