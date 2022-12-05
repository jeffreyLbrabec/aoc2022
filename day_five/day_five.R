## Advent of Code Day 5

library(tidyverse)
library(here)

stacks <- list(one = c("W", "R", "F"), 
               two = c("T", "H", "M", "C", "D", "V", "W", "R"),
               three = c("P", "M", "Z", "N", "L"),
               four = c("J", "C", "H", "R"),
               five = c("C", "P", "G", "H", "Q", "T", "B"),
               six = c("G", "C", "W", "L", "F", "Z"),
               seven = c("W", "V", "L", "Q", "Z", "J", "G", "C"),
               eight = c("P", "N", "R", "F", "W", "T", "V", "C"),
               nine = c("J", "W", "H", "G", "R", "S", "V"))

moves <- read_delim(here("day_five/input.txt"),
                    delim = " ",
                    col_names = FALSE,
                    skip = 10) |>
  select(X2, X4, X6) |> 
  rename(to_move = X2, from = X4, to = X6)


for(i in 1:nrow(moves)) {
  r <- moves[i,]
  #no rev for part2 as the crates stay in the same order when moved!
  to_move <- tail(stacks[[r$from]], r$to_move) #The tail lets you select all the "top" crates and rev gets them in the order you need
  stacks[[r$from]] <- head(stacks[[r$from]], -r$to_move) #Head hear plust the -r$to_move gets ride of the crate from its original stack
  stacks[[r$to]] <- c(stacks[[r$to]], to_move) #Add crates on top of new stack.

}




