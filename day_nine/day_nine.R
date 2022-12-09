##Advent of Code day 9

library(tidyverse)
library(here)

rep2 <- function(x,y) if (x == 0) return (0) else rep(x,y) #This function turns reach line of input into an individual move?

snake <- function(position, head) {
  
  dist <- head - position
  if(abs(dist) <= sqrt(2)) #OOOOH the distance diagonally is root 2! So as long as dist is less than that T doesn't have to move
    return(position)
  position + sign(Re(dist)) + sign(Im(dist)) * 1i
  #^a lot of math up here....
}

moves <- read_table(here("day_nine/input.txt"), col_names = c("dir", "moves")) |> 
  mutate(across(moves, as.numeric)) |> 
  mutate(dx = ((dir == "R") - (dir == "L")) * moves, #this sets a postivie and negative direction for both axes
         dy = ((dir == "U") - (dir == "D")) * moves) |> 
  summarise(dx = mapply(rep2, sign(dx), abs(dx)),
            dy = mapply(rep2, sign(dy), abs(dy))) |> 
  unnest_longer(c(dx,dy)) |> #see each individual 1 as a pos or neg move
  mutate(head = cumsum(dx) + 1i *cumsum(dy)) #unclear what the 1i is? i would be imaginary? Or is it keeping track of T?

knot <- c(0, moves$head)
for(k in 1:9) {
  knot <- purrr:::accumulate(knot, snake)
  if (k %in% c(1,9)) print(length(unique(knot)))
}







