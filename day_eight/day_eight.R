## Advent of Code Day 8

library(tidyverse)
library(here)

input <- read_lines(here("day_eight/input.txt")) |> 
  as_tibble() |> 
  separate(value, into = as.character(c(1:100)), sep = "") |> 
  dplyr::select(- `1`) |> 
  mutate(across(everything(), as.numeric)) |> 
  as.matrix()


## Part One

vis_count <- 0

for(i in 1:nrow(input)) {
  
  for(j in 1:ncol(input)) {
    
    if(i == 1 | j == 1 | i == nrow(input) | j == ncol(input)) {
      
      vis_count <- vis_count + 1
      
    }else if(sum(head(input[,j], n = i - 1) < input[i,j]) == length(head(input[,j], n = i - 1)) |
             sum(tail(input[i,], n = length(input[i,])-j) < input[i,j]) == length(tail(input[i,], n = length(input[i,])-j)) |
             sum(tail(input[,j], n = length(input[,j])-i) < input[i,j]) == length(tail(input[,j], n = length(input[,j])-i)) |
             sum(head(input[i,], n = j - 1) < input[i,j]) == length(head(input[i,], n = j - 1))) {
      
      vis_count <- vis_count + 1
      
    }
    
  }
  
  if(i == nrow(input) & j == ncol(input)) {
    
    print(paste("The number of visible trees is:", vis_count, sep = " "))
    
  }
  
}

## Part 2

best_scene <- 0

for(i in 1:nrow(input)) {
  
  for(j in 1:ncol(input)) {
    
    if(i == 1 | j == 1 | i == nrow(input) | j == ncol(input)) {
      
      print("Not Enough Trees!")
      
    }else {
      up_score <- 0
      right_score <- 0
      down_score <- 0
      left_score <- 0
      
      #Up Score
      mat <- rev(head(input[,j], n = i - 1) < input[i,j])
      for(k in 1:length(mat)) {
        if(mat[k]) {
          up_score <- up_score + 1
        }else if(!mat[k]) {
          up_score <- up_score + 1
          break
        }
      }
      
      #Right Score
      mat <- tail(input[i,], n = length(input[i,])-j) < input[i,j]
      for(k in 1:length(mat)) {
        if(mat[k]) {
          right_score <- right_score + 1
        }else if(!mat[k]) {
          right_score <- right_score + 1
          break
        }
      }
      
      #Down Score
      
      mat <- tail(input[,j], n = length(input[,j])-i) < input[i,j]
      for(k in 1:length(mat)) {
        if(mat[k]) {
          down_score <- down_score + 1
        }else if(!mat[k]) {
          down_score <- down_score + 1
          break
        }
      }
      
      #Left Score
      mat <- rev(head(input[i,], n = j - 1) < input[i,j])
      for(k in 1:length(mat)) {
        if(mat[k]) {
          left_score <- left_score + 1
        }else if(!mat[k]) {
          left_score <- left_score + 1
          break
        }
      }
      
      curr_score <- up_score * right_score * down_score * left_score
      
      if(curr_score > best_scene) {
        best_scene <- curr_score
      }
    }
    
    if(i == nrow(input) & j == ncol(input)) {
      
      print(paste("The Best Scenery Score is:", best_scene, sep = " "))
      
    }
  }
}


