## Advent of Code day 6

library(tidyverse)
library(here)

#input <- unlist(str_split("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", ""))

input <- unlist(str_split(read_lines(here("day_six/input.txt")), ""))

## Part One

for(i in 1:length(input)) {
  
  if(sum(input[i:(i+3)] %in% input[i]) > 1 | 
     sum(input[i:(i+3)] %in% input[i+1]) > 1 | 
     sum(input[i:(i+3)] %in% input[i+2]) > 1) {
    
    print("Not Marker")
    
  } else{
    
    print(paste("The Marker is:", paste(input[i:i+3]), sep = " "))
    print(paste("It occured after index", i+3, paste(" ")))
    break
    
  }
  
}


## Part Two

for(i in 1:length(input)) {
  
  if(sum(input[i:(i+13)] %in% input[i]) > 1 | 
     sum(input[i:(i+13)] %in% input[i+1]) > 1 | 
     sum(input[i:(i+13)] %in% input[i+2]) > 1 |
     sum(input[i:(i+13)] %in% input[i+3]) > 1 |
     sum(input[i:(i+13)] %in% input[i+4]) > 1 |
     sum(input[i:(i+13)] %in% input[i+5]) > 1 |
     sum(input[i:(i+13)] %in% input[i+6]) > 1 |
     sum(input[i:(i+13)] %in% input[i+7]) > 1 |
     sum(input[i:(i+13)] %in% input[i+8]) > 1 |
     sum(input[i:(i+13)] %in% input[i+9]) > 1 |
     sum(input[i:(i+13)] %in% input[i+10]) > 1 |
     sum(input[i:(i+13)] %in% input[i+11]) > 1 |
     sum(input[i:(i+13)] %in% input[i+12]) > 1) {
    
    print("Not Marker")
    
  } else{
    
    print(paste("The Marker is:", paste(input[i:i+13]), sep = " "))
    print(paste("It occured after index", i+13, paste(" ")))
    break
    
  }
  
}
