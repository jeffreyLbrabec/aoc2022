## Advent of Code Day 7

## I had no idea how to do this so this is Antoine Fabri's answer posted to Twitter!
## I was trying to do something like this with for loops but needless to say it was not this elegant and didn't go well!

library(tidyverse)
library(here)

input <- read_lines(here("day_seven/input.txt"))

done <- ongoing <- numeric()

for(line in input) {
  
  if(line == "$ ls") {
    
    ongoing <- c(ongoing, 0)
    
  } else if (grepl("^\\d", line)) {
    ongoing <- ongoing + as.numeric(gsub("\\D", "", line))
  } else if (line == "$ cd ..") {
    done <- c(done, tail(ongoing, 1))
    ongoing <- head(ongoing, -1)
  }
  
}

done <- c(ongoing, done)
part1 <- sum(done[done < 100000])

# part2

free <- 70000000 - done[1]
needed <- 30000000 - free
part2 <- min(done[done > needed])

  



