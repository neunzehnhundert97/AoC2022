setwd("~/Dokumente/Coding/AoC2022/R")

library(readr)
library(tidyverse)
library(stringr)

raw <- read_file("day01.data")

#split raw into blocks
blocks <- str_split(raw, "\n\n", simplify = TRUE)

# split blocks into lists of strings
blocks <- str_split(blocks, "\n")

# remove emplty string elements
blocks <- lapply(blocks, function(z){ z[!is.na(z) & z != ""]})

#iterate over blocks to build sums
sums <- vector()

for (block in 1:length(blocks)) {
  numbers <- blocks[block]
  result <- sum(as.integer(unlist(numbers)))
  sums <- append(sums, result)
}

#find max
maxCalories <- max(sums)

#sum of the three highest sums
sumTopThree <- sums %>%
  sort %>%
  tail(3) %>%
  sum()
