setwd("~/Dokumente/Coding/AoC2022/R")
library(readr)
library(stringr)
library(sets)
library(tidyverse)

raw <- read_file("day04_example.data")
raw <- read_file("day04.data")

boundaries <- raw %>%
  str_split("\n", simplify = TRUE) %>%
  str_replace_all("-", ",") %>%
  str_split(",")
boundaries <- boundaries[1:length(boundaries)-1]

containsOther <- array()

for (i in 1:length(boundaries)) {
  first <- as.integer(boundaries[[i]][1])
  second <- as.integer(boundaries[[i]][2])
  firstSet <- as.set(first:second)
  first <- as.integer(boundaries[[i]][3])
  second <- as.integer(boundaries[[i]][4])
  secondSet <- as.set(first:second)
  full <- 0
  
  if (set_is_subset(firstSet, secondSet) | set_is_subset(secondSet, firstSet)) {
    full <- 1
  }
  
  containsOther[i] <- full
}

print(sum(containsOther))

# PART 2
intersections <- array()

for (i in 1:length(boundaries)) {
  first <- as.integer(boundaries[[i]][1])
  second <- as.integer(boundaries[[i]][2])
  firstSet <- as.set(first:second)
  first <- as.integer(boundaries[[i]][3])
  second <- as.integer(boundaries[[i]][4])
  secondSet <- as.set(first:second)
  full <- 0
  
  intersections[i] <- ifelse(length(set_intersection(firstSet, secondSet)) > 0, 1, 0)
}

#result
print(sum(intersections))
