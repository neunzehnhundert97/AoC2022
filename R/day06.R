setwd("~/Dokumente/Coding/AoC2022/R")
library(readr)
library(stringr)
library(tidyverse)
library(sets)

raw <- read_file("day06_example.data")
raw <- str_split(raw, "\n")[[1]][[3]]

raw <- read_file("day06.data")

i <- 4
markerFound <- FALSE

while (markerFound == FALSE) {
  if (substr(raw, i, i) != substr(raw, i-1, i-1) & substr(raw, i, i) != substr(raw, i-2, i-2) & substr(raw, i, i) != substr(raw, i-3, i-3) &
      substr(raw, i-1, i-1) != substr(raw, i-2, i-2) & substr(raw, i-1, i-1) != substr(raw, i-3, i-3) &
      substr(raw, i-2, i-2) != substr(raw, i-3, i-3)) {
    print(i)
    markerFound <- TRUE
  }
  i <- i + 1
}

# PART 2

raw <- "bvwbjplbgvbhsrlpgdmjqwftvncz"

i <- 14
markerFound <- FALSE
while (markerFound == FALSE) {
  currentSet <- as.set(as.list(str_split(substr(raw, i-13, i), "", simplify = TRUE)))
  if (set_cardinality(currentSet) == 14) {
    markerFound <- TRUE
    print(i)
  }
  i <- i + 1
}
