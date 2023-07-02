setwd("~/Dokumente/Coding/AoC2022/R")
library(readr)
library(stringr)
library(sets)

raw <- read_file("day03_example.data")
raw <- read_file("day03.data")

rucksacks <- str_split(raw, "\n", simplify = TRUE)

splitFirstHalf <- function(x) {
  first <- substr(x, 1, nchar(x)/2)
  return(first)
}

splitSecondHalf <- function(x) {
  second <- substr(x, (nchar(x)/2)+1, nchar(x))
  return(second)
}

compartments <- data.frame(number = 1:length(rucksacks))
compartments$first <- apply(X = rucksacks, MARGIN = 2, FUN = splitFirstHalf)
compartments$second <- apply(X = rucksacks, MARGIN = 2, FUN = splitSecondHalf)

findDoubles <- function(x) {
  first <- strsplit(x["first"], split = character(0))[[1]]
  second <- x["second"]
  
  found <- NA
  
  for (c in first) {
    if (str_detect(second, c)) {
      found <- c
    }
  }
  return(found)
}

compartments$doubles <- apply(X = compartments, MARGIN = 1, FUN = findDoubles)

determinePriority <- function(x) {
  double <- x["doubles"]
  
  prio <- match(double, letterPriority)
}

letterPriority <- c(letters, LETTERS)

compartments$priorities <- apply(X = compartments, MARGIN = 1, FUN = determinePriority)

#solution
print(sum(compartments$priorities, na.rm = TRUE))

# PART 2

groups <- data.frame(number = 1:(length(rucksacks)/3))

n <- 1:(length(rucksacks)-1)
firsts <- n[n %% 3 == 1]
groups$first <- rucksacks[firsts]
seconds <- n[n %% 3 == 2]
groups$second <- rucksacks[seconds]
thirds <- n[n %% 3 == 0]
groups$third <- rucksacks[thirds]

findBadge <- function(x) {
  first <- as.set(as.list(str_split(x["first"], "", simplify = TRUE)))
  second <- as.set(as.list(str_split(x["second"], "", simplify = TRUE)))
  third <- as.set(as.list(str_split(x["third"], "", simplify = TRUE)))
  
  return(as.character(set_intersection(first, second, third)))
}

groups$doubles <- apply(X = groups, MARGIN = 1, FUN = findBadge)
groups$priority <- apply(X = groups, MARGIN = 1, FUN = determinePriority)

#solution
print(sum(groups$priority, na.rm = TRUE))
