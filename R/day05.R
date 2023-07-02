setwd("~/Dokumente/Coding/AoC2022/R")
library(readr)
library(stringr)
library(tidyverse)

raw <- read_file("day05_example.data")
raw <- read_file("day05.data")

raw <- str_split(raw, "\n\n")
config <- raw[[1]][1]
moves <- raw[[1]][2]

# prepare stack data
config <- config %>%
  str_split("\n", simplify = TRUE) %>%
  str_split("")

stacks <- list()
n <- 1:(length(config[[1]]))
n <- n[n %% 4 == 2]

for (index in n) {
  templist <- list()
  
  for (i in 1:(length(config)-1)) {
    if (config[[length(config)-i]][index] != " ") {
      templist[[i]] <- config[[length(config)-i]][index]
    }
  }
  stacks[[as.character(match(index, n))]] <- templist
}

# prepare move data
moves <- moves %>%
  str_split("\n", simplify = TRUE) %>%
  str_split(" ")

number <- c()
fromStack <- c()
toStack <- c()
for (i in 1:(length(moves)-1)) {
  number[i] <- as.integer(moves[[i]][2])
  fromStack[i] <- as.integer(moves[[i]][4])
  toStack[i] <- as.integer(moves[[i]][6])
}
moves <- data.frame(number = number, from = fromStack, to = toStack)

# apply moves to stack
crate <- NA

for (row in 1:nrow(moves)) {
  number <- moves$number[row]
  from <- moves$from[row]
  to <- moves$to[row]
  
  for (times in (1:number)) {
    crate <- stacks[[from]][length(stacks[[from]])]
    stacks[[from]][length(stacks[[from]])] <- NULL
    stacks[[to]][length(stacks[[to]])+1] <- crate
  }
}

# result
message <- c()
for (stack in 1:length(stacks)) {
  message[stack] <- stacks[[stack]][length(stacks[[stack]])]
}
print(str_flatten(message))

# PART 2

crates <- NA

for (row in 1:nrow(moves)) {
  number <- moves$number[row]
  from <- moves$from[row]
  to <- moves$to[row]
  
  crates <- stacks[[from]][(length(stacks[[from]])-number+1):length(stacks[[from]])]
  stacks[[from]][(length(stacks[[from]])-number+1):length(stacks[[from]])] <- NULL
  
  stacks[[to]][(length(stacks[[to]])+1):(length(stacks[[to]])+number)] <- crates
}

# result
message <- c()
for (stack in 1:length(stacks)) {
  message[stack] <- stacks[[stack]][length(stacks[[stack]])]
}
print(str_flatten(message))
