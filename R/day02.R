setwd("~/Dokumente/Coding/AoC2022/R")

raw <- read_file("day02_example.data")
raw <- read_file("day02.data")

rounds <- str_split(raw, "\n", simplify = TRUE)
rounds <- str_split(rounds, " ")
rounds <- rounds[1:(length(rounds)-1)]

scoring <- function(x) {
  score <- 0
  
  if (x[2] == "X") {
    score <- score + 1
    
    if (x[1] == "A") {
      score <- score + 3
    } else if (x[1] == "B") {
      score <- score + 0
    } else if (x[1] == "C") {
      score <- score + 6
    }
  } else if (x[2] == "Y") {
    score <- score + 2
    
    if (x[1] == "A") {
      score <- score + 6
    } else if (x[1] == "B") {
      score <- score + 3
    } else if (x[1] == "C") {
      score <- score + 0
    }
  } else if (x[2] == "Z") {
    score <- score + 3
    
    if (x[1] == "A") {
      score <- score + 0
    } else if (x[1] == "B") {
      score <- score + 6
    } else if (x[1] == "C") {
      score <- score + 3
    }
  }
  return(score)
}

scores <- vector()

for (x in 1:length(rounds)) {
  scores[x] <- scoring(rounds[[x]])
}

totalScore <- sum(scores)

# PART TWO

realScoring <- function(x) {
  score <- 0
  
  if (x[1] == "A") {
    if (x[2] == "X") {
      score <- score + 3
    } else if (x[2] == "Y") {
      score <- score + 4
    } else if (x[2] == "Z") {
      score <- score + 8
    }
  } else if (x[1] == "B") {
    if (x[2] == "X") {
      score <- score + 1
    } else if (x[2] == "Y") {
      score <- score + 5
    } else if (x[2] == "Z") {
      score <- score + 9
    }
  } else if (x[1] == "C") {
    if (x[2] == "X") {
      score <- score + 2
    } else if (x[2] == "Y") {
      score <- score + 6
    } else if (x[2] == "Z") {
      score <- score + 7
    }
  }
}

scores <- vector()

for (x in 1:length(rounds)) {
  scores[x] <- realScoring(rounds[[x]])
}

totalScore <- sum(scores)