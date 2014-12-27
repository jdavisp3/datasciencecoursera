getcolumn <- function(outcome) {
  if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else if (outcome == "pneumonia") {
    23
  } else {
    stop("invalid outcome")
  }
}

rankhospital <- function(state, outcome, num = "best") {
  measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  states <- measures[,7]

  if (! (state %in% states)) {
    stop("invalid state")
  }

  measure <- measures[,getcolumn(outcome)]
  measure[measure == "Not Available"] <- NA
  measure <- as.numeric(measure)

  choices <- (states == state) & !is.na(measure)

  measure <- measure[choices]
  names <- measures[,2][choices]
  ranks <- order(measure, names)

  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- length(ranks)
  }

  names[ranks[num]]
}