best <- function(state, outcome) {
  measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  names <- measures[,2]

  states <- measures[,7]
  if (! (state %in% states)) {
    stop("invalid state")
  }

  column <- if (outcome == "heart attack") {
      11
  } else if (outcome == "heart failure") {
      17
  } else if (outcome == "pneumonia") {
      23
  } else {
      stop("invalid outcome")
  }

  measure <- measures[,column]
  
  nas <- measure == "Not Available"
  measure[nas] <- NA
  
  measure <- as.numeric(measure)

  choices <- (states == state) & !is.na(measure)
  
  best_measure <- min(measure[choices])

  names[choices & measure == best_measure][1]
}