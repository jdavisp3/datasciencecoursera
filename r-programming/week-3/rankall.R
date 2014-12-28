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

rankall <- function(outcome, num = "best") {
  measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  states <- measures[,7]

  measure <- measures[,getcolumn(outcome)]
  measure[measure == "Not Available"] <- NA
  measure <- as.numeric(measure)

  measure_by_state <- split(measure, states)
  name_by_state <- split(measures[,2], states)
  choices_by_state <- mapply(function(measure) {!is.na(measure)}, measure_by_state)

  get_rank <- function (choices, measure, name) {
    order(measure[choices], name[choices])
  }

  rank_by_state <- mapply(get_rank, choices_by_state, measure_by_state, name_by_state)

  get_nth <- function(choices, rank, name) {
    
    index <- if (num == "best") {
      1
    } else if (num == "worst") {
      length(rank)
    } else {
      num
    }

    name[choices][rank[index]]
  }

  nth_by_state <- mapply(get_nth, choices_by_state, rank_by_state, name_by_state)

  data.frame(hospital = nth_by_state, states=names(nth_by_state))
}