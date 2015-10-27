rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  # Check that state and outcome are valid
  # Return hospital(s) given rank - rank can be string: e.g. "best", "worst",
  # or number, or vector of numbers

  # load_validate_hospital_quality_data returns a list:
  # state, outcome, outcomes, validStatess, validOutcomes
  r <- tryCatch({ load_validate_hospital_quality_data <- 
               load_validate_hospital_quality_data(state,outcome)
             }, error = function(e) {
               e
             })
  
  ## read data structures from valid_state_outcome into variables
  state <- load_validate_hospital_quality_data[[1]]
  outcome <- load_validate_hospital_quality_data[[2]]
  outcomes <- load_validate_hospital_quality_data[[3]]
  validStates <- load_validate_hospital_quality_data[[4]]
  validOutcomes <- load_validate_hospital_quality_data[[5]]
  
  ## if input is valid, continue processing
  ## subset the outcome data.frame with requested state
  stateOutcomes <- subset(outcomes, State == state)
  
  ## get the requested outcome column - we do this to know which
  ## column to clean up
  ## must have inspected csv file to know
  ## which column numbers, look for: 
  ## "Hospital 30-Day Death...rates from 'outcome'" only;
  ## not "Comparisons", "Estimates", etc...
  
  if (outcome == validOutcomes[1]) {
    outcomeCol <- 11
  } ## heart attack
  else if (outcome == validOutcomes[2]) {
    outcomeCol <- 17
  } ## heart failure
  else {
    outcomeCol <- 23
  } ## pneumonia
  
  ## check if rank input exceeds number of hospitals in the state, if so,
  ## return NA
  ## if rank is not numeric, ignore
  
  if (is.numeric(num) == TRUE) {
    if (length(outcomes[,2]) < num) {
      return(NA)
    }
  }
  
  ## remove NA's from outcome data
  ## resulting data.frame has only states with NA's removed from
  ## desired outcomes dataset
  stateOutcomes[,outcomeCol] <- as.numeric(stateOutcomes[,outcomeCol])
  naRecords <- is.na(stateOutcomes[,outcomeCol])
  desiredOutcomes <- stateOutcomes[!naRecords,]
  
  ## order desiredOutcomes by ascending outcome values
  ## essentially ranking them
  outcomeColumnName <- names(desiredOutcomes)[outcomeCol]
  hospitalColumnName <- names(desiredOutcomes)[2]
  index <- with(desiredOutcomes, order(desiredOutcomes[outcomeColumnName], desiredOutcomes[hospitalColumnName]))
  orderedDesiredOutcomes <- desiredOutcomes[index, ]
  
  ## give "best" or "worst" a numerical value, best = 1; worst = n
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(orderedDesiredOutcomes[, outcomeCol])
    }
  }
  ## return the hospital name(s) corresponding to ranking
  ## will return multiple if num = 1:n - best to worst
  ## or if num = n:1 - worst to best
  orderedDesiredOutcomes[num, 2]
}