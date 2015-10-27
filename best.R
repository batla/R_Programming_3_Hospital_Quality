best <- function(state, outcome) {
  
  # Reads outcome data
  # Checks that state and outcome are valid
  # Return hospital name in that state with lowest 30-day death rate
  
  # load_validate_hospital_quality_data returns a list:
  # state, outcome, outcomes, validStates, validOutcomes
  r <- tryCatch({ load_validate_hospital_quality_data <- 
                    load_validate_hospital_quality_data(state,outcome)
  }, error = function(e) {
    e
  })
  if(!inherits(r, "error"))
    stop("'rankhospital' should throw an error via 'stop' in this case")
  tolower(conditionMessage(r))
  
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
  
  ## remove NA's from outcome data
  ## resulting data.frame has only states with NA's removed from
  ## desired outcomes dataset
  desiredOutcomeCols <- as.numeric(stateOutcomes[,outcomeCol])
  naRecords <- is.na(desiredOutcomeCols)
  desiredOutcomes <- stateOutcomes[!naRecords,] 
  ## find hospitals with lowest outcome value, e.g. lowest death rate(s)
  ## lowest rate is best
  columnsConsidered <- as.numeric(desiredOutcomes[,outcomeCol])
  rowsConsidered <- which(columnsConsidered == min(columnsConsidered))
  
  ## could be more than one hospital, hospital name is column 2
  bestHospitals <- desiredOutcomes[rowsConsidered, 2]
  
  ## handling multiple hospitals, requirement is to return first hospital
  ## in alpha order if more than one share the lowest rate
  if (length(bestHospitals) > 1 ) {
    bestHospitalsSorted <- sort(bestHospitals)
    bestHospitalsSorted[1]
  }
  else {
    bestHospitals
  }
}