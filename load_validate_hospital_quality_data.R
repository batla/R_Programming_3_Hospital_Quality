load_validate_hospital_quality_data <- function(state, outcome) {
  
  # this function loads hospital quality date for the R Programming
  # Assignment #3 Hospital Quality
  # It also validates state and outcome inputs
  # All three parts of the assignment call this function
  
  # The function stops if either state or outcome are invalid inputs; otherwise
  # the function returns a list of data structures: state, outcome, outcomes,
  # validStates, validOutcomes
  
  ## for a more user-friendly interaction
  ## converting state input to uppercase - State in data is uppercase
  ## converting outcome input to lowercase
  state <- toupper(state)
  outcome <- tolower(outcome)
  
  ## read outcomes dataset
  outcomes <- read.csv(file.path("data/outcome-of-care-measures.csv"),
                       colClasses = "character")
  
  ## create vector of valid states to check for valid state input
  ## after inspecting outcome, col index for State is 7
  validStates <- outcomes[,7]
  
  ## create vector of valid outcomes to check for valid outcome input
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## test state & outcome input, stop processing if either is invalid
  if ((state %in% validStates) == FALSE) {
    stop(print("Invalid state Entered. Try again."))
  }
  else if ((outcome %in% validOutcomes) == FALSE) {
    stop(print("Invalid outcome entered. Try again."))
  }
  
  # if inputs are ok, return list of all data structures required by calling
  # function
  list(state, outcome, outcomes, validStates, validOutcomes)
  
  
}