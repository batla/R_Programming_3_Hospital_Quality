best <- function(state, outcome) {
  # Read outcome data
  # Check that state and outcome are valid
  # Return hospital name in that state with lowest 30-day death
  # rate
  
  # Download data to allow others to run script w/o need to get data first
  # specify data directory from current working directory
  # requires internet connection
  
  #if(!file.exists("./data")){dir.create("./data")}
  #fileUrl <- 
   # "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
  #download.file(fileUrl,destfile="./data/ProgAssignment3-data.zip",method="curl")
    
  ## unzip file in "data" directory
  #unzip(zipfile="./data/ProgAssignment3-data.zip",exdir="./data")
  
  ## get the full path to files recursively - used with read.table() below
  ## NOTE: code assumes level below "data" directory name is "rprog-data-ProgAssignment3-data"
  ## this is known only after Dataset.zip has been unzipped
  #filePath <- file.path("./data")
  
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
  } # heart attack
  else if (outcome == validOutcomes[2]) {
    outcomeCol <- 17
  } # heart failure
  else {
    outcomeCol <- 23
  } # pneumonia

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