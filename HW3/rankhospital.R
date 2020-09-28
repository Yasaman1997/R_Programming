rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
## Check that state and outcome are valid
  if(!any(data$State == state)) {
    stop("invalid state")
  }

  colIndex <- integer(0)
  if('heart attack' == outcome)
    colIndex <- 11
  else if('heart failure' == outcome)
    colIndex <-  17
  else if('pneumonia' == outcome)
    colIndex <- 23
  else {
    stop("invalid outcome")
  }
  
## Return hospital name in that state with the given rank
## 30-day death rate
  ## Select the appropriate State
  data.state <- data[ which(data$State == state), ]
  ## Cast the state's outcome column to numeric
  data.state[ ,colIndex] <- as.numeric(data.state[ ,colIndex])
  ## Choose only complete data in the state
  data.state <- data.state[complete.cases(data.state), ]  
  ## Sort The hospitals
  bestHospitals <- data.state[order(data.state[,colIndex], data.state$Hospital.Name), ]
 
  ##Find the levels, just for information
  levels <- unique(bestHospitals[, colIndex])
  
  selection <- integer(0)
  if(num == "best") {
    selection <- 1
  }
  else if(num == "worst") {
    selection <- nrow(bestHospitals)
  }
  else {
    selection <- num
  }
  
  if(nrow(bestHospitals) < selection)
  {
    return(NA)
  }
  
  ##get the hospital name in the alphabetical order
  bestHospitals$Hospital.Name[selection]
}