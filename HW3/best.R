best <- function(state, outcome) {
## Read outcome data
  data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
## Check that state and outcome are valid
  if(!any(data['State'] == state)) {
     error <- paste("Error in best('", state,"', '", outcome, "') : Invalid state", sep="")
     stop(error)
  }

  colIndex <- integer(0)
  if('heart attack' == outcome)
    colIndex <- 11
  else if('heart failure' == outcome)
    colIndex <-  17
  else if('pneumonia' == outcome)
    colIndex <- 23
  else {
    error <- paste("Error in best('", state,"', '", outcome, "') : Invalid outcome", sep="")
    stop(error)
  }  

## Return hospital name in that state with lowest 30-day death
## rate
  byState <-data[data$State == state,]
  byState[, colIndex] <- as.numeric(byState[,colIndex])
  byState <- byState[complete.cases(byState), ]
  byState[which(byState[,colIndex] == min(byState[,colIndex])),][,2][[1]]
}
