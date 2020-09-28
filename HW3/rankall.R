rankall <- function(outcome, num = "best") {
## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
## Check that state and outcome are valid
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
  ## Cast outcome column to numeric
  data[ ,colIndex] <- as.numeric(data[ ,colIndex])
  ## Hospitals that do not have data on a particlar outcome chould be excluded
  data <- data[complete.cases(data), ]  
  ## Sort The hospitals, probably a Radix sort underneath 
  bestHospitals <- data[order(data$State, data[,colIndex], data$Hospital.Name), ]
 
  ##Find the levels, just for information, 7 being State
  statelevels <- factor(bestHospitals[ , 7])

  ranks <- list()
  if(num == "best") {
    ranks <- tapply(bestHospitals[['Hospital.Name']], statelevels, function(name) { return(name[1]) })
  }
  else if(num == "worst") {
    ranks  <- tapply(bestHospitals[['Hospital.Name']], statelevels, function(name) { return(name[length(name)]) })
  }
  else {
    ranks <- tapply(bestHospitals[['Hospital.Name']], statelevels, function(name) { return(name[num]) })
  }
  
  ##get the hospital name in the alphabetical order
  data.frame(hospital = ranks, state = names(ranks))
}