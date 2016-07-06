# Author      : Sridharan Muthuswamy
# Description : This function takes two arguments: the 2-character abbreviated name of a state 
#               and an outcome name. The function reads the outcome-of-care-measures.csv file and 
#               returns a character vector with the name of the hospital that has the best (i.e. 
#               lowest) 30-day mortality for the specified outcome in that state. The hospital name 
#               is the name provided in the Hospital.Name variable. The outcomes can be one of 
#               “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data 
#               on a particular outcome are excluded from the set of hospitals when deciding the
#               rankings. If there is a tie for the best hospital for a given outcome, then the 
#               hospital names are sorted in alphabetical order and the first hospital in that set 
#               is chosen. This function also checks the validity of its arguments. If an invalid 
#               state value is passed to best, the function throws an error via the stop function 
#               with the exact message “invalid state”. If an invalid outcome value is passed to best, 
#               the function should throws an error via the stop function with the exact message 
#               “invalid outcome”.
# Reference   : ProgAssignment3v2.pdf (Prof. Roger D. Peng, Johns Hopkins University, MD)

options(warn=-1)
best <- function(state, outcome) {
  input <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!(state %in% input$State)) {
    stop("invalid state")
  }
  if (outcome == "heart attack") {
    colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  else if (outcome == "heart failure") {
    colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  else if (outcome == "pneumonia") {
    colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  else {
    stop("invalid outcome")
  }
  input[,colname] <- as.numeric(input[,colname])
  xa <- input[order(input[,colname], na.last = NA),]
  xb <- xa[xa["State"]==state,]
  mv1 <- xb[1,colname]
  for(i in 2:length(xb[,colname])) {
    mv2 <- xb[i,colname]
    if (mv2 != mv1) break;
  }
  hosp_names <- xb["Hospital.Name"]
  hn <- sort(hosp_names[1:i-1,])
  hn
}
