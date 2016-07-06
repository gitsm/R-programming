# Author      : Sridharan Muthuswamy
# Description : This function takes three arguments: the 2-character abbreviated name 
#               of a state (state), an outcome (outcome), and the ranking of a hospital 
#               in that state for that outcome (num). The function reads the this file 
#               outcome-of-care-measures.csv and returns a character vector with the name 
#               of the hospital that has the ranking specified by the num argument. For 
#               example, the call rankhospital("MD", "heart failure", 5) would return a 
#               character vector containing the name of the hospital with the 5th lowest 
#               30-day death rate for heart failure. The num argument can take values “best”, 
#               “worst”, or an integer indicating the ranking (smaller numbers are better). 
#               If the number given by num is larger than the number of hospitals in that state, 
#               then the function returns NA. Hospitals that do not have data on a particular 
#               outcome are excluded from the set of hospitals when deciding the rankings. It 
#               may occur that multiple hospitals have the same 30-day mortality rate for a 
#               given cause of death. In those cases ties are broken by using the hospital name.
#               The function checks the validity of its arguments. If an invalid state value 
#               is passed to best, the function throws an error via the stop function with the 
#               exact message “invalid state”. If an invalid outcome value is passed to best, 
#               the function throws an error via the stop function with the exact message 
#               “invalid outcome”.
# Reference   : ProgAssignment3v2.pdf (Prof. Roger D. Peng, Johns Hopkins University, MD)

options(warn=-1)
rankhospital <- function(state, outcome, num = "best") {
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
  
  if (as.character(num) != "best" && as.character(num) != "worst") {
    if (!is.numeric(num)) {
      stop("invalid rank")
    } 
  }

  input[,colname] <- as.numeric(input[,colname])
  xa <- input[order(input[,colname], input[,"Hospital.Name"], na.last = NA),]
  xb <- xa[xa["State"]==state,]
  len <- length(xb[,1])

  if (is.numeric(num) && len < as.integer(num)) {
    return(NA)
  }

  if (as.character(num) == "best") {
    index <- 1
  } 
  else if (as.character(num) == "worst") {
    index <- len
  }
  else {
    index <- as.integer(num)
  }
   
  xb[index,"Hospital.Name"]
}
