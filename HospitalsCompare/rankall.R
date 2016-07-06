# Author      : Sridharan Muthuswamy
# Description : This function takes two arguments: an outcome name (outcome) 
#               and a hospital ranking (num). The function reads the file 
#               outcome-of-care-measures.csv file and returns a 2-column data 
#               frame containing the hospital in each state that has the ranking 
#               specified in num. For example the function call rankall("heart 
#               attack", "best") returns a data frame containing the names of the
#               hospitals that are the best in their respective states for 30-day 
#               heart attack death rates. The function returns a value for every 
#               state (some may be NA). The first column in the data frame is named 
#               hospital, which contains the hospital name, and the second column is 
#               named state, which contains the 2-character abbreviation for the state 
#               name. Hospitals that do not have data on a particular outcome are excluded 
#               from the set of hospitals when deciding the rankings. The function handles
#               ties in the 30-day mortality rates in the same way that the rankhospital 
#               function does. The function checks the validity of its arguments. If an 
#               invalid outcome value is passed to rankall, the function throws an error 
#               via the stop function with the exact message “invalid outcome”. The num 
#               variable can take values “best”, “worst”, or an integer indicating the 
#               ranking (smaller numbers are better). If the number given by num is larger 
#               than the number of hospitals in that state, then the function returns NA. 
# Reference   : ProgAssignment3v2.pdf (Prof. Roger D. Peng, Johns Hopkins University, MD)

options(warn=-1)
rankall <- function(outcome, num = "best") {
  Hospital <- character()
  State <- character()
  input <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  for(state in sort(unique(xa[,"State"]))) {
    xb <- xa[xa["State"]==state,]
    len <- length(xb[,1])
    if (is.numeric(num) && len < as.integer(num)) {
      Hospital <- c(Hospital, NA)
      State <- c(State, state)
      next()
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
    Hospital <- c(Hospital, xb[index,"Hospital.Name"])
    State <- c(State, xb[index,"State"])
  }
  df <- data.frame(Hospital, State)
  names(df) <- c("hospital","state")
  df
}
