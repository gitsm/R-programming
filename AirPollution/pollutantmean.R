# Author      : Sridharan Muthuswamy
# Description : This function 'pollutantmean' calculates the mean of a pollutant 
#               (sulfate or nitrate) across a specified list of monitors. The 
#               function takes three arguments: 'directory', 'pollutant', and 'id'. 
#               Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
#               particulate matter data from the directory specified in the 'directory' 
#               argument and returns the mean of the pollutant across all of the monitors, 
#               ignoring any missing values coded as NA.
# Reference   : <README>

pollutantmean <- function(directory, pollutant, id=1:332) {
  psum <- 0
  plen <- 0
  for(i in id) {
    k <- if (i < 10) {
      paste("00",i,sep="")
    }
    else if (i >= 10 && i <= 99) {
      paste("0",i,sep="")
    }
    else {
      i
    }
    f <- paste(directory, "/", k,".csv",sep="")
    y <- read.csv(f)
    x <- y[pollutant]
    z <- x[!is.na(x)]
    l <- length(z)
    if (l > 0) {
      psum <- psum + sum(x[!is.na(x)])
    }
    plen <- plen + l
  }
  psum/plen
}
