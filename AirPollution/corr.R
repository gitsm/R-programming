# Author      : Sridharan Muthuswamy
# Description : This function 'corr' takes a directory of data files and 
#               a threshold for complete cases and calculates the correlation 
#               between sulfate and nitrate for monitor locations where the 
#               number of completely observed cases (on all variables) is greater 
#               than the threshold. The function returns a vector of correlations 
#               for the monitors that meet the threshold requirement. If no monitors 
#               meet the threshold requirement, then the function returns a numeric 
#               vector of length 0.
# Reference:    <README>

corr <- function(directory, threshold = 0) {
  pcor <- numeric()
  df <- complete(directory)
  x <- df[["id"]]
  j <- 1
  for(n in df[["nobs"]]) {
    i <- x[j]
    if (n > threshold) {
      k <- if (i < 10) {
        paste("00", i, sep="")
      }
      else if (i >= 10 && i <= 99) {
        paste("0", i, sep="")
      }
      else {
        i
      }
      f <- paste(directory, "/", k,".csv",sep="")
      y <- read.csv(f)
      z <- y[!is.na(y["nitrate"]) & !is.na(y["sulfate"]),]
      v1 <- z[["nitrate"]]
      v2 <- z[["sulfate"]]
      pcor <- c(pcor, cor(v1,v2))
    }
    j <- j + 1
  }
  pcor
}
