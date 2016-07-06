# Author      : Sridharan Muthuswamy
# Description : This function 'complete' reads a directory full of files and 
#               reports the number of completely observed cases in each data file. 
#               The function returns a data frame where the first column is the 
#               name of the file and the second column is the number of complete cases.
# Reference   : <README>

complete <- function(directory, id = 1:332) {
  mat <- matrix(0, nrow=length(id), ncol=2)
  j <- 1
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
    z <- y[!is.na(y["nitrate"]) & !is.na(y["sulfate"]),]
    mat[j,1] <- i
    mat[j,2] <- length(z[,2])
    j <- j + 1
  }
  colnames(mat) <- c("id","nobs")
  as.data.frame(mat)
}
