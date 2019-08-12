pollutantmean <- function(directory, pollutant, id=1:332) {
  outp <- vector()
  for(i in id){
    filename <- sprintf("%03d.csv", i)
    file <- c(directory, filename, sep='/')
    data <- read.csv(file)
    d <- data[,pollutant]
    outp <- c(outp, d)
  }
  ans <- mean(outp, na.rm = TRUE)
  ans
}
