corr <- function(directory, threshold=0){
  corr <- vector()
  c <- complete(directory)
  for(i in 1:dim(c)[1]){
    if(c$n[i] > threshold){
      filename <- sprintf("%03d.csv", c$id[i])
      file <- c(directory, filename, sep='/')
      data <- read.csv(filename)
      completerow <- data[complete.cases(data),]
      corr <- c(corr, cor(completerow$nitrate, completerow$sulfate))
    }
  }
  corr
}