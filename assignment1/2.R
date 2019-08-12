complete <- function(directory, id = 1:332){
  id_v <- vector()
  n <- vector()
  for(i in id){
    filename <- sprintf("%03d.csv", i)
    file <- c(directory, filename, sep='/')
    data <- read.csv(filename)
    id_v <- c(id_v, i)
    completerow <- data[complete.cases(data),]
    n <- c(n, nrow(completerow))
  }
  df <- data.frame(id=id_v, n=n)
  df
}