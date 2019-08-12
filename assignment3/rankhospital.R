rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(state %in% data$State == FALSE) stop("invalid state")
  
  outcome_list <- vector(mode="list", length=3)
  names(outcome_list) <- c("heart attack","heart failure","pneumonia")
  if(outcome %in% names(outcome_list) == FALSE) stop("invalid outcome")
  outcome_list[[1]] <- 11; outcome_list[[2]] <- 17; outcome_list[[3]] <- 23
  col_index <- outcome_list[[outcome]]
  
  statedata <- data[data$State == state, c(2, col_index)]
  statedata[, 2] <- as.numeric(x=statedata[, 2])
  statedata <- statedata[complete.cases(statedata), ]
  
  if((typeof(num) == 'character') && (!num %in% c('best','worst'))) NA
  if(num > dim(statedata)[1]) NA
  
  if(num == 'best') num <- 1
  if(num == 'worst') num <- dim(statedata)[1]
  statedata[order(statedata[,2], statedata[,1]),][num,1]

  
}
