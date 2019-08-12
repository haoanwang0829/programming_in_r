rankall <- function(outcome, num = 'best'){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_list <- vector(mode="list", length=3)
  names(outcome_list) <- c("heart attack","heart failure","pneumonia")
  if(outcome %in% names(outcome_list) == FALSE) stop("invalid outcome")
  outcome_list[[1]] <- 11; outcome_list[[2]] <- 17; outcome_list[[3]] <- 23
  col_index <- outcome_list[[outcome]]
  
  statedata <- data[, c(2,7,col_index)]
  statedata[, 3] <- as.numeric(x=statedata[, 3])
  statedata <- statedata[complete.cases(statedata), ]
  
  if(num == 'best') num <- 1
  if(num == 'worst') num <- dim(statedata)[1]
  
  output <- list()
  statelist <- sort(unique(statedata$State))
  for(state in statelist){
    temp <- statedata[statedata$State == state,]
    target<-temp[order(temp[,3], temp[,1]),][num,1]
    output <- rbind(output, list(target, state))
  }
  output
}
