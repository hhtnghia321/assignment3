best <- function(state, outcome, worst = FALSE) {
  setwd("E:/Code/R")
  data.raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")    
  data.clean <- data.raw[c(2,7,11,17,23)]
  
  if (outcome == "heart attack"){   ## Check that state and outcome are valid
    outcome <- "Heart.Attack"
  } else if (outcome == "heart failure"){
    outcome <- "Heart.Failure"
  } else if (outcome == "pneumonia") {
    outcome <- "Pneumonia"
  } else {stop("invalid outcome")}
  
  if (nrow(data.clean[data.clean$State == state,])==0 ){      
    stop("invalid State")
  }
  
  data.statefil <- data.clean[data.clean$State == state,]
  data.outcomefil <- data.statefil[
    c("Hospital.Name",paste0("Hospital.30.Day.Death..Mortality..Rates.from.",outcome))]
  data.outcomefil[,2] <-suppressWarnings(as.numeric(data.outcomefil[,2]))
  data.outcomefil <- data.outcomefil[!is.na(data.outcomefil[,2]),]
  
  if (worst == FALSE ) {
    out <- min(data.outcomefil[,2], na.rm = TRUE)
    result.unrate <- data.outcomefil[data.outcomefil[,2]== out,]  
    result.rated <- result.unrate[order(result.unrate[1]),]
    result <- result.rated[1,1]
    result
  } else if (worst == TRUE) {
    out <- max(data.outcomefil[,2], na.rm = TRUE)
    result.unrate <- data.outcomefil[data.outcomefil[,2]== out,]  
    result.rated <- result.unrate[order(result.unrate[1]),]
    result <- result.rated[1,1]
    result
  } else { 
    data.outcomefil
   
  }
  
}


rankhospital <- function(state,outcome, num = "best") {
  setwd("E:/Code/R")
  data.raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")       
  data.clean <- data.raw[c(2,7,11,17,23)]
  if (num == "best") {
    result1 <-best(state, outcome)
    print(result1)
  } else if (num == "worst") {
    result1 <-best(state, outcome, worst = TRUE)
    print(result1)
  } else {
    result1 <- best(state, outcome)
    if (is.null(result1) == TRUE) {
      result1
    } else {
      data.outcomefil <- best(state, outcome, worst = num)
      
      data.ranked <- data.outcomefil[order(data.outcomefil[2]),]
      data.uni <-as.vector(unique(data.ranked[,2]))
      cmldata =0
      for ( ID in 1:length(data.uni)){
        data.outcomefil2 <- data.ranked[data.ranked[,2] == data.uni[ID],]
        data.ranked2 <- data.outcomefil2[order(data.outcomefil2[1]),]
        cmldata <- rbind(cmldata,data.ranked2)
      }
      cmldata[-1,][num,1]
    }
  }   
}

rankall <- function(outcome, num = "best") {
  setwd("E:/Code/R")
  data.raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")       
  data.clean <- data.raw[c(2,7,11,17,23)]
  if (outcome == "heart attack"){   ## Check that state and outcome are valid
    outcome <- "Heart.Attack"
  } else if (outcome == "heart failure"){
    outcome <- "Heart.Failure"
  } else if (outcome == "pneumonia") {
    outcome <- "Pneumonia"
  } else {stop("invalid outcome")}
  
  cmlstate = 0
  cmlall = 0
  data.outcomefil <- data.clean[
    c("Hospital.Name","State",paste0("Hospital.30.Day.Death..Mortality..Rates.from.",outcome))
    ]
  data.state.rank <- data.outcomefil[order(data.outcomefil[2]),]
  statedata <- as.vector(unique(data.state.rank[2]))
  for (ST in statedata) {
    data.statefil <- data.state.rank[data.state.rank[2] == ST,]
    data.outcomerank <- data.statefil[order(data.statefil[3]),]
    outcomedata <- as.vector(unique(data.outcomerank[3]))
    for (LT in 1:length(outcomedata)) {
      data.namerank <- data.outcomerank[data.outcomerank[3]== outcomedata[LT]]
      data.namerank <- data.namerank[order(data.namerank[1]),]
      cmlstate <- rbind(cml1,data.namerank)
    }
    cmlall <- rbind(cmlall,cmlstate[-1,][num,])
    
  }
}

