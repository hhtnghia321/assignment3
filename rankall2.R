rankall <- function(outcome, num = "best") {
  setwd("E:/Code/R")
  data.raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")       
  data.clean <- data.raw[c(2,7,11,17,23)]
  if (outcome == "heart attack"){   
    outcome <- "Heart.Attack"
  } else if (outcome == "heart failure"){
    outcome <- "Heart.Failure"
  } else if (outcome == "pneumonia") {
    outcome <- "Pneumonia"
  } else {stop("invalid outcome")
  }
  cmlresult = 0
  data.outcomefil <- data.clean[
    c("Hospital.Name","State",paste0("Hospital.30.Day.Death..Mortality..Rates.from.",outcome))
    ]
  data.state.rank <- data.outcomefil[order(data.outcomefil[2],data.outcomefil[3],data.outcomefil[1]),]
  data.state.rank[,3] <- suppressWarnings(as.numeric(data.state.rank[,3]))
  data.state.rank <- data.state.rank[complete.cases(data.state.rank[,3]),]
  state <- as.vector(as.character(unique(data.state.rank[,2])))
  for (ST in state) {
    data.state <- data.state.rank[data.state.rank[2]== ST,]
    if(num== "best"){
      result.state <- data.state[data.state[,3] == min(data.state[,3]),]
    } else if (num == "worst") {
      result.state <- data.state[data.state[,3] == max(data.state[,3]),]
    } else {result.state <- data.state[num,]
    }
    cmlresult <- rbind(cmlresult, result.state)
    
  }
  cmlresult <- cmlresult[complete.cases(cmlresult[,3]),]
  cmlresult <- cmlresult[-1,-3]
  final <- data.frame(hospital = cmlresult[,1], state = cmlresult[,2])
  final
} 