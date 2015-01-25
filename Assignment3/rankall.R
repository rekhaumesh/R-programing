rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

## Read outcome data
outcomeCare<-read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings=c("Not Available"))
valid_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
if (!is.element(state, valid_states)) stop("invalid state")
if (!is.element(outcome, valid_outcomes)) stop("invalid outcome")
fieldName <- c("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")

disease<-fieldName[outcome]

all_hosps<-c()

for (state in valid_states) {
  
  #state_hosp<-c()
  mydata<-subset(outcomeCare, State == state,select=c("Hospital.Name","State",disease))
  mort<-na.omit(mydata[order(mydata[,disease], as.character(mydata[,"Hospital.Name"])),])

  if (num == "worst") {
    n <- length(mort[,disease])
    state_hosp<-as.character(mort[,"Hospital.Name"][n])
    
  }
  else {
    state_hosp<-as.character(mort[,"Hospital.Name"][num])
  }

  all_hosps<-rbind(all_hosps,c(state_hosp,state))
}

fin_res<-data.frame(all_hosps,stringsAsFactors=FALSE)
colnames(fin_res)<-c("hospital","state")
#print(fin_res)
return(fin_res)

}