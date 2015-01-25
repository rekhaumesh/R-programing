rankhospital <- function(state, outcome, num = "best") {
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
 
  ## Read outcome data
  outcomeCare<-read.csv("outcome-of-care-measures.csv", na.strings=c("Not Available"))
  valid_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(state, valid_states)) stop("invalid state")
  if (!is.element(outcome, valid_outcomes)) stop("invalid outcome")
  
  fieldName <- c("heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  disease<-fieldName[outcome]
  mydata<-subset(outcomeCare, State == state,select=c("Hospital.Name","State",disease))
  mydata$State<-as.character(mydata$State)
  mydata$Hospital.Name<-as.character(mydata$Hospital.Name)
  
  mort<-na.omit(mydata[order(mydata[,disease], mydata[,"Hospital.Name"]),])
  
  if (num == "worst") {
     num = length(mort[,disease])
    }
  
  #print(mort[num,1])
  return(mort[num,1])
    
}
