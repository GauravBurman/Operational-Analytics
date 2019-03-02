
#Read data
setwd('C:/Users/gaura/Documents/Projects/R/Call Centre Optimization problem')
data <- read.csv("Case_Level2.csv")
summary(data)

#Create a dataframe where we will store the maximum waiting time for each value of the number of callers
caller_opt =data.frame( Number_of_callers= integer(), Wait_Time = integer())


#Run loop for every number of callers possible. Here we have taken the range from 1 to 100
for (number_of_callers in (1:50)){
  #Initialize the available time for each caller
  caller <- rep(0,number_of_callers)
  
  #Index will be used to refer a caller
  index <- 1:number_of_callers
  
  #Here we store the difference of each callers availability from the time when the call was made
  caller_diff <- rep(0,number_of_callers)
  
  
  #We add two columns to the table : Caller assigned to the customer & Wait time for the customer
  data$assigned <- 1
  data$waittime <- 0
  for (i in 1:length(data$Call))
  {
    caller_diff <- data$Time[i] - caller
    best_caller_diff <- max(caller_diff)
    index1 <- index[min(index[caller_diff == best_caller_diff])]
    data$assigned[i] <-  index1
    data$waittime[i] <- max(-best_caller_diff,0)
    caller[index1] <- caller[index1] + data$Duration.of.calls[i] 
  }
  caller_opt[number_of_callers,1] = number_of_callers
  caller_opt[number_of_callers,2] = max(data$waittime)
  
}
print(caller_opt)

