
#***********************************Classification table

# Read in data

setwd("C:/Users/Sharif/OneDrive/University of California, Davis/First Year/Spring Quarter/Methods 213/Class data")

# Read in data

mydata <- read.table("FA.tx.txt", header=TRUE, sep="\t")


#***********************************Classification table

#asks how many times do i get the rpediction right and/or wrong

class.logit.field <- data.frame(
  response = mydata$same_sex,     #oberserved values
  predicted = round(yhat.logit,0)) # Predicted values, It is gong to ground .05 down to 0

xtabs(~ predicted + response, data = class.logit.field)
#We want msot of them to match with their respected numbers, diagonally
