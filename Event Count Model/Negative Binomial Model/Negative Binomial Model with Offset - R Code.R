# Negative Binomial Model of Bills Receiving Action with Offset (Bills Proposed)

setwd("C:/Users/Sharif/OneDrive/University of California, Davis/First Year/Spring Quarter/Methods 213/Class data")

mydata <- read.table("Bills.txt", header=TRUE, sep="\t")

bills2 <- data.frame(mydata$all_bills, mydata$all_aic, mydata$seniority, mydata$majority, mydata$chair, mydata$subchr, mydata$female, mydata$latino, mydata$meddist, mydata$votepct)

names(bills2) <- c("bills", "action", "seniority", "majority", "chair", "subchair", "female", "latino", "distance", "margin")
names(bills2)


nbreg.action2 <- glm.nb(action ~ seniority + majority + chair + subchair + female + latino + margin + distance + offset(log(bills)), data = bills2)

sum.nbreg.action2 <- summary(nbreg.action2); sum.nbreg.action2


# Incident rate ratios

irrs4 <- exp(coef(nbreg.action2)); irrs4


# Confidence intervals for betas, irrs

ci.beta4 <- confint(nbreg.action2); ci.beta4

ci.irr4 <- exp(confint.default(nbreg.action2)); ci.irr4
