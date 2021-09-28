
########################### Hurdle Model of Bills Receiving Action #########################

#Data
mydata <- read.table("Bills.txt", header=TRUE, sep="\t")

dim(mydata)
summary(mydata)


bills <- data.frame(mydata$all_bills, mydata$all_aic, mydata$seniority, mydata$majority, mydata$chair, mydata$subchr, mydata$female, mydata$latino, mydata$afam, mydata$meddist, mydata$state_leg, mydata$votepct)

names(bills) <- c("bills", "action", "seniority", "majority", "chair", "subchair", "female", "latino", "black", "distance", "stateleg", "margin")
names(bills)


# The Model
hnbl.action <- hurdle(action ~ seniority + majority + chair + subchair + female + latino + margin + distance,
                      data = bills,
                      link = "logit", # Does someone have a non-zero count
                      zero.dist = "binomial" #Given that someone has a a positive count, what is the loged odds that they get a bill though committee
) 

#Binary model as 

sum.hnbl.action <- summary(hnbl.action); sum.hnbl.action


# Incident rate ratios

irrs5 <- exp(coef(hnbl.action)); irrs5


# Get rid of annoying floating coefficients

coeffs5 <- round(log(irrs5), 3); coeffs5


# Model fit and dispersion

alpha2 <- 1 / hnbl.action$theta; alpha

AIC(hnbl.action)


# Negative Binomial Model of Bills Receiving Action with Offset (Bills Proposed)

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
