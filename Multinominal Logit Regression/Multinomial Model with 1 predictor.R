#Multinomial Model with 1 predictor

##################### Packages ##########################

library(MASS)
library(stats)

library(pscl)
library(DAMisc)

library(nnet)
library(ggplot2)
library(reshape2)

library(Zelig)

library(ZeligChoice)

##################### Upload Data and Data Management ###############
setwd("C:/Users/Sharif/OneDrive/University of California, Davis/First Year/Spring Quarter/Methods 213/Class data")

# Read in data

mydata <- read.table("sfmayor1.txt", header=TRUE, sep="\t")

dim(mydata)
summary(mydata)


# Descriptives

# Table of votes

w <- table(mydata$vote_4)
t = as.data.frame(w)
names(t)[1] = 'Value'
t$Candidate <- c("Avalos", "Herrera", "Lee", "Other")
t$Percent = t$Freq / 1087

t


# Histogram of ideal points

hist(mydata$D1, breaks = 10, prob = TRUE, las = 1,
     col="blue", main="Distribution of Ideal Points", 
     ylab="", xlab="Respondent Ideal Point")


summary(mydata[,8:12])


# Choose reference category for the response
#set as Facotr
mydata$vote_4 <- as.factor(mydata$vote_4)

#defined a new vaiable where he is changing the references category. 
#assume that he resetting the fererence category
mydata$vote_4a <- relevel(mydata$vote_4, ref = 4)

levels(mydata$vote_4a)
levels(mydata$vote_4b)

#creaing the data frame
sf_mayor <- data.frame(mydata$vote_4, mydata$vote_4a, mydata$vote_4b, mydata$D1, mydata$latino, mydata$chinese, mydata$inc_level, mydata$ret_level, mydata$know_hi)

#Resetting the names 
names(sf_mayor)
names(sf_mayor) <- c("vote_4", "vote_4a", "vote_4b", "ideology", "latino", "chinese", "income", "loc_eval", "knowledge")

##################### The Model ######################
# Mlogit Model of Voting
#estimate the model                 #estimate the hessian
mlogit.mayor <- multinom(vote_4 ~ ideology, data = sf_mayor, Hess = TRUE)

sum.mlogit.mayor <- summary(mlogit.mayor); sum.mlogit.mayor

##################### Interpretation ###############

#2 3 4  are the values of the variable, and the coeffeicts are relative to the basline category that you set up
#each line corresponds to 3 different equations

#******************************************Interpretation
#the natural cateorgy of the 2, relaitve to or verse 1:
#Intercept, when the idology is 0, the intercept is the loged odds of choicing 2 relative to 1
# b1 = for a 1 unit increase in ideology you multily the loged odds of 2 realitve 1 by .6279
#THE Ccoefficients are comapiasions to the refernce cateorgies

#*************************************Interpertation
#To estimate the mlogit model, we can use the multinom() command in the nnet package
#The output has three parts, labeled with the categories of the dependent variable; they correspond to three equations
#E.g., Equation 2 indicates the log-odds of "Herrera" vs. "Avalos",
#???????? ????i2/????i1=??2+????2????i=???0.577+0.627???Ideology
#??2 is positive and indicates the effect of a 1-unit change in ideology on the log-odds of voting for Herrera vs. Avalos


##################### Obtain Fitted Values #######################
#This predicted probaility, but you cannot tell who the people are and their characetrisitcs. but you can use these to see the min and max
#we dont know what  the person looks like (what the specific IVs are).
#sould be bounded between 0 and 1.
yhat.mlogit <- predict(mlogit.mayor, typ = "prob")

summary(yhat.mlogit)

#Does the same:
#summary(mlogit.mayor$fitted.values)

##################### Predicted probabilities (formula) #################

#Prediciting the probailtiy for indivudal people who are at specific ideology. tryng to find the probabilty for voting for a specific canidate at a specific probaility
#when we are using predicted probaility we are not n
#NOTE: the probaility of voting for avalos . the forumal is different beteasue it is the basline

#Note: we are using the same ideology value(.950) = this is the median of ideology
#we are asuming the that mideoan ideology, is the "same" person.
#remember this formulat is just derived from the model's a and b1
#note the only thing that would change would be our x, whihc is what we put ieology into
pi_h <- exp(-0.5768014 + 0.6276940*0.950) / (1 + exp(-0.5768014 + 0.6276940*0.950) + exp(-0.7623559 + 0.9374076*0.950) + exp(-1.4380891 + 0.6444767*0.950)); pi_h

pi_l <- exp(-0.7623559 + 0.9374076*0.950) / (1 + exp(-0.5768014 + 0.6276940*0.950) + exp(-0.7623559 + 0.9374076*0.950) + exp(-1.4380891 + 0.6444767*0.950)); pi_l

pi_o <- exp(-1.4380891 + 0.6444767*0.950) / (1 + exp(-0.5768014 + 0.6276940*0.950) + exp(-0.7623559 + 0.9374076*0.950) + exp(-1.4380891 + 0.6444767*0.950)); pi_o

pi_a <- 1 / (1 + exp(-0.5768014 + 0.6276940*0.950) + exp(-0.7623559 + 0.9374076*0.950) + exp(-1.4380891 + 0.6444767*0.950)); pi_a

#thus, the probaility of a single person, over the course of the data, sould equal 1
pi_h + pi_l + pi_o + pi_a


#Note that these predicted propbaility, going accross the rows, should sum to 1!
#becasue they are the probaility, per person, of voting for each group 
head(yhat.mlogit)

##################### Plot predicted probabilities ########################
#creating a sequence of number from the min of ideology to the max of ideolofy
x <- seq(-3.27,4.47, .10)
#These re the predicted probaility formular from the model
yh <- exp(-0.5768014 + 0.6276940*x) / (1 + exp(-0.5768014 + 0.6276940*x) + exp(-0.7623559 + 0.9374076*x) + exp(-1.4380891 + 0.6444767*x))
yl <- exp(-0.7623559 + 0.9374076*x) / (1 + exp(-0.5768014 + 0.6276940*x) + exp(-0.7623559 + 0.9374076*x) + exp(-1.4380891 + 0.6444767*x))
yo <- exp(-1.4380891 + 0.6444767*x) / (1 + exp(-0.5768014 + 0.6276940*x) + exp(-0.7623559 + 0.9374076*x) + exp(-1.4380891 + 0.6444767*x))
ya <- 1 / (1 + exp(-0.5768014 + 0.6276940*x) + exp(-0.7623559 + 0.9374076*x) + exp(-1.4380891 + 0.6444767*x))


plot(x, ya, type = "l", col = 1, 
     ylim = c(0,1), xlab = "Ideology", ylab = "Probability")
lines(x, yh, type = "l", col = 2)
lines(x, yl, type = "l", col = 3)
lines(x, yo, type = "l", col = 4)

text(-2, 0.65, "Avalos", col = 1)
text(3, 0.38, "Herrera", col = 2)
text(3, 0.75, "Lee", col = 3)
text(1, 0.05, "Other", col = 4)

#the Cumukative probaility graphy
#basix=cially adding up the probaility as we go accorss the figre
plot(x, ya, type = "l", col = 1, 
     ylim = c(0,1), xlab = "Ideology", ylab = "Probability")
lines(x, (ya + yh), type = "l", col = 2)
lines(x, (ya + yh + yl), type = "l", col = 3)
lines(x, (ya + yh + yl + yo), type = "l", col = 4)

text(-2, 0.45, "Avalos", col = 1)
text(2, 0.30, "Herrera", col = 2)
text(3, 0.75, "Lee", col = 3)
text(1, 0.95, "Other", col = 4)
#think of these as a histogram with differnct colors stacked on each other! - Like what Paige said!
#look at a specif point (2) and look up and note the relaive denivity the dfistance between  each line


##################### Simulate probabilities ##################

coeffs1 <- sum.mlogit.mayor$coefficients
coeffs <- cbind(t(coeffs1[1, ]), t(coeffs1[2, ]), t(coeffs1[3, ]))
covmat <- solve(mlogit.mayor$Hessian)

ndraws <- 1000
betadraw <- mvrnorm(ndraws, coeffs, covmat)

profile1 <- c(1, 0.950)

xb2 <- betadraw[ ,1:2]%*%profile1
xb3 <- betadraw[ ,3:4]%*%profile1
xb4 <- betadraw[ ,5:6]%*%profile1

prob1 <- exp(0) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4))
prob2 <- exp(xb2) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4))
prob3 <- exp(xb3) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4))
prob4 <- exp(xb4) / (exp(0) + exp(xb2) + exp(xb3) + exp(xb4))

means <- cbind(mean(prob1), mean(prob2), mean(prob3), mean(prob4))
sds <- cbind(apply(prob1, 2, sd), apply(prob2, 2, sd), apply(prob3, 2, sd), apply(prob4, 2, sd))
zs <- means / sds
ps <- 2 * (1 - pnorm(abs(zs)))
presults <- rbind(means, sds, zs, ps)
colnames(presults) <- c("Avalos", "Herrera", "Lee", "Other")
print(round(presults, digits=4))


profile2 <- cbind(1, seq(-3.27,4.47, .10))
xpro2_2 <- profile2%*%coeffs[1:2]
xpro2_3 <- profile2%*%coeffs[3:4]
xpro2_4 <- profile2%*%coeffs[5:6]

prob2_1 <- exp(0) / (exp(0) + exp(xpro2_2) + exp(xpro2_3) + exp(xpro2_4))
prob2_2 <- exp(xpro2_2) / (exp(0) + exp(xpro2_2) + exp(xpro2_3) + exp(xpro2_4))
prob2_3 <- exp(xpro2_3) / (exp(0) + exp(xpro2_2) + exp(xpro2_3) + exp(xpro2_4))
prob2_4 <- exp(xpro2_4) / (exp(0) + exp(xpro2_2) + exp(xpro2_3) + exp(xpro2_4))


plot(seq(-3.27,4.47, .10), prob2_1, type = "l", col = 1, 
     ylim = c(0,1), xlab = "Ideology", ylab = "Probability")
lines(seq(-3.27,4.47, .10), prob2_2, type = "l", col = 2)
lines(seq(-3.27,4.47, .10), prob2_3, type = "l", col = 3)
lines(seq(-3.27,4.47, .10), prob2_4, type = "l", col = 4)

text(-2, 0.65, "Avalos", col = 1)
text(3, 0.38, "Herrera", col = 2)
text(3, 0.75, "Lee", col = 3)
text(1, 0.05, "Other", col = 4)

##################### Zelig to Generate Predicted Probabilities ############
#This is everything we have been doing by hand in a canned commaned

# Mlogit Model of Voting with Zelig
# Zelig chooses the last category as the omitted
# To get equivalent estimates, recode the response with Avalos as 4

#estimates the porbaility
z.out <- zelig(vote_4b ~ ideology,
               model="mlogit", data=sf_mayor)


# Simulate expected probabilities, first differences

#we are going to estimate it for:
x.out <- setx(z.out, ideology = 0.950)

#now we want to simulate it
s.out <- sim(z.out, x = x.out)

summary(s.out)
plot(s.out)


x.out1a <- setx(z.out, ideology = -1.000)
x.out1b <- setx(z.out, ideology = 1.830)

s.out1ab <- sim(z.out, x = x.out1a, x1 = x.out1b)
summary(s.out1ab)

##################### Odds ratios for coefficients ###############

odds_ha <- pi_h/pi_a; odds_ha

odds_ha_2 <- exp(-0.5768014 + 0.6276940*0.950); odds_ha_2

odds_la <- pi_l/pi_a; odds_la
odds_oa <- pi_o/pi_a; odds_oa

odds_hl <- odds_ha / odds_la; odds_hl

odds_hl2 <- exp((-0.5768014 - -0.7623559) + (0.6276940 - 0.9374076)*0.950); odds_hl2


# The easy way

exp(coef(mlogit.mayor))



##################### Wald statistics ################

z <- summary(mlogit.mayor)$coefficients / summary(mlogit.mayor)$standard.errors; z

p <- (1 - pnorm(abs(z), 0, 1))*2; p


##################### Wald statistic and confidence intervals ###########

z_intercept1 <- (-0.5768014 - 0) / 0.09025893; z_intercept1
z_intercept2 <- (-0.7623559 - 0) / 0.10344315; z_intercept2
z_intercept3 <- (-1.4380891 - 0) / 0.12462948; z_intercept3

c(-0.5768014-1.96*0.09025893, -0.5768014+1.96*0.09025893)
c(-0.7623559-1.96*0.10344315, -0.7623559+1.96*0.10344315)
c(-1.4380891-1.96*0.12462948, -1.4380891+1.96*0.12462948)

z_beta1 <- (0.6276940 - 0) / 0.05729491; z_beta1
z_beta2 <- (0.9374076 - 0) / 0.06297711; z_beta2
z_beta3 <- (0.6444767 - 0) / 0.07774351; z_beta3

c(0.6276940-1.96*0.05729491, 0.6276940+1.96*0.05729491)
c(0.9374076-1.96*0.06297711, 0.9374076+1.96*0.06297711)
c(0.6444767-1.96*0.07511292, 0.6444767+1.96*0.07511292)


##################### Pseudo-R2 #################

pR2(mlogit.mayor)


##################### PRE ################

library(DAMisc)

pre(mlogit.mayor, sim=TRUE, R=1000)
