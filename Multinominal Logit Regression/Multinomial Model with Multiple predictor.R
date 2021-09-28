# Mlogit Model of Voting with Multiple Predictors

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


##################### The Model ###############
# Mlogit Model of Voting with Multiple Predictors

mlogit.mayor3 <- multinom(vote_4 ~ ideology + latino + chinese + loc_eval + knowledge + knowledge*ideology, data = sf_mayor, Hess = TRUE)

sum.mlogit.mayor3 <- summary(mlogit.mayor3); sum.mlogit.mayor3
#Remember: the looged odds relative to the baseline
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




##################### Obtain Fitted Values ###############

yhat.mlogit3 <- predict(mlogit.mayor3, typ = "prob")

summary(yhat.mlogit3)

##################### Plot fitted values ################
#Plot by knowledge

coeffs3 <- sum.mlogit.mayor3$coefficients
coeffs <- cbind(t(coeffs3[1, ]), t(coeffs3[2, ]), t(coeffs3[3, ]))
covmat <- solve(mlogit.mayor3$Hessian)

ndraws <- 1000
betadraw <- mvrnorm(ndraws, coeffs, covmat)

##################### Covariate Profiles #############
#This is the profilte of the person with values corresponding to variables and howe they appear in the modoel
profile1 <- c(1, 0.950, 0, 0, 2, 0, 0)

xb2 <- betadraw[ ,1:7]%*%profile1
xb3 <- betadraw[ ,8:14]%*%profile1
xb4 <- betadraw[ ,15:21]%*%profile1

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

#this is just one person with a specific set of parameters
profile2 <- cbind(1, seq(-3.27,4.47, .10), 0, 0, 2, 0, 0)

xpro2_2 <- profile2%*%coeffs[1:7]
xpro2_3 <- profile2%*%coeffs[8:14]
xpro2_4 <- profile2%*%coeffs[15:21]

prob2_1 <- exp(0) / (exp(0) + exp(xpro2_2) + exp(xpro2_3) + exp(xpro2_4))
prob2_2 <- exp(xpro2_2) / (exp(0) + exp(xpro2_2) + exp(xpro2_3) + exp(xpro2_4))
prob2_3 <- exp(xpro2_3) / (exp(0) + exp(xpro2_2) + exp(xpro2_3) + exp(xpro2_4))
prob2_4 <- exp(xpro2_4) / (exp(0) + exp(xpro2_2) + exp(xpro2_3) + exp(xpro2_4))

#This is profifile 3, of the 3rd person
profile3 <- c(1, 0.950, 0, 0, 2, 1, 0.950)

xb2_2 <- betadraw[ ,1:7]%*%profile3
xb3_2 <- betadraw[ ,8:14]%*%profile3
xb4_2 <- betadraw[ ,15:21]%*%profile3

prob5 <- exp(0) / (exp(0) + exp(xb2_2) + exp(xb3_2) + exp(xb4_2))
prob6 <- exp(xb2_2) / (exp(0) + exp(xb2_2) + exp(xb3_2) + exp(xb4_2))
prob7 <- exp(xb3_2) / (exp(0) + exp(xb2_2) + exp(xb3_2) + exp(xb4_2))
prob8 <- exp(xb4_2) / (exp(0) + exp(xb2_2) + exp(xb3_2) + exp(xb4_2))

means_2 <- cbind(mean(prob5), mean(prob6), mean(prob7), mean(prob8))
sds_2 <- cbind(apply(prob5, 2, sd), apply(prob6, 2, sd), apply(prob7, 2, sd), apply(prob8, 2, sd))
zs_2 <- means_2 / sds_2
ps_2 <- 2 * (1 - pnorm(abs(zs_2)))
presults_2 <- rbind(means_2, sds_2, zs_2, ps_2)
colnames(presults_2) <- c("Avalos", "Herrera", "Lee", "Other")
print(round(presults_2, digits=4))


profile4 <- cbind(1, seq(-3.27,4.47, .10), 0, 0, 2, 1, seq(-3.27,4.47, .10))
xpro4_2 <- profile4%*%coeffs[1:7]
xpro4_3 <- profile4%*%coeffs[8:14]
xpro4_4 <- profile4%*%coeffs[15:21]

prob4_1 <- exp(0) / (exp(0) + exp(xpro4_2) + exp(xpro4_3) + exp(xpro4_4))
prob4_2 <- exp(xpro4_2) / (exp(0) + exp(xpro4_2) + exp(xpro4_3) + exp(xpro4_4))
prob4_3 <- exp(xpro4_3) / (exp(0) + exp(xpro4_2) + exp(xpro4_3) + exp(xpro4_4))
prob4_4 <- exp(xpro4_4) / (exp(0) + exp(xpro4_2) + exp(xpro4_3) + exp(xpro4_4))


##################### The Predicted probabaity plot ###############

plot(seq(-3.27,4.47, .10), prob2_1, type = "l", col = 1, 
     ylim = c(0,1), xlab = "Ideology", ylab = "Probability")
lines(seq(-3.27,4.47, .10), prob2_2, type = "l", col = 2)
lines(seq(-3.27,4.47, .10), prob2_3, type = "l", col = 3)
lines(seq(-3.27,4.47, .10), prob2_4, type = "l", col = 4)

text(-2, 0.65, "Avalos", col = 1)
text(3, 0.38, "Herrera", col = 2)
text(3, 0.75, "Lee", col = 3)
text(1, 0.05, "Other", col = 4)

##################### The Cumlative probability plot #############
plot(seq(-3.27,4.47, .10), prob2_1, type = "l", col = 1, 
     ylim = c(0,1), xlab = "Ideology", ylab = "Probability", main = "Effect of Ideology, Low Knowledge")
lines(seq(-3.27,4.47, .10), (prob2_2 + prob2_1), type = "l", col = 2)
lines(seq(-3.27,4.47, .10), (prob2_3 + prob2_2 + prob2_1), type = "l", col = 3)
lines(seq(-3.27,4.47, .10), (prob2_4 + prob2_3 + prob2_2 + prob2_1), type = "l", col = 4)

text(-2, 0.40, "Avalos", col = 1)
text(2, 0.30, "Herrera", col = 2)
text(3, 0.60, "Lee", col = 3)
text(1, 0.95, "Other", col = 4)


##################### Odds ratios ##################

exp(coef(mlogit.mayor3))

odds_1 <- exp(coef(mlogit.mayor3)[1,1] + coef(mlogit.mayor3)[1,2]*0.950 + coef(mlogit.mayor3)[1,3] + coef(mlogit.mayor3)[1,5]*2); odds_1
odds_2 <- exp(coef(mlogit.mayor3)[2,1] + coef(mlogit.mayor3)[2,2]*0.950 + coef(mlogit.mayor3)[2,4] + coef(mlogit.mayor3)[2,5]*2 + coef(mlogit.mayor3)[2,6] + coef(mlogit.mayor3)[2,7]*0.950); odds_2


##################### Wald statistics ##################

z <- summary(mlogit.mayor3)$coefficients / summary(mlogit.mayor3)$standard.errors; z

p <- (1 - pnorm(abs(z), 0, 1))*2; p


##################### ZELIG: Mlogit Model of Voting #################

z.out <- zelig(vote_4b ~ ideology + latino + chinese + loc_eval + knowledge + knowledge*ideology,
               model="mlogit", data=sf_mayor, reflevel = "1")


##################### ZELIG: Simulate expected probabilities, first differences ################

# Latino at the median of ideology, local evaluation

x.out_base <- setx(z.out, ideology = 0.950, latino = 0, chinese = 0, loc_eval = 0, knowledge = 0)

#Note the diff:
x.out_lat1 <- setx(z.out, ideology = 0.950, latino = 1, chinese = 0, loc_eval = 0, knowledge = 0)

#Now, we simulate the differnces in the probabilty between the two profiles
#Generate the prob for x and the prob x1, then take the differnce in probabaility.
s.out_lat1 <- sim(z.out, x = x.out_base, x1 = x.out_lat1)


# Chinese at the median of ideology, local evaluation vs. Baseline

x.out_chi1 <- setx(z.out, ideology = 0.950, latino = 0, chinese = 1, loc_eval = 0, knowledge = 0)

s.out_chi1 <- sim(z.out, x = x.out_base, x1 = x.out_chi1)


# Low to high local evaluation at the median of ideology

x.out_eval1 <- setx(z.out, ideology = 0.950, latino = 0, chinese = 0, loc_eval = 1, knowledge = 0)

x.out_eval3 <- setx(z.out, ideology = 0.950, latino = 0, chinese = 0, loc_eval = 3, knowledge = 0)

s.out_eval <- sim(z.out, x = x.out_eval1, x1 = x.out_eval3)


# Low to high ideology at the median of local evaluation, low knowledge
#Measing the differnce at different quartiles

x.out_ideo25 <- setx(z.out, ideology = -1.000, latino = 0, chinese = 0, loc_eval = 2, knowledge = 0)

x.out_ideo75 <- setx(z.out, ideology = 1.830, latino = 0, chinese = 0, loc_eval = 2, knowledge = 0)

s.out_ideo <- sim(z.out, x = x.out_ideo25, x1 = x.out_ideo75)


# Low to high knowledge at the median of ideology and local evaluation

x.out_know1 <- setx(z.out, ideology = 0.950, latino = 0, chinese = 0, loc_eval = 2, knowledge = 1)

s.out_know1 <- sim(z.out, x = x.out_base, x1 = x.out_know1)


# Low to high ideology at the median of local evaluation, high knowledge

x.out_ideo25know1 <- setx(z.out, ideology = -1.000, latino = 0, chinese = 0, loc_eval = 2, knowledge = 1)

x.out_ideo75know1 <- setx(z.out, ideology = 1.830, latino = 0, chinese = 0, loc_eval = 2, knowledge = 1)

s.out_ideoknow1 <- sim(z.out, x = x.out_ideo25know1, x1 = x.out_ideo75know1)


##################### Pseudo-R2 #####################

pR2(mlogit.mayor3)


# Adjusted McFadden

##################### Likelyhood Ratio Test #############

#Mlogit Model of Voting (Base)

mlogit.mayor4 <- multinom(vote_4 ~ 1, data = sf_mayor, Hess = TRUE)

L.base <- logLik(mlogit.mayor4)

L.full2 <- logLik(mlogit.mayor)
P3 <- attr(L.full2, "df")

McFadden.R2.full2 <- 1 - (L.full2 / L.base); McFadden.R2.full2
McFadden.Adj.R2.full2 <- 1 - ((L.full2 - P3) / L.base); McFadden.Adj.R2.full2


L.full3 <- logLik(mlogit.mayor3)
P3 <- attr(L.full3, "df")

McFadden.R2.full3 <- 1 - (L.full3 / L.base); McFadden.R2.full3
McFadden.Adj.R2.full3 <- 1 - ((L.full3 - P3) / L.base); McFadden.Adj.R2.full3


###################### PRE ############################

pre(mlogit.mayor3, sim=TRUE, R=1000)

pre(mlogit.mayor3, mlogit.mayor, sim=TRUE, R=1000)


###################### AIC and BIC ################

AIC(mlogit.mayor4)
AIC(mlogit.mayor)
AIC(mlogit.mayor3)

BIC(mlogit.mayor4)
BIC(mlogit.mayor)
BIC(mlogit.mayor3)

