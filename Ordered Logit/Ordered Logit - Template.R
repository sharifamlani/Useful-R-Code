# Date: May 7, 2016
# Apply to: ca_taxes.txt
# Description: Analyses of CA tax survey data
################### Preambe and Upload Data #############
# Remove all objects just to be safe

rm(list=ls(all=TRUE))

mydata <- read.dta13("ca_taxes.dta")

# Load appropriate packages

# Standard

library(MASS)
library(stats)

library(pscl)
library(DAMisc)

library(nnet)
library(ggplot2)
library(reshape2)

library(Zelig)
library(ZeligChoice)


# Read in data


dim(mydata)
summary(mydata)


# Descriptives

# Table of opinion

w <- table(mydata$tax_soda)
t = as.data.frame(w)
names(t)[1] = 'Value'
t$Opinion <- c("Strongly Oppose", "Somewhat Oppose", "Somewhat Support", "Strongly Support")
t$Percent = t$Freq / 795

t

mydata$soda_tax2 <- as.factor(mydata$tax_soda)


ca_soda <- data.frame(mydata$tax_soda, mydata$soda_tax2, mydata$ideology, mydata$income, mydata$educate, mydata$cue, mydata$ind, mydata$gop, mydata$gop_cue)

names(ca_soda) <- c("soda_tax", "soda_tax2", "ideology", "income", "education", "cue", "ind", "gop", "gop_cue")
names(ca_soda)


################### OLS Model of Tax Opinion #######################

ols.soda <- lm(soda_tax ~ ideology + income + education + ind + gop + cue + gop_cue, data = ca_soda)

sum.ols.soda <- summary(ols.soda); sum.ols.soda


################### Obtain Fitted Values ##################

yhat.ols <- fitted.values(ols.soda)

summary(yhat.ols)


x <- seq(1, 5, 0.10)

plot(jitter(soda_tax, .25) ~ ideology, data = ca_soda, xlab="Ideology", 
     ylab="Support for Soda Tax")
#OLS line with median control vairbales 
lines(x, 3.12071 + -0.30214*x + 0.02166*6 + 0.09214*3, lty = 1)


# Plot Residuals

resid.ols <- resid(ols.soda)

plot(yhat.ols, resid.ols, ylab = "Fitted Support", xlab = "Residuals")
abline(0, 0)

d.y <- density(resid.ols)

plot(d.y, ylab = "Density", xlab = "Ideology", main = " ")



################### The Model: Ordered Logit Model of Tax Opinion #####################

ologit.soda <- polr(soda_tax2 ~ ideology + income + education + ind + gop + cue + gop_cue, data = ca_soda, Hess = TRUE)

sum.ologit.soda <- summary(ologit.soda); sum.ologit.soda
#These betas are log odds ratios
#if you change ideology by one unit, you change the log odds of being in a higher category by B

################### p-values and CIs for estimates ################
#put this in the next code:
table.ologit.soda <- coef(summary(ologit.soda)); table.ologit.soda
      #One tailed                                         #Makes it two tailed    
p <- pnorm(abs(table.ologit.soda[, "t value"]), lower.tail = FALSE) * 2
table.ologit.soda2 <- cbind(table.ologit.soda, "p value" = p); table.ologit.soda2

ci <- confint(ologit.soda); ci


################### Odds Ratios ########################

or.soda <- exp(ologit.soda$coefficients)

# With CIs

or.soda.ci <- exp(cbind(OR = coef(ologit.soda), ci)); or.soda.ci


parm.vector <- cbind(coef(ologit.soda)); parm.vector

################### Plots #########################
#Y = The odds ratio of landing in a higher level of support
#X = Differnet levels of x
or_ideology <- exp(parm.vector[1,]*ca_soda$ideology)
plot(ca_soda$ideology, or_ideology, xlab = "Ideology", ylab = "Odds Ratio"); title("Odds Ratio for Ideology Scale")


or_income <- exp(parm.vector[2,]*ca_soda$income)
plot(ca_soda$income, or_income, xlab = "Income", ylab = "Odds Ratio"); title("Odds Ratio for Income Scale")


or_education <- exp(parm.vector[3,]*ca_soda$education)
plot(ca_soda$education, or_education, xlab = "Education", ylab = "Odds Ratio"); title("Odds Ratio for Education")


################### Covariate profiles #################

summary(ologit.soda)$coeff

#Just covarite profiles (based on the order they appear in the model), not intercpet 
x_D_base <- c(3, 6, 3, 0, 0, 0, 0)
or_D_base <- exp(sum(x_D_base*coef(ologit.soda))); or_D_base

x_D_cue <- c(3, 6, 3, 0, 0, 1, 0)
or_D_cue <- exp(sum(x_D_cue*coef(ologit.soda))); or_D_cue


x_R_base <- c(3, 6, 3, 0, 1, 0, 0)
or_R_base <- exp(sum(x_R_base*coef(ologit.soda))); or_R_base

x_R_cue <- c(3, 6, 3, 0, 1, 1, 1)
or_R_cue <- exp(sum(x_R_cue*coef(ologit.soda))); or_R_cue


x_D_lo <- c(2, 6, 3, 0, 0, 0, 0)
or_D_lo <- exp(sum(x_D_lo*coef(ologit.soda))); or_D_lo

x_D_hi <- c(4, 6, 3, 0, 0, 0, 0)
or_D_hi <- exp(sum(x_D_hi*coef(ologit.soda))); or_D_hi

#The odds tell us the likelyhood of landing in a 
#lower cateogry verse a higher category

#If the odds are less than 1, then the likley of landing in a lower category is greater
#If the odds are greater than 1, thne the likelyhood lnaing in a higher cateogry is greater


################### Obtain Fitted Values ##################

yhat.ologit <- fitted.values(ologit.soda)

summary(yhat.ologit)

head(yhat.ologit)


# Probabilities

summary(ologit.soda)
                    #These are the cutpoints, The "intercept" from the model
zeta.vector <- cbind(-1.8271604, -1.1768830, -0.1066465); zeta.vector
                        #You'll have to change the cutpoint per model

                #Profite    #Coef
z_D_base <- sum(x_D_base * coef(ologit.soda))

#Change: x_D_base
#Four probabialites over a single covarite profile 
p1_D_base <- 1 / (1 + exp(z_D_base - zeta.vector[1])); p1_D_base
p2_D_base <- 1 / (1 + exp(z_D_base - zeta.vector[2])) - 1 / (1 + exp(z_D_base - zeta.vector[1])); p2_D_base
p3_D_base <- 1 / (1 + exp(z_D_base - zeta.vector[3])) - 1 / (1 + exp(z_D_base - zeta.vector[2])); p3_D_base
p4_D_base <- 1 - 1 / (1 + exp(z_D_base - zeta.vector[3])); p4_D_base

#Should equal 1
p1_D_base + p2_D_base + p3_D_base + p4_D_base

#Profile 2
                #profile  #coef
z_D_cue <- sum(x_D_cue * coef(ologit.soda))

p1_D_cue <- 1 / (1 + exp(z_D_cue - zeta.vector[1])); p1_D_cue
p2_D_cue <- 1 / (1 + exp(z_D_cue - zeta.vector[2])) - 1 / (1 + exp(z_D_cue - zeta.vector[1])); p2_D_cue
p3_D_cue <- 1 / (1 + exp(z_D_cue - zeta.vector[3])) - 1 / (1 + exp(z_D_cue - zeta.vector[2])); p3_D_cue
p4_D_cue <- 1 - 1 / (1 + exp(z_D_cue - zeta.vector[3])); p4_D_cue

p1_D_cue + p2_D_cue + p3_D_cue + p4_D_cue

#Profile 3
z_R_base <- sum(x_R_base * coef(ologit.soda))

p1_R_base <- 1 / (1 + exp(z_R_base - zeta.vector[1])); p1_R_base
p2_R_base <- 1 / (1 + exp(z_R_base - zeta.vector[2])) - 1 / (1 + exp(z_R_base - zeta.vector[1])); p2_R_base
p3_R_base <- 1 / (1 + exp(z_R_base - zeta.vector[3])) - 1 / (1 + exp(z_R_base - zeta.vector[2])); p3_R_base
p4_R_base <- 1 - 1 / (1 + exp(z_R_base - zeta.vector[3])); p4_R_base

p1_R_base + p2_R_base + p3_R_base + p4_R_base


#Profile 4
z_R_cue <- sum(x_R_cue * coef(ologit.soda))

p1_R_cue <- 1 / (1 + exp(z_R_cue - zeta.vector[1])); p1_R_cue
p2_R_cue <- 1 / (1 + exp(z_R_cue - zeta.vector[2])) - 1 / (1 + exp(z_R_cue - zeta.vector[1])); p2_R_cue
p3_R_cue <- 1 / (1 + exp(z_R_cue - zeta.vector[3])) - 1 / (1 + exp(z_R_cue - zeta.vector[2])); p3_R_cue
p4_R_cue <- 1 - 1 / (1 + exp(z_R_cue - zeta.vector[3])); p4_R_cue

p1_R_cue + p2_R_cue + p3_R_cue + p4_R_cue

#Profile 5
z_D_lo <- sum(x_D_lo * coef(ologit.soda))

p1_D_lo <- 1 / (1 + exp(z_D_lo - zeta.vector[1])); p1_D_lo
p2_D_lo <- 1 / (1 + exp(z_D_lo - zeta.vector[2])) - 1 / (1 + exp(z_D_lo - zeta.vector[1])); p2_D_lo
p3_D_lo <- 1 / (1 + exp(z_D_lo - zeta.vector[3])) - 1 / (1 + exp(z_D_lo - zeta.vector[2])); p3_D_lo
p4_D_lo <- 1 - 1 / (1 + exp(z_D_lo - zeta.vector[3])); p4_D_lo

p1_D_lo + p2_D_lo + p3_D_lo + p4_D_lo

#Profile 3

z_D_hi <- sum(x_D_hi * coef(ologit.soda))

p1_D_hi <- 1 / (1 + exp(z_D_hi - zeta.vector[1])); p1_D_hi
p2_D_hi <- 1 / (1 + exp(z_D_hi - zeta.vector[2])) - 1 / (1 + exp(z_D_hi - zeta.vector[1])); p2_D_hi
p3_D_hi <- 1 / (1 + exp(z_D_hi - zeta.vector[3])) - 1 / (1 + exp(z_D_hi - zeta.vector[2])); p3_D_hi
p4_D_hi <- 1 - 1 / (1 + exp(z_D_hi - zeta.vector[3])); p4_D_hi

p1_D_hi + p2_D_hi + p3_D_hi + p4_D_hi


################### Plot predicted probabilities over a range of an IV ###################

#Thus, creating a profile where the only thing that chnages it ideology, everything else is constant

#This create the range -> pluged in for ideology
x <- seq(1,5, .05)

                  #The ideolgy = x From previous code
basedata = data.frame(ideology=x, income=rep(6, length(x)), education=rep(3, length(x)), ind=rep(0, length(x)), gop=rep(0, length(x)), cue=rep(0, length(x)), gop_cue=rep(0, length(x)))

#Predictions
base_soda <- predict(ologit.soda, newdata=basedata, type="prob", se.fit=TRUE)


# Predicted Support for Soda Tax against Ideology

plot(x, base_soda[,1], type="l", ylim=c(0,1), xlab="Ideology", 
     ylab="Probability", col = "red")

lines(x, base_soda[,2], lty = 1, col = "blue")
lines(x, base_soda[,3], lty = 1, col = "green")
lines(x, base_soda[,4], lty = 1, col = "purple")


text(4.5, 0.35, "Strongly Oppose", col="red")
text(3.5, 0.07, "Somewhat Oppose", col="blue")
text(1.5, 0.35, "Somewhat Support", col="green")
text(2.0, 0.60, "Strongly Support", col="purple")


################### Cumulative Support for Soda Tax against Ideology ####################

plot(x, base_soda[,1], type="l", ylim=c(0,1), xlab="Ideology", 
     ylab="Probability", col = "red")

lines(x, base_soda[,1] + base_soda[,2], lty = 1, col = "blue")
lines(x, base_soda[,1] + base_soda[,2] + base_soda[,3], lty = 1, col = "green")
lines(x, base_soda[,1] + base_soda[,2] + base_soda[,3] + base_soda[,4], lty = 1, col = "purple")


text(4.5, 0.20, "Strongly", col="red")
text(4.5, 0.16, "Oppose", col="red")

text(3.5, 0.43, "Somewhat", col="blue")
text(3.5, 0.39, "Oppose", col="blue")


text(2.5, 0.52, "Somewhat", col="green")
text(2.5, 0.48, "Support", col="green")

text(1.5, 0.80, "Strongly", col="purple")
text(1.5, 0.76, "Support", col="purple")


################### Plot predicted probabilities #########################

x <- seq(1,5, .05)


basedata = data.frame(ideology=x, income=rep(6, length(x)), education=rep(3, length(x)), ind=rep(0, length(x)), gop=rep(0, length(x)), cue=rep(0, length(x)), gop_cue=rep(0, length(x)))

base_soda <- predict(ologit.soda, newdata=basedata, type="prob", se.fit=TRUE)


# Predicted Support for Soda Tax against Ideology

plot(x, base_soda[,1], type="l", ylim=c(0,1), xlab="Ideology", 
     ylab="Probability", col = "red")

lines(x, base_soda[,2], lty = 1, col = "blue")
lines(x, base_soda[,3], lty = 1, col = "green")
lines(x, base_soda[,4], lty = 1, col = "purple")


text(4.5, 0.35, "Strongly Oppose", col="red")
text(3.5, 0.07, "Somewhat Oppose", col="blue")
text(1.5, 0.35, "Somewhat Support", col="green")
text(2.0, 0.60, "Strongly Support", col="purple")


################### Cumulative Support for Soda Tax against Ideology ####################

plot(x, base_soda[,1], type="l", ylim=c(0,1), xlab="Ideology", 
     ylab="Probability", col = "black")

lines(x, base_soda[,1] + base_soda[,2], lty = 1, col = "black")
lines(x, base_soda[,1] + base_soda[,2] + base_soda[,3], lty = 1, col = "black")


text(4.5, 0.20, "Strongly", col="black")
text(4.5, 0.16, "Oppose", col="black")

text(3.5, 0.43, "Somewhat", col="black")
text(3.5, 0.39, "Oppose", col="black")


text(2.5, 0.52, "Somewhat", col="black")
text(2.5, 0.48, "Support", col="black")

text(1.5, 0.80, "Strongly", col="black")
text(1.5, 0.76, "Support", col="black")

#interperation: think of a stacked histograpn and read up!

################### Simulate probabilities ########################

coeffs <- ologit.soda$coefficients
cutpoints <- ologit.soda$zeta
ocoeffs <- c(coeffs, cutpoints)
covmat <- solve(ologit.soda$Hessian)

ndraws <- 1000
betadraw <- mvrnorm(ndraws, ocoeffs, covmat)

profile1 <- x_D_base

linkfn <- plogis

xb <- betadraw[, 1:7]%*%profile1


prob1 <- linkfn(betadraw[,8] - xb)
prob2 <- linkfn(betadraw[,9] - xb) - prob1
prob3 <- linkfn(betadraw[,10] - xb) - prob2 - prob1
prob4 <- 1 - prob3 - prob2 - prob1


means <- cbind(mean(prob1), mean(prob2), mean(prob3), mean(prob4))
sds <- cbind(apply(prob1, 2, sd), apply(prob2, 2, sd), apply(prob3, 2, sd), apply(prob4, 2, sd))
zs <- means / sds
ps <- 2 * (1 - pnorm(abs(zs)))
presults <- rbind(means, sds, zs, ps)
colnames(presults) <- c("Strongly Oppose", "Somewhat Oppose", "Somewhat Support", "Strongly Support")
print(round(presults, digits=4))


################### Parallel regressions ###################

profile2 <- as.matrix(basedata)

xb2 <- profile2%*%coeffs

pc1c <- linkfn(cutpoints[1] - xb2)
pc2c <- linkfn(cutpoints[2] - xb2) 
pc3c <- linkfn(cutpoints[3] - xb2) 

plot(x, pc1c, type="l", col="2", ylim=c(0,1), xlab="Ideology", ylab="Probability")
lines(x, pc2c, col="3")
lines(x, pc3c, col="4")
legend(1, 1, legend=c("P(<= Strongly Oppose)", "P(<= Somewhat Oppose)", "P(<= Somewhat Support)"), col=2:4, lty=1)


################### Zelig: Ologit Model of Voting ####################

z.out <- zelig(soda_tax2 ~ ideology + income + education + ind + gop + cue + gop_cue,
	       model="ologit", data=ca_soda)


################### Zelig: Simulate expected probabilities, first differences ##########

x.out <- setx(z.out, ideology = 3, income = 6, education = 3, ind = 0, gop = 0, cue = 0, gop_cue = 0)

s.out <- sim(z.out, x = x.out)

#INterpreated as probability fo supopritng the measure (becasue our higherst coded value is 5 which = support for measure)
summary(s.out)
plot(s.out)


x.out1a <- setx(z.out, ideology = 2, income = 6, education = 3, ind = 0, gop = 0, cue = 0, gop_cue = 0)
x.out1b <- setx(z.out, ideology = 4, income = 6, education = 3, ind = 0, gop = 0, cue = 0, gop_cue = 0)

#Calulating the difference between the two covarite profiles 
s.out1ab <- sim(z.out, x = x.out1a, x1 = x.out1b)
summary(s.out1ab)


x.out2a <- setx(z.out, ideology = 3, income = 4, education = 3, ind = 0, gop = 0, cue = 0, gop_cue = 0)
x.out2b <- setx(z.out, ideology = 3, income = 9, education = 3, ind = 0, gop = 0, cue = 0, gop_cue = 0)

s.out2ab <- sim(z.out, x = x.out2a, x1 = x.out2b)
summary(s.out2ab)


x.out3a <- setx(z.out, ideology = 3, income = 6, education = 3, ind = 0, gop = 0, cue = 0, gop_cue = 0)
x.out3b <- setx(z.out, ideology = 3, income = 6, education = 5, ind = 0, gop = 0, cue = 0, gop_cue = 0)

s.out3ab <- sim(z.out, x = x.out3a, x1 = x.out3b)
summary(s.out3ab)


x.out4 <- setx(z.out, ideology = 3, income = 6, education = 3, ind = 1, gop = 0, cue = 0, gop_cue = 0)

s.out4 <- sim(z.out, x = x.out, x1 = x.out4)
summary(s.out4)

x.out5 <- setx(z.out, ideology = 3, income = 6, education = 3, ind = 0, gop = 1, cue = 0, gop_cue = 0)

s.out5 <- sim(z.out, x = x.out, x1 = x.out5)
summary(s.out5)

x.out6 <- setx(z.out, ideology = 3, income = 6, education = 3, ind = 0, gop = 0, cue = 1, gop_cue = 0)

s.out6 <- sim(z.out, x = x.out, x1 = x.out6)
summary(s.out6)

x.out7 <- setx(z.out, ideology = 3, income = 6, education = 3, ind = 0, gop = 1, cue = 0, gop_cue = 1)

s.out7 <- sim(z.out, x = x.out5, x1 = x.out7)
summary(s.out7)


################### Wald statistics ######################

z <- summary(ologit.soda)$coefficients[,1] / summary(ologit.soda)$coefficients[,2]; z

p <- (1 - pnorm(abs(z), 0, 1))*2; p


library(foreign)
library(multcomp)

names(coef(ologit.soda))

# Test if independent, republican simultaneously 0

summary(glht(ologit.soda, linfct = c("ind + gop = 0")))


# Test if republican, republican*cue simultaneously 0

summary(glht(ologit.soda, linfct = c("cue = 0")))

summary(glht(ologit.soda, linfct = c("cue + gop_cue = 0")))


################### Pseudo-R2 #################

pR2(ologit.soda)


################### PRE #######################

library(DAMisc)

pre(ologit.soda, sim=TRUE, R=1000)


################### Adjusted McFadden ###############

# Ologit Model of Voting (Base)

ologit.soda2 <- polr(soda_tax2 ~ 1, data = ca_soda, Hess = TRUE)

L.base <- logLik(ologit.soda2)

L.full <- logLik(ologit.soda)
P3 <- attr(L.full, "df")

McFadden.R2.full <- 1 - (L.full / L.base); McFadden.R2.full
McFadden.Adj.R2.full <- 1 - ((L.full - P3) / L.base); McFadden.Adj.R2.full


################### AIC and BIC #################

AIC(ologit.soda2)
AIC(ologit.soda)

BIC(ologit.soda2)
BIC(ologit.soda)


#Check the parrellel lines assumptions with the STATA code in the same folder

# Clean up

# rm(list=ls(all=TRUE))

# End
