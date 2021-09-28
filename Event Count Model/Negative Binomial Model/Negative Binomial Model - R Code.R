
############ Negative Binomial Model################

setwd("C:/Users/Sharif/OneDrive/University of California, Davis/First Year/Spring Quarter/Methods 213/Class data")


mydata <- read.table("Bills.txt", header=TRUE, sep="\t")

dim(mydata)
summary(mydata)


bills <- data.frame(mydata$all_bills, mydata$all_aic, mydata$seniority, mydata$majority, mydata$chair, mydata$subchr, mydata$female, mydata$latino, mydata$afam, mydata$meddist, mydata$state_leg, mydata$votepct)

names(bills) <- c("bills", "action", "seniority", "majority", "chair", "subchair", "female", "latino", "black", "distance", "stateleg", "margin")
names(bills)


#The Model
nbreg.action <- glm.nb(action ~ seniority + majority + chair + subchair + female + latino + margin + distance, data = bills)

sum.nbreg.action <- summary(nbreg.action); sum.nbreg.action
#Interpret the same way as the count model

alpha <- 1 / nbreg.action$theta; alpha

P__disp(nbreg.action)


# Get rid of annoying floating coefficients

coeffs3 <- round(nbreg.action$coef, 3); coeffs3


# Incident rate ratios

irrs3 <- exp(coef(nbreg.action)); irrs3


# Confidence intervals for betas, irrs

ci.beta3 <- confint(nbreg.action); ci.beta3

ci.irr3 <- exp(confint.default(nbreg.action)); ci.irr3


# Predicted Values For Profiles

# Covariate profiles

x_Maj_base <- c(1, 4, 1, 0, 0, 0, 0, 66, 0.327)
cnt_Maj_base <- exp(sum(x_Maj_base*coef(nbreg.action))); cnt_Maj_base

x_Min_base <- c(1, 4, 0, 0, 0, 0, 0, 66, 0.327)
cnt_Min_base <- exp(sum(x_Min_base*coef(nbreg.action))); cnt_Min_base


x_s1_base <- c(1, 1, 1, 0, 0, 0, 0, 66, 0.327)
cnt_s1_base <- exp(sum(x_s1_base*coef(nbreg.action))); cnt_s1_base

x_s9_base <- c(1, 9, 1, 0, 0, 0, 0, 66, 0.327)
cnt_s9_base <- exp(sum(x_s9_base*coef(nbreg.action))); cnt_s9_base


x_f_base <- c(1, 4, 1, 0, 0, 1, 0, 66, 0.327)
cnt_f_base <- exp(sum(x_f_base*coef(nbreg.action))); cnt_f_base

x_l_base <- c(1, 4, 1, 0, 0, 0, 1, 66, 0.327)
cnt_l_base <- exp(sum(x_l_base*coef(nbreg.action))); cnt_l_base


# Plot predicted counts

x <- seq(1,27, .20)


basedata = data.frame(seniority=x, majority=rep(1, length(x)), chair=rep(0, length(x)), subchair=rep(0, length(x)), female=rep(0, length(x)), latino=rep(0, length(x)), margin=rep(66, length(x)), distance=rep(0.327, length(x)))

base_bills <- predict(nbreg.action, newdata=basedata, type="response", se.fit=TRUE)


basedata2 = data.frame(seniority=x, majority=rep(1, length(x)), chair=rep(0, length(x)), subchair=rep(1, length(x)), female=rep(0, length(x)), latino=rep(0, length(x)), margin=rep(66, length(x)), distance=rep(0.327, length(x)))

subchair_bills <- predict(nbreg.action, newdata=basedata2, type="response", se.fit=TRUE)


basedata3 = data.frame(seniority=x, majority=rep(1, length(x)), chair=rep(0, length(x)), subchair=rep(0, length(x)), female=rep(0, length(x)), latino=rep(0, length(x)), margin=rep(66, length(x)), distance=rep(0.327, length(x)))

malemaj_bills <- predict(nbreg.action, newdata=basedata3, type="response", se.fit=TRUE)


basedata4 = data.frame(seniority=x, majority=rep(0, length(x)), chair=rep(0, length(x)), subchair=rep(0, length(x)), female=rep(0, length(x)), latino=rep(0, length(x)), margin=rep(66, length(x)), distance=rep(0.327, length(x)))

malemin_bills <- predict(nbreg.action, newdata=basedata4, type="response", se.fit=TRUE)


# Predicted Bills Receiving Action against Seniority

plot(x, base_bills$fit, type="l", ylim=c(0,8), xlab="Seniority", 
     ylab="Predicted Bills Receiving Action", col = "red")

lines(x, (base_bills$fit + 2*base_bills$se), lty = 2, col = "gray")
lines(x, (base_bills$fit - 2*base_bills$se), lty = 2, col = "gray")

lines(x, subchair_bills$fit, lty = 1, col = "blue")
lines(x, (subchair_bills$fit + 2*subchair_bills$se), lty = 2, col = "gray")
lines(x, (subchair_bills$fit - 2*subchair_bills$se), lty = 2, col = "gray")

text(7, 6, "Subcommittee Chair", col="blue")
text(15, 1.5, "Non-Chair", col="red")


plot(x, malemaj_bills$fit, type="l", ylim=c(0,5), xlab="Seniority", 
     ylab="Predicted Bills Receiving Action", col = "red")

lines(x, (malemaj_bills$fit + 2*malemaj_bills$se), lty = 2, col = "red")
lines(x, (malemaj_bills$fit - 2*malemaj_bills$se), lty = 2, col = "red")

lines(x, malemin_bills$fit, lty = 1, col = "blue")
lines(x, (malemin_bills$fit + 2*femalemin_bills$se), lty = 2, col = "blue")
lines(x, (malemin_bills$fit - 2*femalemin_bills$se), lty = 2, col = "blue")

text(10, 3.5, "Majority Party", col="red")
text(15, 0.5, "Minority Party", col="blue")


# Predicted Probabilities

pi_0 <- exp(-exp(sum(x_Maj_base*coef(nbreg.action)))) * exp(sum(x_Maj_base*coef(nbreg.action)))^0 / factorial(0); pi_0

pi_1 <- exp(-exp(sum(x_Maj_base*coef(nbreg.action)))) * exp(sum(x_Maj_base*coef(nbreg.action)))^1 / factorial(1); pi_1

pi_2 <- exp(-exp(sum(x_Maj_base*coef(nbreg.action)))) * exp(sum(x_Maj_base*coef(nbreg.action)))^2 / factorial(2); pi_2

pi_3 <- exp(-exp(sum(x_Maj_base*coef(nbreg.action)))) * exp(sum(x_Maj_base*coef(nbreg.action)))^3 / factorial(3); pi_3

pi_4 <- exp(-exp(sum(x_Maj_base*coef(nbreg.action)))) * exp(sum(x_Maj_base*coef(nbreg.action)))^4 / factorial(4); pi_4

pi_0 + pi_1 + pi_2 + pi_3 + pi_4


# Negative Binomial Model of Voting with Zelig

z.out <- zelig(action ~ seniority + majority + chair + subchair + female + latino + margin + distance,
               model="negbin", data=bills)


# Simulate expected counts, first differences

x.out <- setx(z.out, seniority = 4, majority = 0, chair = 0, subchair = 0, female = 0, latino = 0, margin = 66, distance = 0.327)

s.out <- sim(z.out, x = x.out)

summary(s.out)
plot(s.out)


x.out1a <- setx(z.out, seniority = 1, majority = 0, chair = 0, subchair = 0, female = 0, latino = 0, margin = 66, distance = 0.327)
x.out1b <- setx(z.out, seniority = 9, majority = 0, chair = 0, subchair = 0, female = 0, latino = 0, margin = 66, distance = 0.327)

s.out1ab <- sim(z.out, x = x.out1a, x1 = x.out1b)
summary(s.out1ab)


# Model fit

# Likelihood-ratio test

nbreg.null <- glm.nb(action ~ 1, data = bills)

summary(nbreg.null)

logLik(nbreg.action)
logLik(nbreg.null)

g_action <- 2*(-16308.56 - -15283.9); g_action


library(lmtest)

lrtest(nbreg.action, nbreg.null)


# Likelihood-ratio test comparing NB2 and Poisson models

logLik(nbreg.action)
logLik(poiss.action)

g_action2 <- 2*(-16308.56 - -13904.72); g_action2


library(lmtest)

lrtest(nbreg.action, poiss.action)

#Negative binomal is better at modeling the data

# Pseudo-R2

pR2(nbreg.action)


# Adjusted McFadden

L.full <- logLik(nbreg.action)
L.null <- logLik(nbreg.null)
P <- attr(L.full, "df")

McFadden.R2.full <- 1 - (L.full / L.null); McFadden.R2.full
McFadden.Adj.R2.full <- 1 - ((L.full - P) / L.null); McFadden.Adj.R2.full


# Information criteria

AIC(poiss.action)
AIC(nbreg.action)

BIC(poiss.action)
BIC(nbreg.action)


# Test Dispersion for Negative Binomial

alpha <- 1 / nbreg.action$theta; alpha

P__disp(nbreg.action)
#Still greater than one, which isnt great, Still generatieing mroe expected than our observed data would perdict
