
#################### Event Count Model - Poission without Offset

#Data
setwd("C:/Users/Sharif/OneDrive/University of California, Davis/First Year/Spring Quarter/Methods 213/Class data")


mydata <- read.table("Bills.txt", header=TRUE, sep="\t")

dim(mydata)
summary(mydata)


bills <- data.frame(mydata$all_bills, mydata$all_aic, mydata$seniority, mydata$majority, mydata$chair, mydata$subchr, mydata$female, mydata$latino, mydata$afam, mydata$meddist, mydata$state_leg, mydata$votepct)

names(bills) <- c("bills", "action", "seniority", "majority", "chair", "subchair", "female", "latino", "black", "distance", "stateleg", "margin")
names(bills)

# Poisson Model of Bills Receiving Action

poiss.action <- glm(action ~ seniority + majority + chair + subchair + female + latino + margin + distance, family = poisson, data = bills)

sum.poiss.action <- summary(poiss.action); sum.poiss.action


# Get rid of annoying floating coefficients

coeffs <- round(poiss.action$coef, 3); coeffs

#Get the "logged-count"

# Incident rate ratios

irrs <- exp(coef(poiss.action)); irrs

#when you chnage senoirty by one unity is results in an increase/decrease of _____ units 

# Confidence intervals for betas, irrs

ci.beta <- confint(poiss.action); ci.beta

ci.irr <- exp(confint.default(poiss.action)); ci.irr


# Predicted Values For Profiles

# Covariate profiles

summary(coeffs)

#Majoirty party member
x_Maj_base <- c(1, 4, 1, 0, 0, 0, 0, 66, 0.327)
cnt_Maj_base <- exp(sum(x_Maj_base*coef(poiss.action))); cnt_Maj_base

#Minority party members
x_Min_base <- c(1, 4, 0, 0, 0, 0, 0, 66, 0.327)
cnt_Min_base <- exp(sum(x_Min_base*coef(poiss.action))); cnt_Min_base

#Person 1 term
x_s1_base <- c(1, 1, 1, 0, 0, 0, 0, 66, 0.327)
cnt_s1_base <- exp(sum(x_s1_base*coef(poiss.action))); cnt_s1_base

#Person in 9th term
x_s9_base <- c(1, 9, 1, 0, 0, 0, 0, 66, 0.327)
cnt_s9_base <- exp(sum(x_s9_base*coef(poiss.action))); cnt_s9_base

#Female
x_f_base <- c(1, 4, 1, 0, 0, 1, 0, 66, 0.327)
cnt_f_base <- exp(sum(x_f_base*coef(poiss.action))); cnt_f_base

#latino
x_l_base <- c(1, 4, 1, 0, 0, 0, 1, 66, 0.327)
cnt_l_base <- exp(sum(x_l_base*coef(poiss.action))); cnt_l_base


# Plot predicted counts

x <- seq(1,27, .20)

#Creating a data set that ranges accorss senoirty at a median covariate profile
basedata = data.frame(seniority=x, majority=rep(1, length(x)), chair=rep(0, length(x)), subchair=rep(0, length(x)), female=rep(0, length(x)), latino=rep(0, length(x)), margin=rep(66, length(x)), distance=rep(0.327, length(x)))

base_bills <- predict(poiss.action, newdata=basedata, type="response", se.fit=TRUE)


basedata2 = data.frame(seniority=x, majority=rep(1, length(x)), chair=rep(0, length(x)), subchair=rep(1, length(x)), female=rep(0, length(x)), latino=rep(0, length(x)), margin=rep(66, length(x)), distance=rep(0.327, length(x)))

subchair_bills <- predict(poiss.action, newdata=basedata2, type="response", se.fit=TRUE)


basedata3 = data.frame(seniority=x, majority=rep(0, length(x)), chair=rep(0, length(x)), subchair=rep(0, length(x)), female=rep(0, length(x)), latino=rep(0, length(x)), margin=rep(66, length(x)), distance=rep(0.327, length(x)))

malemin_bills <- predict(poiss.action, newdata=basedata3, type="response", se.fit=TRUE)


basedata4 = data.frame(seniority=x, majority=rep(0, length(x)), chair=rep(0, length(x)), subchair=rep(0, length(x)), female=rep(1, length(x)), latino=rep(0, length(x)), margin=rep(66, length(x)), distance=rep(0.327, length(x)))

femalemin_bills <- predict(poiss.action, newdata=basedata4, type="response", se.fit=TRUE)


# Predicted Bills Receiving Action against Seniority

plot(x, base_bills$fit, type="l", ylim=c(0,8), xlab="Seniority", 
     ylab="Predicted Bills Receiving Action", col = "red")

lines(x, (base_bills$fit + 2*base_bills$se), lty = 2, col = "gray")
lines(x, (base_bills$fit - 2*base_bills$se), lty = 2, col = "gray")

lines(x, subchair_bills$fit, lty = 1, col = "blue")
lines(x, (subchair_bills$fit + 2*subchair_bills$se), lty = 2, col = "gray")
lines(x, (subchair_bills$fit - 2*subchair_bills$se), lty = 2, col = "gray")

text(13, 6, "Subcommittee Chair", col="blue")
text(15, 1.5, "Non-Chair", col="red")


plot(x, malemin_bills$fit, type="l", ylim=c(0,3), xlab="Seniority", 
     ylab="Predicted Bills Receiving Action", col = "red")

lines(x, (malemin_bills$fit + 2*malemin_bills$se), lty = 2, col = "red")
lines(x, (malemin_bills$fit - 2*malemin_bills$se), lty = 2, col = "red")

lines(x, femalemin_bills$fit, lty = 1, col = "blue")
lines(x, (femalemin_bills$fit + 2*femalemin_bills$se), lty = 2, col = "blue")
lines(x, (femalemin_bills$fit - 2*femalemin_bills$se), lty = 2, col = "blue")

text(15, 2.0, "Minority Party Male", col="red")
text(15, 0.5, "Minority Party Female", col="blue")


# Predicted Probabilities: Predcitng the porbabailiyt of each count

#FOr the majoirty party
#IF you notice, the distracution of predicted probabailites are differnt than the acutual dirabution. This measn that poision disabution model may not be the best tool to use for this distracution

pi_0 <- exp(-exp(sum(x_Maj_base*coef(poiss.action)))) * exp(sum(x_Maj_base*coef(poiss.action)))^0 / factorial(0); pi_0

pi_1 <- exp(-exp(sum(x_Maj_base*coef(poiss.action)))) * exp(sum(x_Maj_base*coef(poiss.action)))^1 / factorial(1); pi_1

pi_2 <- exp(-exp(sum(x_Maj_base*coef(poiss.action)))) * exp(sum(x_Maj_base*coef(poiss.action)))^2 / factorial(2); pi_2

pi_3 <- exp(-exp(sum(x_Maj_base*coef(poiss.action)))) * exp(sum(x_Maj_base*coef(poiss.action)))^3 / factorial(3); pi_3

pi_4 <- exp(-exp(sum(x_Maj_base*coef(poiss.action)))) * exp(sum(x_Maj_base*coef(poiss.action)))^4 / factorial(4); pi_4

pi_0 + pi_1 + pi_2 + pi_3 + pi_4


# Poisson Model of Voting with Zelig

z.out <- zelig(action ~ seniority + majority + chair + subchair + female + latino + margin + distance,
               model="poisson", data=bills)


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

#NULL Model!!!!!!!!!!!!!!!!!!!!!!
poiss.null <- glm(action ~ 1, data = bills, family = poisson)

summary(poiss.null)

logLik(poiss.action)
logLik(poiss.null)

g_action <- 2*(-16308.56 - -21198.26); g_action


library(lmtest)

lrtest(poiss.action, poiss.null)


# Pseudo-R2

pR2(poiss.action)


# Adjusted McFadden

L.full <- logLik(poiss.action)
L.null <- logLik(poiss.null)
P <- attr(L.full, "df")

McFadden.R2.full <- 1 - (L.full / L.null); McFadden.R2.full
McFadden.Adj.R2.full <- 1 - ((L.full - P) / L.null); McFadden.Adj.R2.full


# Information criteria

AIC(poiss.null)
AIC(poiss.action)

BIC(poiss.null)
BIC(poiss.action)


# Assessing overdispersion

# Goodness of fit test

dev <- deviance(poiss.action); dev

df <- df.residual(poiss.action); df

p_value <- 1 - pchisq(dev, df); p_value

################### Testing For Overdispurson ##############
# 1. Pearson chi-square and dispersion

pr <- sum(residuals(poiss.action, typ = "pearson")^2); pr

pr / poiss.action$df.residual


# Or, use Hilbe's canned routine

P__disp(poiss.action)

#Any dispursion greater than 1 is BAD!


# 2. Score test

mu <- predict(poiss.action, type = "response")

z <- ((bills$action - mu)^2 - bills$action) / (mu * sqrt(2))

summary(zscore <- lm(z ~ 1))
#If signifigant, then you have overdispursion

# 3. Observed versus predicted values

rbind(obs = table(bills$action)[1:35],
      exp = round(sapply(0:34, function(x)sum(dpois(x, fitted(poiss.action))))))
meany <- mean(bills$action)
expect0 <- exp(-meany)*meany^0/exp(log(factorial(0)))
zerodays <- (poiss.action$df.null + 1) * expect0
obs = table(bills$action)[1:35]
exp = round(sapply(0:34, function(x)sum(dpois(x, fitted(poiss.action)))))
chisq.test(obs, exp)

obs_v <- sd(bills$action)^2; obs_v
mean(exp(predict(poiss.action)))
