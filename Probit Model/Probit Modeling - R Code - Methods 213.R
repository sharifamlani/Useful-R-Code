

#**************************************Probit Modeling

#Set Data
setwd("C:/Users/Sharif/OneDrive/University of California, Davis/First Year/Spring Quarter/Methods 213/Class data")

# Read in data

mydata <- read.table("FA.tx.txt", header=TRUE, sep="\t")

dim(mydata)
summary(mydata)


#Probit Model of Support

probit.field <- glm(same_sex ~ age, mydata, family = binomial(link=probit))

summary(probit.field)


# Calculate, Plot Fitted Values

yhat.probit <- fitted.values(probit.field)

summary(yhat.probit)


library(VGAM)

prob1 <- 0.833962-0.012922*53
probit(prob1, inverse=TRUE)


prob1 <- 0.833962-0.012922*37
probit(prob1, inverse=TRUE)


# Statistical inference for MLE

# Wald statistic and confidence intervals

z_intercept <- (0.833962 - 0) / 0.178294; z_intercept

c(0.833962-1.96*0.178294, 0.833962+1.96*0.178294)


z_beta <- (-0.012922 - 0) / 0.003178; z_beta

c(-0.012922-1.96*0.003178, -0.012922+1.96*0.003178)


# Likelihood ratio test and confidence intervals

probit.null <- glm(mydata$same_sex ~ 1, mydata, family = binomial(link=probit))

summary(probit.null)

logLik(probit.field)
logLik(probit.null)

g_statusquo <- 2*(-318.8138 - -327.2174); g_statusquo


# Or, use the lrtest function to conduct this test

library(lmtest)

lrtest(probit.field, probit.null)


confint(probit.field)


# Probit Model of Support with GOP-Age Interaction

probit.field5 <- glm(same_sex ~ republican + born_christian + age + republican*age, mydata, family = binomial(link=probit))

summary(probit.field5)


# Calculate, Plot Fitted Values

yhat.probit5 <- fitted.values(probit.field5)

summary(yhat.probit5)


# Covariate profiles

summary(probit.field5)$coeff

x_G_age_lo <- c(1, 1, 0, 37, 37)
y_G_age_lo <- sum(x_G_age_lo*coef(probit.field5))

pi_G_age_lo <- probit(y_G_age_lo, inverse=TRUE); pi_G_age_lo


x_G_age_hi <- c(1, 1, 0, 67, 67)
y_G_age_hi <- sum(x_G_age_hi*coef(probit.field5))

pi_G_age_hi <- probit(y_G_age_hi, inverse=TRUE); pi_G_age_hi


x_D_age_lo <- c(1, 0, 0, 37, 0)
y_D_age_lo <- sum(x_D_age_lo*coef(probit.field5))

pi_D_age_lo <- probit(y_D_age_lo, inverse=TRUE); pi_D_age_lo


x_D_age_hi <- c(1, 0, 0, 67, 0)
y_D_age_hi <- sum(x_D_age_hi*coef(probit.field5))

pi_D_age_hi <- probit(y_D_age_hi, inverse=TRUE); pi_D_age_hi


summary(probit.field5)

x_0 <- c(1, 0, 0, 53, 0)
y_0 <- sum(x_0*coef(probit.field5))

pi_0 <- probit(y_0, inverse=TRUE); pi_0


x_G <- c(1, 1, 0, 53, 53)
y_G <- sum(x_G*coef(probit.field5))

pi_G <- probit(y_G, inverse=TRUE); pi_G

pi_G - pi_0


x_born <- c(1, 0, 1, 53, 0)
y_born <- sum(x_born*coef(probit.field5))

pi_born <- probit(y_born, inverse=TRUE); pi_born

pi_born - pi_0


pi_D_age_lo - pi_D_age_hi
pi_G_age_lo - pi_G_age_hi


# Calculate, plot predicted probabilities

base_field5_probit <- predict(probit.field5, newdata=basedata, type="response", se.fit=TRUE)

gop_field5_probit <- predict(probit.field5, newdata=gopdata, type="response", se.fit=TRUE)

born_field5_probit <- predict(probit.field5, newdata=borndata, type="response", se.fit=TRUE)


# Scatterplot of Support for Same-sex Marriage against Age

plot(jitter(same_sex, .25) ~ age, mydata, xlab="Age", xlim=c(20,100), 
     ylab="Support Same-Sex Marriage")
abline(h = 1, lty = 2)
abline(h = 0, lty = 2)


lines(x, base_field5_probit$fit, lty = 2, col = "red")
lines(x, (base_field5_probit$fit + 2*base_field5_probit$se), lty = 2, col = "gray")
lines(x, (base_field5_probit$fit - 2*base_field5_probit$se), lty = 2, col = "gray")

lines(x, gop_field5_probit$fit, lty = 2, col = "blue")
lines(x, (gop_field5_probit$fit + 2*gop_field5_probit$se), lty = 2, col = "gray")
lines(x, (gop_field5_probit$fit - 2*gop_field5_probit$se), lty = 2, col = "gray")


text(25, 0.40, "red = DEM, IND", col="red", pos=4)
text(25, 0.35, "blue = GOP", col="blue", pos=4)


# Clean up

# rm(list=ls(all=TRUE))

# End
