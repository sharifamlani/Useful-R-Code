#Illistration of floating-point inaccuracies

#Assume the following model:
set.seed(1995)
x <- sample(c(0,1), 100, replace = T, prob = c(.5,.5))
y <- runif(100, min = 1, max = 100)

df <- data.frame(x, y)

OLS <- lm(y ~ as.factor(x), data = df)
summary(OLS)

#It produces inaccuracies at the last decimal point
table(OLS$fitted.values)
#This is a float point inaccuracy.
#This happens more often than we think about 

#Yet, most of the time, here is what R shows us:
OLS$fitted.values

#The rounded verson of the calulation
table(round(OLS$fitted.values, 5))
#Which are the same.

