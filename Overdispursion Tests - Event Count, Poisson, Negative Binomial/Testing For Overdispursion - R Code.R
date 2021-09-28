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
