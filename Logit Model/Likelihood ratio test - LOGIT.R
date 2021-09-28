# Logit Model of Support with Continuous Partisanship Predictor

logit.field3 <- glm(same_sex ~ party + born_christian + age, mydata, family = binomial(link=logit))

summary(logit.field3)


# Calculate, Plot Fitted Values

yhat.logit3 <- fitted.values(logit.field3)

summary(yhat.logit3)


#**************************Likelihood ratio test****************************

logLik(logit.field2)
logLik(logit.field3)

#Always multiply by 2
g_statusquo <- 2*(-269.8239 - -270.1203); g_statusquo

#Or, use the lrtest function to conduct this test 

#The EASY way
library(lmtest)

lrtest("Model 1", "Model 2")
#Look at ehe chisq