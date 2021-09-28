#Predictions for OLS Model


#**********************Same Party and Lagged Agreement 
OLS.3 <- lm(agreement.113.x ~  Fortune500.Norm.Num.Com.Donors + PAC.Norm.Num.Com.Donors +
              Sameparty +
              agreement.112.x,
            data=PAC.Fortune500)

summary(OLS.3)
vif(OLS.3) #Same Party and Lagged Role Call Agreement are Multicolinear

#Predicted Values **** Forture 500
newdat <- data.frame(
  PAC.Norm.Num.Com.Donors = rep(mean(PAC.Fortune500$PAC.Norm.Num.Com.Donors, na.rm = T), length(seq(0,1, .00001))),
  Fortune500.Norm.Num.Com.Donors = seq(0,1, .00001),
  Sameparty = rep(0, length(seq(0,1, .00001))),
  agreement.112.x = rep(mean(PAC.Fortune500$agreement.112.x, na.rm = T), length(seq(0,1, .00001))))

Prediction.1 <- cbind(newdat, predict(OLS.3, newdat, se.fit = TRUE))


#Confidence Intervals
Prediction.1$lower <- Prediction.1$fit - (1.96 * Prediction.1$se.fit)
Prediction.1$upper <- Prediction.1$fit + (1.96 * Prediction.1$se.fit)

##show first few rows
head(Prediction.1)

#Line Plot
ggplot(Prediction.1, aes(x = Fortune500.Norm.Num.Com.Donors, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin= lower, ymax = upper), fill = "green", alpha = .2) +
  xlab("Normalized Number of Common Fortune 500 Donors") +
  ylab("Predicted Roll Call Agreement")+
  ggtitle("Predicted Roll Call Agreement ", 
          subtitle = "OLS Regression: Model 3") +
  theme_bw()
