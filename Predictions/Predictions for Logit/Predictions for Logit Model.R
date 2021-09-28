#Predictions for Logit Model 

#Logit
Model1logit <- glm(dichagree ~ samestate + sameparty, data = S.RollCall, family = "binomial")
summary(Model1logit)
exp(coef(Model1logit))

#Predicted Values
newdat <- data.frame(
  samestate = rep(median(S.RollCall$samestate, na.rm = T)), 
  sameparty = rep(seq(0,1, .00001)))

Prediction.1 <- cbind(newdat, predict.glm(Model1logit, newdat, se.fit = TRUE, type = c("response")))


#Line Plot
library(ggplot2)
ggplot(Prediction.1, aes(x = sameparty, y = fit)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_ribbon(aes(ymin= fit-se.fit, ymax = fit + se.fit), fill = "blue", alpha = .2) +
  xlab("Same Party") +
  ylab("Predicted Roll Call Agreement")+
  ggtitle("Predicted Roll Call Agreement ", 
          subtitle = "Logit Regression") +
  theme_bw()

library(ggplot2)
ggplot(Prediction.1, aes(x = LoA, y = fit)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  #geom_point(data = Master5, aes(x = LoA, y = Conflict)) +
  xlab("LOA") +
  ylab("Predicted Probability of Conflict")+
  ggtitle("LOA on Conflict", 
          subtitle = "Logit Regression") +
  theme_bw()
