#Coefficient Plot of Predicted Probability
#With SE included

ggplot(Prediction.1, aes(x = MC.Resp.SameRace, y = fit)) +
  geom_pointrange(aes(ymin= fit-se.fit, ymax = fit + se.fit)) +
  scale_x_continuous(breaks = c(0,1), labels = c("No", "Yes")) +
  xlab("Member of Congress and Constituents: Same Race") +
  ylab("Predicted Probability")+
  ggtitle("Predicted Probability of Contact", 
          subtitle = "Unmatched Logistic Regression")
