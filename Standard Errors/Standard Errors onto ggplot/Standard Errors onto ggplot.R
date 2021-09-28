#Putting Standard Errors onto a plot

#Graph it - Coeffiecent Plots
library(ggplot2)
ggplot(Prediction.1, aes(x = MC.Resp.SameRace, y = fit)) +
  geom_pointrange(aes(ymin= fit-se.fit, ymax = fit + se.fit)) #+ facet_wrap(. ~ Level)

#Line Plot
ggplot(Prediction.1, aes(x = MC.Resp.SameRace, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin= fit-se.fit, ymax = fit + se.fit), alpha = .3) #+ facet_wrap(. ~ Level)