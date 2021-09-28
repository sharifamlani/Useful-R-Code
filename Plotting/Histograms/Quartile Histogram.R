# Quartile Histogram

ggplot(MVestimate.DPE, aes(x=sd)) +
  geom_histogram(data=subset(MVestimate.DPE,sd <= quantile(MVestimate.DPE$sd, na.rm =T)[2]),bins=30, fill = "#000066", alpha = 0.4) +
  geom_histogram(data=subset(MVestimate.DPE,sd <= quantile(MVestimate.DPE$sd, na.rm =T)[3]),bins=30, fill = "#0033CC", alpha = 0.4) +
  geom_histogram(data=subset(MVestimate.DPE,sd <= quantile(MVestimate.DPE$sd, na.rm =T)[4]),bins=30, fill = "#0099FF", alpha = 0.4) +
  geom_histogram(data=subset(MVestimate.DPE,sd <= quantile(MVestimate.DPE$sd, na.rm =T)[5]),bins=30, fill = "#00FFFF", alpha = 0.4) +
  labs(x = "Uncertanity in Democratic Primary Election (Quartiles)",
       y = "Frequency",
       title = "Standard Error In Democratic Primary Election") +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(0, .20, .01))
