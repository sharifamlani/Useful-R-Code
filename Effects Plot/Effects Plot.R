
#Effects Plot
library(effects)


plot(
  effect("Ideo.Valence.zero", 
         Val.Ideo.Error
  ), 
  xlab = "Valence and Ideology Distance from Zero",
  ylab = "Probability of Making an Error",
  main = "OLS Valence and Ideology Distance and Error"
)