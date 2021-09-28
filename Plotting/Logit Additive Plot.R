#Logit additive Plot

################# ggplot Regression ################

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(Y ~ Xincome + X + XFactor, data= df)) +
  labs(x = "Incumbent Job and Leadership Valence",
       y = "Incumbent's Vote Share",
       title = "Additive Effect of Valence on Vote Share") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))
