# Plotting an Additive Regression with ggPlot2

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

ggplotRegression(lm(inc.vote10 ~ Incval_pc09.recode + Inc.party +
                      Challenger.spend.log +
                      dpresvote08 + 
                      chexp10, data = Valence)) + theme_fivethirtyeight()

#Note the "names(fit$model)[2]". Change the number to specify the coefficient.
