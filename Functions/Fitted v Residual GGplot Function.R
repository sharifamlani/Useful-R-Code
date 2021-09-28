#Fitted v Residual Ggplot Function

#This function plots the fitted values against the residuals


Fitted.Residuals.Plot <- function(Model){
    OLS.DF <- as.data.frame(cbind(Model$residuals, Model$fitted.values))
    colnames(OLS.DF) <- c("residuals", "fitted.values")

    library(ggplot2)
    Plot <- ggplot2::ggplot(data = OLS.DF, aes(x= fitted.values, y = residuals))+
      geom_point() +
      labs(x= "Fitted Values",
           y= "Residuals",
           title = "Fitted v. Residuals")
    
    return(Plot)
}

#Call
Fitted.Residuals.Plot(Model)
    #Model = OLS model output

#Example
OLS.1 <- lm(Norm.Num.Bill.CS ~ Norm.Num.Com.Donors + Sameparty + Samestate + ideo.diff, data = Money.Cosponsor.3)
summary(OLS.1)
Fitted.Residuals.Plot(OLS.1)
