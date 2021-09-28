#Function to convert predict outputs into a dataframe 


#################### Predict.GLM Function ##################

predict.df_convert <- function(Predict.object){
  
  #Convert the fitted vales into a dataframe
  Pred.Df.F.fit <-data.frame(Predict.object$fit)
  Pred.Df.F.se <-data.frame(Predict.object$se.fit)
  Pred.DfF <- cbind(Pred.Df.F.fit, Pred.Df.F.se)
  
  # Rename 
  colnames(Pred.DfF)[1] <- 'fit'
  colnames(Pred.DfF)[2] <- 'se'
  
  return(Pred.DfF)
  
}

#Useage
predict.df_convert(Predict.object)

#    Predict.object = An object derived from a predict/predict.glm function


#Example
Pred.F <- predict.glm(model, newdata = data.frame(X= 500, XFactor= "0", Xincome = 1:15),
                      type = "response",
                      se.fit = TRUE)

predict.df_convert(Pred.F)
