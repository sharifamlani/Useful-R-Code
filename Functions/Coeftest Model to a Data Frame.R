
#coeftest Model to a Data Frame

#********************** v1 ***********************
coeftest_Model_DF<- function(model){
  require(plyr)
  
  Estimate_DF <- setNames(data.frame(model[,1]), c("Coeff"));Estimate_DF$Label <-  rownames(Estimate_DF); rownames(Estimate_DF) <- NULL
  SE_DF <- setNames(data.frame(model[,2]), c("SE")); SE_DF$Label <-  rownames(SE_DF); rownames(SE_DF) <- NULL
  t_DF <- setNames(data.frame(model[,3]), c("t.value")); t_DF$Label <-  rownames(t_DF); rownames(t_DF) <- NULL
  p_DF <- setNames(data.frame(model[,4]), c("P.Value")); p_DF$Label <-  rownames(p_DF); rownames(p_DF) <- NULL
  
  df <- plyr::join_all(list(Estimate_DF, SE_DF, t_DF, p_DF), by = c("Label"))
  
  df2 <- df[, c(2,1,3,4,5)]
  
  #Calualte Confidence Intervals 
  df2$lower <- df2$Coeff - (qt(0.975,df=3000) * df2$SE)
  df2$upper <- df2$Coeff + (qt(0.975,df=3000) * df2$SE)
  
  #Signifigance 
  df2$P.Value <- as.numeric(df2$P.Value)
  df2$Sig.05<- ifelse(df2$P.Value < .05, 1, 0)
  df2$Sig.10<- ifelse(df2$P.Value < .1, 1, 0)
  
  return(df2)
  
}

coeftest_Model_DF(B_DID.VS.3)
