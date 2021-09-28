
########################### VERSON 5: coef command #######################

as.data.frame(coef(summary(sen_model1_cases_fe)))

########################### VERSON 4: Tidy package #######################
library(broom)
data.frame(tidy(Results))

########################### VERSON 3 #######################
#Turns a Regression into a data frame
Model.DF <- function(Model, Robust.SE = NULL) {
  
  #Extract Coefficients
  Model.Output <- as.data.frame(coef(summary(Model)))
  Model.Output$Label <- rownames(Model.Output)
  rownames(Model.Output) <- NULL
  
  #Generate Confidence Intervals
  CI <- as.data.frame(confint(Model, variable.names(Model), level=0.95))
  CI$Label <- rownames(CI)
  rownames(CI) <- NULL
  
  #Merge Model and CIs together 
  Model.Output.Final <- merge(x = Model.Output, y = CI, by =c("Label"))
  
  #Name the columns numeric
  colnames(Model.Output.Final) <- c("Label", "Coeff", "SE", "t.value", "P.Value", "lower", "upper")
  
  Model.Output.Final$Sig.05 <- ifelse(Model.Output.Final$P.Value <= .05, 1,0)
  Model.Output.Final$Sig.10 <- ifelse(Model.Output.Final$P.Value <= .10, 1,0)
  
  #Adjusted R Squared
  Model.Output.Final$AdJ.R2 <- summary(Model)$adj.r.squared
  
  #Dependent Variable
  Model.Output.Final$DV <- all.vars(formula(Model))[1]
  
  #Check for NA's in Model
  for(n in names(coef(Model))){
    if(is.na(Model$coefficients[[n]]) == T){
      newRow <- data.frame(Label=n, 
                           Coeff = NA, 
                           SE = NA, 
                           t.value = NA,
                           P.Value = NA,
                           lower = NA,
                           upper = NA,
                           AdJ.R2 = NA, 
                           Sig.05 = NA,
                           Sig.10 = NA,
                           DV=all.vars(formula(Model))[1])
      
      Model.Output.Final <- rbind(Model.Output.Final, newRow)
      
    }
  }
  
  #Option for Robust Standard Errors
  if(is.null(Robust.SE) == F){
    library(sandwich)
    x<- coeftest(Model, vcov = sandwich::vcovHC(Model, type=Robust.SE))
    xr<- setNames(data.frame(x[1:dim(x)[1], 2]), c("Robust Standard Errors"))
    xr$Label<- rownames(xr); rownames(xr) <- NULL
    
    Model.Output.Final <- merge(Model.Output.Final, xr, by = "Label")
    
  }
  
  return(Model.Output.Final)
  
}

#Example
Result <- lm(Total_PAC_Donors ~ as.factor(cycle) * as.factor(Influence_Member) + as.factor(party) + votepct + DW_Extreme+ majority, data = MC_Master.1); summary(Result)
Model.DF(Result)

Model.DF(Result, Robust.SE = "HC3")

########################### VERSON 2 ########################

#Turns a Regression into a data frame
Model.DF <- function(Model) {
  
  #Extract Coefficients
  Model.Output <- as.data.frame(coef(summary(Model)))
  Model.Output$Label <- rownames(Model.Output)
  rownames(Model.Output) <- NULL
  
  #Generate Confidence Intervals
  CI <- as.data.frame(confint(Model, variable.names(Model), level=0.95))
  CI$Label <- rownames(CI)
  rownames(CI) <- NULL
  
  #Merge Model and CIs together 
  Model.Output.Final <- merge(x = Model.Output, y = CI, by =c("Label"))
  
  #Name the columns numeric
  colnames(Model.Output.Final) <- c("Label", "Coeff", "SE", "t.value", "P.Value", "lower", "upper")
  
  Model.Output.Final$Sig.05 <- ifelse(Model.Output.Final$P.Value <= .05, 1,0)
  Model.Output.Final$Sig.10 <- ifelse(Model.Output.Final$P.Value <= .10, 1,0)
  
  #Adjusted R Squared
  Model.Output.Final$AdJ.R2 <- summary(Model)$adj.r.squared
  
  #Dependent Variable
  Model.Output.Final$DV <- all.vars(formula(Model))[1]
  
  #Check for NA's in Model
  for(n in names(coef(Model))){
    if(is.na(Model$coefficients[[n]]) == T){
      newRow <- data.frame(Label=n, 
                           Coeff = NA, 
                           SE = NA, 
                           t.value = NA,
                           P.Value = NA,
                           lower = NA,
                           upper = NA,
                           AdJ.R2 = NA, 
                           Sig.05 = NA,
                           Sig.10 = NA,
                           DV=all.vars(formula(Model))[1])
      
      Model.Output.Final <- rbind(Model.Output.Final, newRow)
      
    }
  }
  
  return(Model.Output.Final)
  
}


############################# VERSON 1 ######################################
#Turns a Regression into a data frame
Model.DF <- function(Model) {
  
  Model.Output.Final <- NULL
  Model.Output <- NULL
  for (i in seq(1, length(Model$coefficients), by = 1)){
    
    #Extract Coefficients
    Model.Output$Coeff <- as.numeric(Model$coefficients[[i]])
    Model.Output$SE <- as.numeric(coef(summary(Model))[, "Std. Error"] [[i]])
    x <- as.data.frame(confint(Model, variable.names(Model)[[i]], level=0.95))
    Model.Output$lower <- as.numeric(x$`2.5 %`)
    Model.Output$upper <- as.numeric(x$`97.5 %`)
    Model.Output$P.Value <- as.numeric(coef(summary(Model))[, "Pr(>|t|)"] [[i]])
    
    Model.Output$Label <- variable.names(Model)[[i]]
    
    # print(i)
    
    Model.Output.Final <- rbind(Model.Output.Final, Model.Output)
    
  }
  
  #Make Model into a Data Frame
  Model.Output.Final <- as.data.frame(Model.Output.Final)
  
  #Get Rid of RowNames
  rownames(Model.Output.Final) <- NULL
  
  #Name the columns numeric
  Numbnames <- c( "Coeff", "SE", "lower", "upper", "P.Value")
  for (i in Numbnames){
    Model.Output.Final[[i]] <- as.numeric(Model.Output.Final[[i]])
  }
  
  Model.Output.Final$Sig.05 <- ifelse(Model.Output.Final$P.Value <= .05, 1,0)
  Model.Output.Final$Sig.10 <- ifelse(Model.Output.Final$P.Value <= .10, 1,0)
  
  #Dependent Variable
  Model.Output.Final$DV <- all.vars(formula(Model))[1]

  return(Model.Output.Final)
  
}

