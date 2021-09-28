#Sharif Amlani
#R 4.0.3
#Winter 2021

######################## Code Summary ##################
#********************* v1 **********************
#Original Code

#********************* v2 **********************
#Updated Control Variables
#Competitive changed to Battleground
#Percent Old Age changed to Median Age

#********************* v3 **********************
#Includes FE models from DID analysis for Ben's Approval

#********************* v4 **********************
#Includes FE models from DID analysis derived by Ben

##################### Resouces ################E
#https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen =99)
set.seed(1993)

######################### Functions ###################
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
    library(lmtest)
    x<- coeftest(Model, vcov = vcovHC(Model, type=Robust.SE))
    xr<- setNames(data.frame(x[1:dim(x)[1], 2]), c("Robust Standard Errors"))
    xr$Label<- rownames(xr); rownames(xr) <- NULL
    
    Model.Output.Final <- merge(Model.Output.Final, xr, by = "Label")
    
  }
  
  return(Model.Output.Final)
  
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


Regression_To_Table <- function(Model, type = type){
  
  if(type == "RE"){
    sar.chi_DF <- data.frame(Coeff=fixef(Model),SE=sqrt(diag(vcov(Model, useScale = FALSE))),DV="Percent_Turnout_VAP", Model="Random Effects")
    #Calualte Confidence Intervals 
    sar.chi_DF$lower <- sar.chi_DF$Coeff - (qt(0.975,df=3000) * sar.chi_DF$SE)
    sar.chi_DF$upper <- sar.chi_DF$Coeff + (qt(0.975,df=3000) * sar.chi_DF$SE)
    
    sar.chi_DF$Sig.05 <- ifelse(sar.chi_DF$lower <=0 & sar.chi_DF$upper >= 0, 0, 1)
    
    sar.chi_DF$Names <- rownames(sar.chi_DF)
    
    sar.chi_DF <- cbind(Names = sar.chi_DF$Names, round(with(sar.chi_DF, data.frame(Coeff, SE, Sig.05)), 3))
    
  }
  
  
  if(type == "spatial"){
    sar.chi_DF <- data.frame(Coeff=Model$coefficients,SE=Model$rest.se)
    
    #Calualte Confidence Intervals 
    sar.chi_DF$lower <- sar.chi_DF$Coeff - (qt(0.975,df=3000) * sar.chi_DF$SE)
    sar.chi_DF$upper <- sar.chi_DF$Coeff + (qt(0.975,df=3000) * sar.chi_DF$SE)
    
    sar.chi_DF$Sig.05 <- ifelse(sar.chi_DF$lower <=0 & sar.chi_DF$upper >= 0, 0, 1)
    
    sar.chi_DF$Names <- rownames(sar.chi_DF)
    
    sar.chi_DF <- cbind(Names = sar.chi_DF$Names, round(with(sar.chi_DF, data.frame(Coeff, SE, Sig.05)), 3))
    
  }
  
  return(sar.chi_DF)
}


cor_hack <- function(Data, DV){
  df_cor_Final <- NULL
  for (i in colnames(Data)){
    
    if(sum(is.na(Data[[i]])) != length(Data[[i]])){
      
      if(class(Data[[i]]) == "numeric" | class(Data[[i]]) == "integer"){
        
        if(sd(Data[[i]], na.rm = T) >  0){
          Correlation <- cor(Data[[i]], Data[DV], use = "complete.obs")
          
          if(Correlation >= .2 | Correlation <= -.2){
            df_cor<- data.frame(Corr = Correlation)
            df_cor$variable <- i
            df_cor_Final <- rbind(df_cor_Final, df_cor)
          }
        }
      }
    }
  }
  
  return(df_cor_Final)
  
}


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



######################### Library #####################
library(rgdal)
library(spdep) #http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html#running-spatial-regressions https://rspatial.org/analysis/7-spregression.html#spatial-lag-model
library(tmap) 
library(sf)
library(sandwich)
library(lmtest)
library(stargazer)
library(ggplot2)


library(lme4) # mixed random effects models
library(lmtest) # coeftest function
library(lmerTest) # p-values for mixed random effects models
library(multiwayvcov) # clustered standard errors
library(spatialreg) # package for the spatial dependency regressions
######################## Upload Data ##################

#Set Working Directory
setwd("C:/Users/Shari/Dropbox (Personal)/2020 election collab/Data/Combined Data/Master Data/Long Data Format")

#Upload Data 
load("Master Data - VBM - Long - Combined Data -- w4.rda"); VBM_Master.1 <- VBM_Master.Pure 

####################### Data Management #####################

#*********************** Turnout Data *************************
VBM_Master.1$Change_Percent_Turnout_VAP <- (VBM_Master.1$Percent_Turnout_VAP - VBM_Master.1$Percent_Turnout_VAP_Lagged) *100
VBM_Master.1$year <- as.factor(VBM_Master.1$year)
VBM_Master.1$Percent_Turnout_VAP <-VBM_Master.1$Percent_Turnout_VAP*100
VBM_Master.1$Percent_Turnout_VAP_Lagged <-VBM_Master.1$Percent_Turnout_VAP_Lagged*100

#******************** BattleGround States *******************
unique(VBM_Master.1$state_abb)
VBM_Master.1$battleground <- ifelse(VBM_Master.1$state_abb %in% c("AZ","GA", "NC", "MI", "PA", "WI"), "Battleground", "Not Battleground")
VBM_Master.1$battleground <- factor(VBM_Master.1$battleground, levels = c("Not Battleground", "Battleground"))

table(VBM_Master.1$battleground)

#********************* Vote Share *********************
VBM_Master.1$Rep_Change_Vote_Share  <- (VBM_Master.1$republican_percent - VBM_Master.1$republican_percent_lagged) *100
summary(VBM_Master.1$Rep_Change_Vote_Share )

VBM_Master.1$republican_percent <- VBM_Master.1$republican_percent*100
VBM_Master.1$republican_percent_lagged <- VBM_Master.1$republican_percent_lagged*100

#********************* Fix Levels *********************
VBM_Master.1$Conditions_Detail <- factor(VBM_Master.1$Conditions_Detail, 
                                         levels = c("Control",
                                                    "With Excuse To No Excuse",  "With Excuse To Applications",
                                                    "No Excuse to Applications", "No Excuse to VBM Election"))


sum(is.na(VBM_Master.1$Conditions_Detail))

#********************* Covid Cases/Deaths *********************
VBM_Master.1$daily_change_cases_1week_100K[VBM_Master.1$year == 2016] <- 0
VBM_Master.1$daily_change_deaths_1week_100K[VBM_Master.1$year == 2016] <- 0

#********************** 2020 Only Data *********************
VBM_Master_2020.1<- subset(VBM_Master.1, year == 2020)

#********************** Fix Levels **************************
VBM_Master_2020.1$Mail_In_Vote <- factor(VBM_Master_2020.1$Mail_In_Vote, levels = c("need an excuse", "request a mail-in ballot", "Mail-in ballot applications automatically sent", "Mail-in ballots automatically sent"))
levels(VBM_Master_2020.1$Mail_In_Vote)
table(VBM_Master_2020.1$Mail_In_Vote)

#********************** Check For Error Correction **************************
cor(VBM_Master_2020.1$daily_change_cases_1week, VBM_Master_2020.1$mean_covid_cases_1w, use = "complete")

#********************** VBM Liberalization **************************
VBM_Master_2020.1$VBM_Liberalization <- NA
VBM_Master_2020.1$VBM_Liberalization[VBM_Master_2020.1$Conditions == "Control"] <- 0
VBM_Master_2020.1$VBM_Liberalization[VBM_Master_2020.1$Conditions == "Treatment"] <- 1

table(VBM_Master_2020.1$VBM_Liberalization, VBM_Master_2020.1$Conditions)

####################### Parallel Trends: Boxplots #####################

ggplot(data = VBM_Master.1, aes(x = year, y = Percent_Turnout_VAP, fill = Conditions_Detail)) +
  facet_wrap(Conditions_Detail ~. ) +
  geom_boxplot()+
  labs(x = "Year", y = "VAP Turnout", title = "VAP Turnout Accross VBM Conditions Accross Time")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = c(1, 0),
        legend.justification = c(1, 0)) 


