#Sharif Amlani
#R 4.0.3
#Spring 2021

######################## Code Summary ##################
#Tutorial can be accessed here: 
#https://cran.r-project.org/web/packages/sensemakr/vignettes/sensemakr.html
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)





######################### Functions ###################
Alternative_ModelDF <- function(Model, Type){
  
  if(Type == "RE"){
    Results_Master.P2 <- data.frame(Coeff=fixef(Model),SE=sqrt(diag(vcov(Model, useScale = FALSE))), statistic = coef(summary(Model))[,4], Model="Random Effects"); Results_Master.P2$Label <- rownames(Results_Master.P2)
    
    
  }
  
  if(Type == "Spatial Lag"){
    Results_Master.P2 <- data.frame(Coeff=Model$coefficients,SE=Model$rest.se, Model="Spatial Lag"); Results_Master.P2$Label <- rownames(Results_Master.P2)
    
    
  }
  
  
  if(Type == "Spatial Error"){
    Results_Master.P2 <- data.frame(Coeff=Model$coefficients, SE=Model$rest.se, Model="Spatial Error"); Results_Master.P2$Label <- rownames(Results_Master.P2)
    
    
  }
  
  rownames(Results_Master.P2) <- NULL
  
  #******************* Major Stats in P2 ************************
  
  #Calualte Confidence Intervals 
  Results_Master.P2$lower <- Results_Master.P2$Coeff - (qt(0.975,df=3000) * Results_Master.P2$SE)
  Results_Master.P2$upper <- Results_Master.P2$Coeff + (qt(0.975,df=3000) * Results_Master.P2$SE)
  
  #Signifigance 
  Results_Master.P2$Sig.05 <- ifelse(Results_Master.P2$lower <=0 & Results_Master.P2$upper >= 0, 0, 1)
  
  #******************* Merge Dataset ************************
  return(Results_Master.P2)
  
}
######################### Library #####################
library(ggplot2)
library(sensemakr)
library(broom)
library(lme4) # mixed random effects models
library(lmtest) # coeftest function
library(lmerTest) # p-values for mixed random effects models
library(multiwayvcov) # clustered standard errors
######################## Upload Data ##################
data("darfur")

darfur.model <- lm(peacefactor ~ directlyharmed   +  female +
                     age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + village, 
                   data = darfur)

summary(darfur.model)

##################### Insignificance to Significance ##############
#Create the bounds
my_bounds <- ovb_bounds(darfur.model, 
                        benchmark_covariates = "female",
                        treatment = "herder_dar",
                        kd = 1:3,
                        ky = 1:3, 
                        q = 1,
                        alpha = 0.05,
                        reduce = FALSE, 
                        h0 = 0)

#Plot the Contour Figure
ovb_contour_plot(darfur.model, 
                 treatment = "herder_dar",
                 sensitivity.of = "t-value", 
                 reduce = FALSE, 
                 estimate.threshold = 0, 
                 t.threshold = 1.96)

#Add the bounds to the countor figure 
add_bound_to_contour(my_bounds)

#Adjusted estimates and t-stats
my_bounds
