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

darfur.model <- lm(peacefactor ~ directlyharmed  + village +  female +
                     age + farmer_dar + herder_dar + pastvoted + hhsize_darfur, 
                   data = darfur)

summary(darfur.model)

# runs sensemakr for sensitivity analysis
# in the darfur example
darfur.sensitivity <- sensemakr(model = darfur.model, 
                                treatment = "directlyharmed",
                                benchmark_covariates = "female",
                                kd = 1:3,
                                ky = 1:3, 
                                q = 1,
                                alpha = 0.05, 
                                reduce = TRUE)

#The print method of sensemakr provides a quick review of the original (unadjusted) estimate along with three summary sensitivity statistics suited for routine reporting: 
  #the partial R2 of the treatment with the outcome, 
  #the robustness value (RV) required to reduce the estimate entirely to zero (i.e. q=1), 
  #and the RV beyond which the estimate would no longer be statistically distinguishable from zero at the 0.05 level (q=1, a=0.05).

#Entering summary(darfur.sensitivity) produces verbose output similar to the text explanations in the last several paragraphs, so that researchers can directly cite or include such text in their reports.
summary(darfur.sensitivity)


ovb_minimal_reporting(darfur.sensitivity, format = "latex")

######################### Plot #############################
#Sensitivity contour plots of point estimates 
plot(darfur.sensitivity)
#The contours show what would be the estimate for directlyharmed that one would have obtained in the full regression model including unobserved confounders with such hypothetical strengths. 
#Note the plot is parameterized in way that hurts our preferred hypothesis, by pulling the estimate towards zero-the direction of the bias was set in the argument reduce = TRUE 


#Sensitivity contour plots of t-values
plot(darfur.sensitivity, sensitivity.of = "t-value")
#We now examine the sensitivity of the t-value for testing the null hypothesis of zero effect. 
#For this, it suffices to change the option sensitivity.of = "t-value".

#The plot reveals that, at the 5% significance level, the null hypothesis of zero effect would still be rejected given confounders once or twice as strong as female. 
#However, by contrast to the point-estimate, accounting for sampling uncertainty now means that the null hypothesis of zero effect would not be rejected with the inclusion of a confounder three times as strong as female.


######################### Stats #############################
darfur.sensitivity$sensitivity_stats
darfur.sensitivity$bounds


###################### Plot: Bar Plot of Robusiness Values  ######################
darfur.sensitivity_df <- cbind(data.frame(darfur.sensitivity$sensitivity_stats), data.frame(darfur.sensitivity$bounds)[1,]); darfur.sensitivity_df$DV <- "directlyharmed"

darfur.sensitivity_df2 <- reshape2::melt(with(darfur.sensitivity_df, data.frame(DV, r2yd.x, rv_q, rv_qa)))

#Recode Labels
darfur.sensitivity_df2$label[darfur.sensitivity_df2$variable == "r2yd.x"] <- "Partial R2 of Extreme Confounder"
darfur.sensitivity_df2$label[darfur.sensitivity_df2$variable == "rv_q"] <- "Robustness Value: Effect Size == 0"
darfur.sensitivity_df2$label[darfur.sensitivity_df2$variable == "rv_qa"] <- "Robustness Value: Statistical Significance"

darfur.sensitivity_df2$value_percent <- darfur.sensitivity_df2$value*100

ggplot(darfur.sensitivity_df2, aes(x= label, y = value_percent, fill = label, label = round(value_percent, 2))) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  geom_text(size = 4, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = "", 
       y = "Residual Variance (Percent)",
       title = "Robustness Values For Unobserved Confounders",
       subtitle = "Sensitivity Analysis",
       caption = "Note: Statistical significance defined as p < 0.05")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position="none")


###################### Plot: Confounder Plot  ######################
Stats_DF <- data.frame(darfur.sensitivity$sensitivity_stats)[c("treatment",   "estimate",   "se", "t_statistic")]
Bound_DF <-data.frame(darfur.sensitivity$bounds)[c("bound_label","adjusted_estimate","adjusted_se", "adjusted_t", "adjusted_lower_CI","adjusted_upper_CI")]

dof <- darfur.sensitivity$sensitivity_stats$dof

rv_qa <- darfur.sensitivity$sensitivity_stats["rv_qa"][[1]][1]
r2dz.x <- darfur.sensitivity$bounds[1,2] * 100
r2yz.dx <- darfur.sensitivity$bounds[1,3] * 100

Stats_DF$lower <-  Stats_DF$estimate - qt(.975, dof) * Stats_DF$se
Stats_DF$upper <-  Stats_DF$estimate + qt(.975, dof) * Stats_DF$se

colnames(Bound_DF) <- colnames(Stats_DF)

Plot_DF <- rbind(Stats_DF, Bound_DF)
Plot_DF[1,1] <- "Actual Estimate"

Plot_DF$treatment <- gsub('[[:punct:] ]+',' ',Plot_DF$treatment)
New_Levels <- rev(levels(factor(Plot_DF$treatment))[1:(length(unique(levels(factor(Plot_DF$treatment))))-1)])
Plot_DF$treatment <- factor(Plot_DF$treatment, levels = c(New_Levels, "Actual Estimate"))

ggplot(data = Plot_DF, 
       aes(x = treatment, y = estimate, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  theme_bw() +
  labs(x= "Strength of Unknown Confounder", 
       y = "DV Estimate", 
       title = "Change in Estimate Size and Statistical Significance",
       #  subtitle = "Main Effects",
       caption = paste("Partial R2 of X with the treatment D = ", round(r2dz.x, 2),
                       "\nPartial R2 of X with the outcome Y = ", round(r2yz.dx, 2), sep = "")) +
  geom_hline(yintercept = 0,  colour = gray(1/2), lty = 2) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  coord_flip()


###################### Plot: Confounder Plot Function  ######################

Confound_Plot_Df <- function(sensitivity_object){
  Stats_DF <- data.frame(sensitivity_object$sensitivity_stats)[c("treatment",   "estimate",   "se", "t_statistic")]
  Bound_DF <-data.frame(sensitivity_object$bounds)[c("bound_label","adjusted_estimate","adjusted_se", "adjusted_t", "adjusted_lower_CI","adjusted_upper_CI")]
  
  dof <- sensitivity_object$sensitivity_stats$dof
  
  rv_qa <- sensitivity_object$sensitivity_stats["rv_qa"][[1]][1]
  r2dz.x <- sensitivity_object$bounds[1,2] * 100
  r2yz.dx <- sensitivity_object$bounds[1,3] * 100
  
  Stats_DF$lower <-  Stats_DF$estimate - qt(.975, dof) * Stats_DF$se
  Stats_DF$upper <-  Stats_DF$estimate + qt(.975, dof) * Stats_DF$se
  
  colnames(Bound_DF) <- colnames(Stats_DF)
  
  Plot_DF <- rbind(Stats_DF, Bound_DF)
  Plot_DF[1,1] <- "Actual Estimate"
  
  Plot_DF$treatment <- gsub('[[:punct:] ]+',' ',Plot_DF$treatment)
  New_Levels <- rev(levels(factor(Plot_DF$treatment))[1:(length(unique(levels(factor(Plot_DF$treatment))))-1)])
  Plot_DF$treatment <- factor(Plot_DF$treatment, levels = c(New_Levels, "Actual Estimate"))
  
  return(list(Plot_DF = Plot_DF, r2dz.x=r2dz.x,r2yz.dx=r2yz.dx, rv_qa = rv_qa))
}

df <- Confound_Plot_Df(darfur.sensitivity)

ggplot(data = df$Plot_DF, 
       aes(x = treatment, y = estimate, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  theme_bw() +
  labs(x= "Strength of Unknown Confounder", 
       y = "DV Estimate", 
       title = "Change in Estimate Size and Statistical Significance",
       #  subtitle = "Main Effects",
       caption = paste("Partial R2 of X with the treatment D = ", round(df$r2dz.x, 2),
                       "\nPartial R2 of X with the outcome Y = ", round(df$r2yz.dx, 2), sep = "")) +
  geom_hline(yintercept = 0,  colour = gray(1/2), lty = 2) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  coord_flip()


###################### Manuel Sensativity Analysis ###################
######################## Upload Data ##################
data("darfur")

darfur.model <- lm(peacefactor ~ directlyharmed  + village +  female +
                     age + farmer_dar + herder_dar + pastvoted + hhsize_darfur, 
                   data = darfur)

summary(darfur.model)

#********************** Truth ********************
#Truth
darfur.sensitivity_truth <- sensemakr(model = darfur.model, 
                                treatment = "directlyharmed",
                                benchmark_covariates = "female",
                                kd = 1:3,
                                ky = 1:3, 
                                q = 1,
                                alpha = 0.05, 
                                reduce = TRUE)

summary(darfur.sensitivity_truth)


partial_r2(darfur.model, covariates = "female")
partial_r2(darfur.model, covariates = "directlyharmed")

#********************* Hand: Full Model ********************
#1. The treatment on the covariates 
darfur.model.r2dxj.x <- lm(directlyharmed  ~ village +  female +
                     age + farmer_dar + herder_dar + pastvoted + hhsize_darfur, 
                   data = darfur)

T.Stat_bx <- subset(tidy(darfur.model.r2dxj.x), term %in% c("female"))$statistic; DOF <- df.residual(darfur.model.r2dxj.x)

r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)


#2. Regression of the outcome on the treatment and the covariates 
darfur.model.r2yxj.dx <- lm(peacefactor ~ directlyharmed  + village +  female +
                     age + farmer_dar + herder_dar + pastvoted + hhsize_darfur, 
                   data = darfur)


T.Stat_dx <- subset(tidy(darfur.model.r2yxj.dx), term %in% c("female"))$statistic; DOF <- df.residual(darfur.model.r2yxj.dx)

r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


darfur.sensitivity_email <- sensemakr(estimate = subset(tidy(darfur.model), term %in% c("directlyharmed"))$estimate, 
                                     se =subset(tidy(darfur.model), term %in% c("directlyharmed"))$std.error, 
                                     dof = df.residual(darfur.model),
                                     treatment = "directlyharmed",
                                     benchmark_covariates = "female",
                                     kd = 1:3,
                                     ky = 1:3, 
                                     q = 1,
                                     r2dxj.x = r2dxj.x_value,
                                     r2yxj.dx = r2yxj.dx_value,
                                     alpha = 0.05, 
                                     reduce = TRUE)


#In correct
summary(darfur.sensitivity_email)

#***************** Compare ******************
darfur.sensitivity_truth$bound
darfur.sensitivity_email$bound
#Both Check Out! Method Found.

####################### Make A By Hand Function #########################
#DV
#IV
#Benchmark
#Controls (!With the Benchmark and IV Excluded!)
#Model Type
#Cluster - Where to cluster SE
########################### V1: Set Up ########################### 

#Set Up
Model_Type <- NULL
d <- darfur
DV <- "peacefactor"
IV <- "directlyharmed"
Benchmark <- "female"
Controls <- c("age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur","village")
Cluster <- NULL

#1. The treatment on the covariates (Excluding the DV)
model.r2dxj.x <- lm(as.formula(paste(IV,"~",Benchmark, " + " ,paste(Controls, collapse = " + "))), 
                           data = d)

T.Stat_bx <- subset(tidy(model.r2dxj.x), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2dxj.x)

r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)


#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lm(as.formula(paste(DV,"~",IV, " + ", Benchmark, " + " ,paste(Controls, collapse = " + "))), 
                            data = d)

T.Stat_dx <- subset(tidy(model.r2yxj.dx), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2yxj.dx)

r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


########################### V1: Define Function -  PNAS Function ########################### 

pr2_hand <- function(Model_Type = NULL, d, DV, IV, Benchmark,Controls){
  #1. The treatment on the covariates (Excluding the DV)
  model.r2dxj.x <- lm(as.formula(paste(IV,"~",Benchmark, " + " ,paste(Controls, collapse = " + "))), 
                      data = d)
  
  T.Stat_bx <- subset(tidy(model.r2dxj.x), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2dxj.x)
  
  r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)
  
  
  #2. Regression of the outcome on the treatment and the covariates 
  model.r2yxj.dx <- lm(as.formula(paste(DV,"~",IV, " + ", Benchmark, " + " ,paste(Controls, collapse = " + "))), 
                       data = d)
  
  T.Stat_dx <- subset(tidy(model.r2yxj.dx), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2yxj.dx)
  
  r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)
  
  return(list(r2dxj.x_value = r2dxj.x_value, r2yxj.dx_value = r2yxj.dx_value))
  
}

pr2_hand(d =darfur,
         DV = "peacefactor",
         IV ="directlyharmed",
         Benchmark ="female",
         Controls = c("age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur","village"))


########################### V2: Set Up -  PNAS Function ########################### 

#******************** Fixed Effect Framework *************************
#1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
model.r2dxj.x <- lm(directlyharmed  ~ female + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + as.factor(village), 
                    data = darfur)

model.r2dxj.x_cluster <- coeftest(model.r2dxj.x, vcov = cluster.vcov(model.r2dxj.x, cluster=darfur["village"])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

T.Stat_bx <- subset(tidy(model.r2dxj.x_cluster), term %in% c("female"))$statistic; DOF <- df.residual(model.r2dxj.x_cluster)

r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)


#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lm(peacefactor ~ directlyharmed + female + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + as.factor(village), 
                     data = darfur)

model.r2yxj.dx_cluster <- coeftest(model.r2yxj.dx, vcov = cluster.vcov(model.r2yxj.dx, cluster=darfur["village"])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

T.Stat_dx <- subset(tidy(model.r2yxj.dx_cluster), term %in% c("female"))$statistic; DOF <- df.residual(model.r2yxj.dx_cluster)

r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


#******************** Random Effect Framework *************************
#1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
model.r2dxj.x <- lmer(directlyharmed  ~ female + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + (1 | village),
                      data = darfur); # Model 1 Linear Estimator; State-Level Random Effects Model Estimation--Total Covid Cases Per 100K


T.Stat_bx <- subset(Alternative_ModelDF(model.r2dxj.x, Type = "RE"), Label %in% c("female"))$statistic; DOF <- df.residual(model.r2dxj.x)

r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)

#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lmer(peacefactor ~ directlyharmed  + female + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + (1 | village),
                       data = darfur)

T.Stat_dx <- subset(Alternative_ModelDF(model.r2yxj.dx, Type = "RE"), Label %in%  c("female"))$statistic; DOF <- df.residual(model.r2yxj.dx)

r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


########################### V2: Input Coefficients -  PNAS Function ########################### 
Model_Type <- NULL
d <- darfur
DV <- "peacefactor"
IV <- "directlyharmed"
Benchmark <- "female"
Controls <- c("age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur")
Effects <- "village"
#******************** Fixed Effect Framework *************************
#1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
model.r2dxj.x <- lm(as.formula(paste(IV,"~",Benchmark, " + " ,paste(Controls, collapse = " + "), "+ as.factor(", Effects, ")")), 
                    data = d)

model.r2dxj.x_cluster <- coeftest(model.r2dxj.x, vcov = cluster.vcov(model.r2dxj.x, cluster=d[Effects])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

T.Stat_bx <- subset(tidy(model.r2dxj.x_cluster), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2dxj.x_cluster)

r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)


#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lm(as.formula(paste(DV,"~",IV, " + ", Benchmark, " + " ,paste(Controls, collapse = " + "), "+ as.factor(", Effects, ")")),  
                     data = d)

model.r2yxj.dx_cluster <- coeftest(model.r2yxj.dx, vcov = cluster.vcov(model.r2yxj.dx, cluster=d[Effects])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

T.Stat_dx <- subset(tidy(model.r2yxj.dx_cluster), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2yxj.dx_cluster)

r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


#******************** Random Effect Framework *************************
#1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
model.r2dxj.x <- lmer(as.formula(paste(IV,"~",Benchmark, " + " ,paste(Controls, collapse = " + "), "+ (1 | ", Effects, ")")),
                      data = d); # Model 1 Linear Estimator; State-Level Random Effects Model Estimation--Total Covid Cases Per 100K


T.Stat_bx <- subset(Alternative_ModelDF(model.r2dxj.x, Type = "RE"), Label %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2dxj.x)

r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)

#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lmer(as.formula(paste(DV,"~",IV, " + ", Benchmark, " + " , paste(Controls, collapse = " + "), "+ (1 | ", Effects, ")")),
                     data = d)

T.Stat_dx <- subset(Alternative_ModelDF(model.r2yxj.dx, Type = "RE"), Label %in%  c(Benchmark))$statistic; DOF <- df.residual(model.r2yxj.dx)

r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


########################### V2: Define Function -  PNAS Function ########################### 

pr2_hand_adv <- function(Model_Type, d, DV, IV, Benchmark,Controls, Effects){
  
  if(Model_Type == "Fixed Effects"){
  
  #******************** Fixed Effect Framework *************************
  #1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
  model.r2dxj.x <- lm(as.formula(paste(IV,"~",Benchmark, " + " ,paste(Controls, collapse = " + "), "+ as.factor(", Effects, ")")), 
                      data = d)
  
  model.r2dxj.x_cluster <- coeftest(model.r2dxj.x, vcov = cluster.vcov(model.r2dxj.x, cluster=d[Effects])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K
  
  T.Stat_bx <- subset(tidy(model.r2dxj.x_cluster), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2dxj.x_cluster)
  
  r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)
  
  
  #2. Regression of the outcome on the treatment and the covariates 
  model.r2yxj.dx <- lm(as.formula(paste(DV,"~",IV, " + ", Benchmark, " + " ,paste(Controls, collapse = " + "), "+ as.factor(", Effects, ")")),  
                       data = d)
  
  model.r2yxj.dx_cluster <- coeftest(model.r2yxj.dx, vcov = cluster.vcov(model.r2yxj.dx, cluster=d[Effects])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K
  
  T.Stat_dx <- subset(tidy(model.r2yxj.dx_cluster), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2yxj.dx_cluster)
  
  r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)
  
  return(list(r2dxj.x_value = r2dxj.x_value, r2yxj.dx_value = r2yxj.dx_value))
  
  
  }
  
  if(Model_Type == "Random Effects"){
    
  
  #******************** Random Effect Framework *************************
  #1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
  model.r2dxj.x <- lmer(as.formula(paste(IV,"~",Benchmark, " + " ,paste(Controls, collapse = " + "), "+ (1 | ", Effects, ")")),
                        data = d); # Model 1 Linear Estimator; State-Level Random Effects Model Estimation--Total Covid Cases Per 100K
  
  summary(model.r2dxj.x)
  
  T.Stat_bx <- subset(Alternative_ModelDF(model.r2dxj.x, Type = "RE"), Label %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2dxj.x)
  
  r2dxj.x_value  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)
  
  #2. Regression of the outcome on the treatment and the covariates 
  model.r2yxj.dx <- lmer(as.formula(paste(DV,"~",IV, " + ", Benchmark, " + " , paste(Controls, collapse = " + "), "+ (1 | ", Effects, ")")),
                         data = d)
  
  T.Stat_dx <- subset(Alternative_ModelDF(model.r2yxj.dx, Type = "RE"), Label %in%  c(Benchmark))$statistic; DOF <- df.residual(model.r2yxj.dx)
  
  r2yxj.dx_value <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)
  
  return(list(r2dxj.x_value = r2dxj.x_value, r2yxj.dx_value = r2yxj.dx_value))
  
  }
  
  
}
  
pr2_hand_adv(Model_Type = "Fixed Effects",
             d = darfur,
             DV = "peacefactor",
             IV = "directlyharmed",
             Benchmark = "female",
             Controls = c("age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur"),
             Effects = "village")


pr2_hand_adv(Model_Type = "Random Effects",
             d = darfur,
             DV = "peacefactor",
             IV = "directlyharmed",
             Benchmark = "female",
             Controls = c("age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur"),
             Effects = "village")


########################### Checks: Fixed Effects ##############################
Model_Type <- NULL
d <- darfur
DV <- "peacefactor"
IV <- "directlyharmed"
Benchmark <- "female"
Controls <- c("age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur")
Effects <- "village"

#***************************** STEP 1 ******************************
#************************ Fixed Effects: Hand
#1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
model.r2dxj.x <- lm(directlyharmed  ~ female + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + as.factor(village), 
                    data = darfur)

model.r2dxj.x_cluster <- coeftest(model.r2dxj.x, vcov = cluster.vcov(model.r2dxj.x, cluster=darfur["village"])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

T.Stat_bx <- subset(tidy(model.r2dxj.x_cluster), term %in% c("female"))$statistic; DOF <- df.residual(model.r2dxj.x_cluster)

r2dxj.x_value_F  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)


#******************** Fixed Effect Framework: Formula
#1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
model.r2dxj.x <- lm(as.formula(paste(IV,"~",Benchmark, " + " ,paste(Controls, collapse = " + "), "+ as.factor(", Effects, ")")), 
                    data = d)

model.r2dxj.x_cluster <- coeftest(model.r2dxj.x, vcov = cluster.vcov(model.r2dxj.x, cluster=d[Effects])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

T.Stat_bx <- subset(tidy(model.r2dxj.x_cluster), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2dxj.x_cluster)

r2dxj.x_value_H  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)



#**************** Check
r2dxj.x_value_H == r2dxj.x_value_F

#***************************** STEP 2 ******************************
#************************ Fixed Effects: Hand
#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lm(peacefactor ~ directlyharmed + female + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + as.factor(village), 
                     data = darfur)

model.r2yxj.dx_cluster <- coeftest(model.r2yxj.dx, vcov = cluster.vcov(model.r2yxj.dx, cluster=darfur["village"])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

T.Stat_dx <- subset(tidy(model.r2yxj.dx_cluster), term %in% c("female"))$statistic; DOF <- df.residual(model.r2yxj.dx_cluster)

r2yxj.dx_value_H <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


#******************** Fixed Effect Framework: Formula
#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lm(as.formula(paste(DV,"~",IV, " + ", Benchmark, " + " ,paste(Controls, collapse = " + "), "+ as.factor(", Effects, ")")),  
                     data = d)

model.r2yxj.dx_cluster <- coeftest(model.r2yxj.dx, vcov = cluster.vcov(model.r2yxj.dx, cluster=d[Effects])) # Model 1 Linear Model Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Cases Per 100K

T.Stat_dx <- subset(tidy(model.r2yxj.dx_cluster), term %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2yxj.dx_cluster)

r2yxj.dx_value_F <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


#**************** Check
r2yxj.dx_value_H == r2yxj.dx_value_F

#********************** Function *****************
pr2_hand_adv(Model_Type = "Fixed Effects",
             d = darfur,
             DV = "peacefactor",
             IV = "directlyharmed",
             Benchmark = "female",
             Controls = c("age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur"),
             Effects = "village")

####################### Check: Random Effects ##########################
#************************* Random Effects: Hand

#1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
model.r2dxj.x <- lmer(directlyharmed  ~ female + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + (1 | village),
                      data = darfur); # Model 1 Linear Estimator; State-Level Random Effects Model Estimation--Total Covid Cases Per 100K


T.Stat_bx <- subset(Alternative_ModelDF(model.r2dxj.x, Type = "RE"), Label %in% c("female"))$statistic; DOF <- df.residual(model.r2dxj.x)

r2dxj.x_value_re_H  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)

#1. The treatment on the covariates (Excluding the DV) -- Cluster SE here because I do so in the DV Model.
model.r2dxj.x <- lmer(as.formula(paste(IV,"~",Benchmark, " + " ,paste(Controls, collapse = " + "), "+ (1 | ", Effects, ")")),
                      data = d); # Model 1 Linear Estimator; State-Level Random Effects Model Estimation--Total Covid Cases Per 100K


T.Stat_bx <- subset(Alternative_ModelDF(model.r2dxj.x, Type = "RE"), Label %in% c(Benchmark))$statistic; DOF <- df.residual(model.r2dxj.x)

r2dxj.x_value__re_F  <- partial_r2(t_statistic = T.Stat_bx, dof = DOF)


r2dxj.x_value_re_H ==r2dxj.x_value__re_F


#************************* Random Effects: Formula

#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lmer(as.formula(paste(DV,"~",IV, " + ", Benchmark, " + " , paste(Controls, collapse = " + "), "+ (1 | ", Effects, ")")),
                       data = d)

T.Stat_dx <- subset(Alternative_ModelDF(model.r2yxj.dx, Type = "RE"), Label %in%  c(Benchmark))$statistic; DOF <- df.residual(model.r2yxj.dx)

r2yxj.dx_value_re_F <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


#2. Regression of the outcome on the treatment and the covariates 
model.r2yxj.dx <- lmer(peacefactor ~ directlyharmed  + female + age + farmer_dar + herder_dar + pastvoted + hhsize_darfur + (1 | village),
                       data = darfur)

T.Stat_dx <- subset(Alternative_ModelDF(model.r2yxj.dx, Type = "RE"), Label %in%  c("female"))$statistic; DOF <- df.residual(model.r2yxj.dx)

r2yxj.dx_value_re_H <- partial_r2(t_statistic = T.Stat_dx, dof = DOF)


r2yxj.dx_value_re_F == r2yxj.dx_value_re_H


#************************* Random Effects: Function

pr2_hand_adv(Model_Type = "Random Effects",
             d = darfur,
             DV = "peacefactor",
             IV = "directlyharmed",
             Benchmark = "female",
             Controls = c("age", "farmer_dar", "herder_dar", "pastvoted", "hhsize_darfur"),
             Effects = "village")

