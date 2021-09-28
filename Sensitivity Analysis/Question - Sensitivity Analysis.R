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

######################### Library #####################
library(sensemakr)
library(lme4) # mixed random effects models
library(lmtest) # coeftest function
library(lmerTest) # p-values for mixed random effects models
library(multiwayvcov) # clustered standard errors
library(broom)
######################## Upload Data ##################
data("darfur")

#LM Model
darfur.model <- lm(peacefactor ~ directlyharmed  + village +  female +
                     age + farmer_dar + herder_dar + pastvoted + hhsize_darfur, 
                   data = darfur)

summary(darfur.model)

#coeftest 
darfur.model_cluster <- coeftest(darfur.model, vcov = cluster.vcov(darfur.model, cluster=darfur$village)) # Model 1 Linear Estimator; OLS State Fixed-Effects with Clustered Standard Errors--Total Covid Deaths Per 100K


#sensitivity analysis

#How can I specify benchmark_covariates' estimates and se using numeric class of the sensemakr function?
darfur.sensitivity <- sensemakr(estimate = subset(tidy(darfur.model_cluster), term %in% c("directlyharmed"))$estimate, 
                                se =subset(tidy(darfur.model_cluster), term %in% c("directlyharmed"))$std.error, 
                                dof = df.residual(darfur.model_cluster),
                                treatment = "directlyharmed",
                                benchmark_covariates = "female",
                                kd = 1:3,
                                ky = 1:3, 
                                q = 1,
                                alpha = 0.05, 
                                reduce = TRUE)

summary(darfur.sensitivity)
