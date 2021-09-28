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

#Make Female a Factor
darfur$female <- as.factor(darfur$female)

#LM Model
darfur.model <- lm(peacefactor ~ directlyharmed  +   female +
                     age + farmer_dar + herder_dar + pastvoted + hhsize_darfur, 
                   data = darfur)

summary(darfur.model)

#sensitivity analysis

#How can I specify treatment variable using a factor level variable?
sensitivity <- sensemakr(model = darfur.model, 
                         treatment = "female1",
                         benchmark_covariates = "directlyharmed",
                         kd = 1:3,
                         ky = 1:3, 
                         q = 1,
                         alpha = 0.05, 
                         reduce = TRUE)

