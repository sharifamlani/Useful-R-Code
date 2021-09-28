#Sharif Amlani
#R 4.0.2
#Summer 2020

######################## Code Summary ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################

######################## Upload Data ##################

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/Data/House and Senate Election Data")

#Upload Data
library(readstata13)
House.Pure <- read.dta13("house_elex_1900_2018.dta"); House.1 <- House.Pure

####################### Examine Data ####################
head(House.1)


###################### First Digit After Decimal ##################

#Data Management 
House.1$F_Decimal <- as.numeric(substr(sapply(strsplit(as.character(House.1$dv), "[.]"), `[`, 2),1, 1))

#Check - YUP
head(substr(sapply(strsplit(as.character(House.1$dv), "[.]"), `[`, 2),1, 1))
head(House.1$dv)

#Histogram
table(House.1$F_Decimal)
hist(House.1$F_Decimal)

#Benfords Law does not hold for Election Data


###################### Isolate First Digit ##################

#Data Management 
House.1$F_Digit<- as.numeric(substr(House.1$dv,1, 1))

#Check - YUP
head(House.1$F_Digit)
head(House.1$dv)

#Histogram
table(House.1$F_Digit)
hist(House.1$F_Digit)

###################### Isolate Second Digit ##################

#Data Management 
House.1$S_Digit<- as.numeric(substr(House.1$dv,2, 2))

#Check - YUP
head(House.1$S_Digit)
head(House.1$dv)

#Histogram
table(House.1$S_Digit)
hist(House.1$S_Digit)

mean(House.1$S_Digit, na.rm = T)


