#Sharif Amlani
#R 4.0.2
#Fall 2020

######################## Code Summary ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################
library(stm)
library(quanteda)

######################## Upload Data ##################

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/R-Scripts/Structural Topic Modeling/Tutorial/Data")

#Upload Data
data <- read.csv("poliblogs2008.csv")

######################## Data Management ##################

#Process the Text
processed <- stm::textProcessor(data$documents, metadata = data)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#The utility function plotRemoved will plot the number of words and documents removed for different thresholds.
#I can use the ployRemoved function to evaluate how many words and documents would be removed from the data set at each word threshold, which is the minimum number of documents a word needs to appear in order for the word to be kept within the vocabulary.
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)


####################### Topic Modeling ##################
#************** Purpose 
#Topical prevalence captures how much each topic contributes to a document.

#************** Covariates
#We use the ratings variable (blog ideology) as a covariate in the topic prevalence portion of the model
#We will let prevalence be a function of the "rating" variable, which is coded as either "Liberal" or "Conservative," and the variable "day" which is an integer measure of days running from the first to the last day of 2008.

#A feature of the stm function is that "prevalence" can be expressed as a formula that can include multiple covariates and factorial or continuous covariates.
#For example, by using the formula setup we can enter other covariates additively.
#************** Number of Topics
#We now estimate a 20 topic STM model.

poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                          K = 20, 
                          prevalence =~ rating + s(day), 
                          max.em.its = 75,
                          data = out$meta, init.type = "Spectral")