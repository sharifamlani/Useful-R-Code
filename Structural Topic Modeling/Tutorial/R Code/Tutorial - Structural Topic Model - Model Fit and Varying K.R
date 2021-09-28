#Sharif Amlani
#R 4.0.2
#Fall 2020

######################## Code Summary ##################
#************ v1 ****************
#I run the STM package on open ended responces

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################
library(stm)
library(quanteda)
library(tidyverse)
library(tidytext)

######################## Upload Data ##################

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/University of California, Davis/Research Projects/Immigration One-Word Project/Data/Open Ended Answers")

#Upload Data
Words_Pure <- read.csv("mergedQ3and4.csv"); Words.1 <- Words_Pure

Words.1$Respondent <- Words.1$ï..v1; Words.1$ï..v1<- NULL

################### Examine Data ################
head(Words.1)

#################### Descriptive Stats ###############
head(Words.1)

Words.2 <- with(Words.1, data.frame(Respondent, bd_blameopen))

Words.3 <- Words.2 %>%
  mutate(line = row_number()) %>% #Add row numbers
  unnest_tokens(word, bd_blameopen) %>%  #Make one word per row
  anti_join(stop_words) #Remove stop word for topic modeling

Words.3 %>%
  count(word, sort = TRUE)

######################## Text Editor ##################

#Process the Text
processed <- stm::textProcessor(Words.1$bd_blameopen, metadata = Words.1)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

#Plot the words removed
#I  use the ployRemoved function to evaluate how many words and documents would be removed from the data set at each word threshold, 
#which is the minimum number of documents a word needs to appear in order for the word to be kept within the vocabulary.

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

#Im going to lowe the threshold to 15 to maintain the most words
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)


####################### Notes: Topic Modeling Basics ##################
#************** Purpose 
#Topical prevalence captures how much each topic contributes to a document.

#************** Covariates
#We use the ratings variable (blog ideology) as a covariate in the topic prevalence portion of the model
#We will let prevalence be a function of the "rating" variable, which is coded as either "Liberal" or "Conservative," and the variable "day" which is an integer measure of days running from the first to the last day of 2008.

#A feature of the stm function is that "prevalence" can be expressed as a formula that can include multiple covariates and factorial or continuous covariates.
#For example, by using the formula setup we can enter other covariates additively.

#************** Number of Topics
#We now estimate a K number topic STM model.

####################### Topic Modeling Models: Base Model ##################
#Three Topics - First Pass of Three Topics

#Base Model
Results_1 <- stm(documents = out$documents, vocab = out$vocab,
                 K = 3, 
                 max.em.its = 75, #The argument max.em.its sets the maximum number of iterations.
                 data = out$meta, init.type = "Spectral")

summary(Results_1)

#**************** Residuals *******************
#Low is Good

# The basic idea is that when the model is correctly specified the multinomial likelihood implies a dispersion of the residuals: ??^2=1. 
# If we calculate the sample dispersion and the value is greater than one, this implies that the number of topics is set too low, because the latent topics are not able to account for the overdispersion. 
# In practice this can be a very demanding criterion, especially if the documents are long

stm::checkResiduals(Results_1, out$documents)$dispersion

#*************** Semantic coherence ****************

# The paper details a series of manual evaluations which show that their metric is a reasonable surrogate for human judgment. 
# The core idea here is that in models which are semantically coherent the words which are most probable under a topic should co-occur within the same document.
stm::semanticCoherence(Results_1, out$documents)

#**************** Exclusivity ******************
#Our exclusivity measure includes some information on word frequency as well.
exclusivity(Results_1, M = 10, frexw = 0.7)

###################### Vary K Levels ########################
#High Held-out likelihood is good

#Here we vary K to find the most optimal model fit
Results_K <- stm::searchK(out$documents, out$vocab, K = seq(2,15,1), data = out$meta)

summary(Results_K)
