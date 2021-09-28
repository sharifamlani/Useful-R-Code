#Sharif Amlani
#R 4.0.2
#Fall 2020

######################## Code Summary ##################
#************ v1 ****************
# Tutorial for STM
# From: https://juliasilge.com/blog/sherlock-holmes-stm/

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################
library(stm)
library(gutenbergr)
library(tidyverse)
library(tidytext)

######################## Upload Data ##################

sherlock_raw <- gutenberg_download(1661)


#################### Data Management ####################
#Add the name of the story to the data frame
sherlock <- sherlock_raw %>%
  mutate(story = ifelse(str_detect(text, "ADVENTURE"),
                        text,
                        NA)) %>%
  fill(story) %>%
  filter(story != "THE ADVENTURES OF SHERLOCK HOLMES") %>%
  mutate(story = factor(story, levels = unique(story)))

sherlock

#Count the number of lines
sherlock %>% count(story)

#Add the line numbers to the short story
tidy_sherlock <- sherlock %>%
  mutate(line = row_number()) %>% #Add row numbers
  unnest_tokens(word, text) %>%  #Make one word per row
  anti_join(stop_words) %>% #Remove stop word for topic modeling
  filter(word != "holmes") #Remove "holmes" becasue it is unnessary and very frequent

tidy_sherlock %>%
  count(word, sort = TRUE)


###################### TF - IDF ##################
#The statistic tf-idf identifies words that are important to a document in a collection of documents; in this case, we'll see which words are important in one of the stories compared to the others.
#wrods that are characterist of each story

sherlock_tf_idf <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>% #Count the number of words per story
  bind_tf_idf(word, story, n) %>% #How important is this words to this dorucment, comparied to the other documents in the collection
  arrange(-tf_idf) %>%
  group_by(story) %>%
  top_n(10) %>%
  ungroup

sherlock_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, story)) %>%
  ggplot(aes(word, tf_idf, fill = story)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ story, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in Sherlock Holmes short stories",
       subtitle = "Individual stories focus on different characters and narrative elements")

###################### Implement Topic Modeling ##################
#***************** Data Management ********************
#This code creates the necessary data frame to input into STM package

library(quanteda)
library(stm)

sherlock_dfm <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>%
  cast_dfm(story, word, n)

sherlock_sparse <- tidy_sherlock %>%
  count(story, word, sort = TRUE) %>%
  cast_sparse(story, word, n)

#****************** STM Package ***********************
topic_model <- stm(sherlock_dfm,  #Data
                   K = 6,         #6 Topic, topic molde
                   verbose = T, 
                   init.type = "Spectral")

#A topic model asks: How many topics are there in your text?
#Unsupervided machine learned to ask which words contribute to which topics? Then, which topics contribute to which documents?
#So there is freedome for differnce so then words can contribute to at different proportions to different topics. Topics can contribute in differnet porpotions to different documents

summary(topic_model)

#****************** Examine the Beta Matrix ***********************
#Beta Matrix
td_beta <- tidy(topic_model)

#Beta matrix tells us what are the words that contribute to each topic

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

#****************** Examine the Gamma Matrix ***********************

td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(sherlock_dfm))

#Gamma: In this document, how much did this topic contribute to it.

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))

#gamma propobability tells us the probabiliy that a document belongs in a topic
#So, the figure tells us that the the stories were either stored into one topic (1) or not (0). There was no ambiguity as to which topic the stories belonged in.
#For example, there is one story that belongs in topic 2 and 11 that do not.
#The topics are being assocated very strongly with the stories, and not assocated with other stories