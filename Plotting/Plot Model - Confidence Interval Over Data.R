
#Ploting Model w/ confidence interals on origional points.

############## Create a Dataframe from scratch ###################

X <- seq(1:50)
Y <- runif(50, min = 1, max = 100)


Z <-data.frame(X, Y)


############### Run Linear Model #######################
Model_1 <- lm(Y ~ X, data = Z)

############### Plot ######################
library(ggplot2)
library(sjPlot)
plot_model(Model_1, type = "pred", terms = c("X")) +
  geom_jitter(data = Z, aes(x = X, y = Y)) +
  labs(title="White Racial Identity in Relation to Reporting White Experiential Discrimination", 
       x = "Racial Identity", 
       y="Reporting Experiential Discrimination")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



################ Your Verson of the Code ####################
#Note: This will not work in this R code. But this is the code you need to use!!!
library(ggplot2)
library(sjPlot)
plot_model(OLS.1, type = "pred", terms = c("Ridentity1")) +
  geom_jitter(data = CMPS.White, aes(x = Ridentity1, y = Groupdiscrim1)) +
  labs(title="White Racial Identity in Relation to Reporting White Experiential Discrimination", 
       x = "Racial Identity", 
       y="Reporting Experiential Discrimination")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
