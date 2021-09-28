#Sharif Amlani
#R 4.0.3
#Summer 2021

######################## Code Summary ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Data #####################
library(sjPlot)

y <- sample(c(0,1), 1000, replace = 1)
x <- sample(c(1,2,3), 1000, replace = 1)
m <- sample(seq(0,1,.01), 1000, replace = 1)

df<- data.frame(y,x,m)
#df$m <- as.factor(df$m)

Model <- glm(y ~ x*m, data = df, family = "binomial")
summary(Model)
plot_model(Model, type = c("pred"), terms = c("x", "m[0,1]"))
get_model_data(Model, type = c("pred"), terms = c("x", "m[.1,.3,.5]"))
get_model_data(Model, type = c("pred"), terms = c("x", "m[0,1]"))

#As we move from M = 0 to M = 1 there is a 5% increase in the probability of Y, when X = 1
#For every one unit increase in x, there is ~ a 3 percent increase in y when M = 0
#For every one unit increase in x, there is ~ a 6 percent increase in y when M = 1


df$m_cut <- cut(df$m, breaks = 3)

Model <- glm(y ~ x*m_cut, data = df, family = "binomial")
summary(Model)
plot_model(Model, type = c("pred"), terms = c("x", "m_cut"))
get_model_data(Model, type = c("pred"), terms = c("x", "m_cut"))
