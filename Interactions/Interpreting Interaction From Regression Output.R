
################ Two Categorical variables ##################
#Stack Overflow
#See: https://stackoverflow.com/questions/63516591/calculate-by-hand-fitted-values-of-an-interaction-from-a-regression-output

set.seed(1993)

moderating <- sample(c("Yes", "No"),100, replace = T)
x <- sample(c("Yes", "No"), 100, replace = T)
y <- sample(1:100, 100, replace = T)

df <- data.frame(y, x, moderating)

Results <- lm(y ~ x*moderating)
summary(Results)

#Interpretation
#Interaction Term: the combined effect (and boost/decrease) when both additive terms are present (1)


#Calulate Fitted Value From a Regression Interaction by hand
#Omitted Variable = X_no.M_no

X_no.M_no <- 52.4000
X_yes.M_no <- 52.4000 + 8.4571 
X_no.M_yes <- 52.4000 + -11.4435
X_yes.M_yes <- 52.4000 + 8.4571 - 11.4435 - 0.1233


#Validated Here Using the Predict Function:
newdat <- NULL
for(m in na.omit(unique(df$moderating))){
  for(i in na.omit(unique(df$x))){
    moderating <- m
    x <- i
    
    newdat<- rbind(newdat, data.frame(x, moderating))
    
  }
}

Prediction.1 <- cbind(newdat, predict(Results, newdat, se.fit = TRUE))
Prediction.1


################ One Categorical and One Continuious variables ##################
set.seed(1993)

moderating <- sample(c("Yes", "No"),100, replace = T)
x <- sample(1:100, 100, replace = T)
y <- sample(1:100, 100, replace = T)

df <- data.frame(y, x, moderating)

Results <- lm(y ~ x*moderating)
summary(Results)

#Interpretation
#The difference in the slope when the categorical vaiable is 0 and when it is 1

#Calulate Fitted Value From a Regression Interaction by hand
#Omitted Variable = M_no

Intercept <- 50.301
Slope_X.M_no <- 0.15015639
Slopt_X.M_yes <- 0.15015639 + 0.00001113 



