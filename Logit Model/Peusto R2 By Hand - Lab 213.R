#####################
#Pseudo R^2 for my real friends and real R^2 for my pseudo friends.
#213, May 10th
#XKCDdiditfirst
#####################

library(pscl)

#First, let's redo what we did last week,
#to get a model we can work with

#This binary dependent variable measures whether a car has manual drive,
#or whether it's for people who don't know how to really drive.
manual <- mtcars$am

#Multi-player game.
#Measures how many games can be played by the car's passengers between gas refills
mpg <- mtcars$mpg

#horsepower (measured in metric, using 10-legged horses)
hp <- mtcars$hp

#Now for the basic command for logit regression.
#Notice the simularities and differences between it and the "lm" command
cars <- glm(manual~mpg+hp,family=binomial(link=logit))

#Great, now that we've redone that and reappriacted last week's jokes,
#on to the new stuff

#The R2 is a good measure of how predicitive your model is. You can have a higly signifigant model that has a low R2
#This means that you have a stong predictor but their is still a lot of varance our their that goes unexplained

##Today's goal is to get our hands dirty with the mechanics behind various
#pseudo R^2

#our goals:
pR2(cars)

#count R^2: The number you got correct divided by the total number of n in your sample size
(
  length(
    which(
      fitted.values(cars)>.5&manual==1
      )
         )+
  length(
    which(
      fitted.values(cars)<.5&manual==0
      )
    )
  )/length(manual)

#McFadden's R^2:
#Our model compaired to a model with only an intercept
#Interpretation: the ratio of log lilkeyhoold of the full model to the null model is moderate
#AS the log likleyhood of the full model increases the null model decreases = high r squared


#Our model only at the intercept!
cars0 <- glm(manual~1,family=binomial(link=logit))
summary(cars0)

1-as.numeric(logLik(cars))/as.numeric(logLik(cars0))

#Log likelyhoold
logLik(cars)

#McFadden's adjusted R^2
1-(
  as.numeric(logLik(cars)
             )-length(cars$coefficients)+
    1)/as.numeric(logLik(cars0))

#The macFadded 1 penalized for each of its parameters

#Cox-Snell pseudo R^2 ()
1-(
  (exp(
    as.numeric(
      logLik(cars0)
      )
    )
   )/exp(
     as.numeric(
       logLik(cars)
       )
     )
  )^(2/length(manual)
     )


#McKelvey & Zavoina
var(cars$fitted.values)/(var(cars$fitted.values)+pi^(2/3))
