#####################
#The basics of logit regression in r, focusing on plotting
#213
#nofilter
#####################

###First, let's extract some variables from the mtcars dataset
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

#The resulting object contains many of the same components as a lm object
cars$coefficients
#Notice that the fitted values are converted to probabilities.
cars$fitted.values

#The summary command returns a similar object to the lm summary
#(but with small differences)
summary(cars)

#Interpretaion
# Logged odds: Every one unit increase in x there is a ____ increae in the logged odd of y


#From this summary object,
#we can extract even more information than from the basic glm object.
#Like the standard errors, for instance
summary(cars)$coefficients[,2]

###Now let's start making some basic plots

#First, look at how cool the basic plots for the object are!
plot(cars)

#****************************************Plot the Results
##Log Odds Curves
#First, Start with a curve for when HP is at its minimum
curve(cars$coefficients[1]+ #alpha
      cars$coefficients[2]*x+ #b1
      cars$coefficients[3]*min(hp), #when horse power at its min
  from = min(mpg),
  to=max(mpg),
  xlab="mpg",
  ylab="Ln(Odds Ratio)",
  main="Log Odds of a Car having a Manual Transmission"
)
curve(
  cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*as.numeric(quantile(hp)[2]), #Hp at its 1 quartile
  add=T,
  lty=2
)
#Next add a curve for when HP is at its median
curve(cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*as.numeric(quantile(hp)[3]), #HP is at is median
  add=T,
  lty=3
)
#Then add a curve for when hp is at its 75 percentile
curve(
  cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*as.numeric(quantile(hp)[4]), #when horse power is at its .75 quartile
  add=T,
  lty=4
)
#Finally, add a curve for when Hp is at its maximum
curve(
  cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*max(hp), #When horse power is at its max
  add=T,
  lty=5
)
#Now we need a nice legend to tie it together
legend(
  "bottomright",
  legend=c(min(hp),as.numeric(quantile(hp)[2]),as.numeric(quantile(hp)[3]),as.numeric(quantile(hp)[4]),max(hp)),
  lty=c(1,2,3,4,5),
  title="Horsepower"
)

##*******************************************Odds Curves
#First, Start with a curve for when HP is at its minimum
curve(
  exp( #The same code but the logged odds are expenicated, uses e
    cars$coefficients[1]+ #intercept
      cars$coefficients[2]*x+ #Applied to our x (b1)
      cars$coefficients[3]*min(hp) #b2 times hp (x2)
  ),
  from = min(mpg),
  to=max(mpg),
  xlab="mpg",
  ylab="Odds Ratio",
  main="Odds of a Car having a Manual Transmission"
)
curve(
  exp(
    cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*as.numeric(quantile(hp)[2])
  ),
  add=T,
  lty=2
)
#Next add a curve for when HP is at its median
curve(
  exp(
  cars$coefficients[1]+
    cars$coefficients[2]*x+
    cars$coefficients[3]*as.numeric(quantile(hp)[3])
),
add=T,
lty=3
)
#Then add a curve for when hp is at its 75 percentile
curve(
  exp(
  cars$coefficients[1]+
    cars$coefficients[2]*x+
    cars$coefficients[3]*as.numeric(quantile(hp)[4])
),
add=T,
lty=4
)
#Finally, add a curve for when Hp is at its maximum
curve(
  exp(
  cars$coefficients[1]+
    cars$coefficients[2]*x+
    cars$coefficients[3]*max(hp)
),
add=T,
lty=5
)
#Now we need a nice legend to tie it together
legend(
  "topleft",
  legend=c(min(hp),as.numeric(quantile(hp)[2]),as.numeric(quantile(hp)[3]),as.numeric(quantile(hp)[4]),max(hp)),
  lty=c(1,2,3,4,5),
  title="Horsepower"
)

#####Probability Curves
#an increase in hp increase the

#First, Start with a curve for when HP is at its minimum
curve(
  exp(
    cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*min(hp)
  )/(
    1+
      exp(
        cars$coefficients[1]+
          cars$coefficients[2]*x+
          cars$coefficients[3]*min(hp)
      )
  ),
  from = min(mpg),
  to=max(mpg),
  xlab="mpg",
  ylab="Probability",
  main="Probability of a Car having a Manual Transmission"
)
curve(
  exp(
    cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*as.numeric(quantile(hp)[2])
  )/(
    1+
      exp(
        cars$coefficients[1]+
          cars$coefficients[2]*x+
          cars$coefficients[3]*as.numeric(quantile(hp)[2])
      )
  ),
  add=T,
  lty=2
)
#Next add a curve for when HP is at its median
curve(
  exp(
  cars$coefficients[1]+
    cars$coefficients[2]*x+
    cars$coefficients[3]*as.numeric(quantile(hp)[3])
)/
  (1+exp(
    cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*as.numeric(quantile(hp)[3])
  )),
add=T,
lty=3
)
#Then add a curve for when hp is at its 75 percentile
curve(
  exp(
  cars$coefficients[1]+
    cars$coefficients[2]*x+
    cars$coefficients[3]*as.numeric(quantile(hp)[4])
)/
  (1+exp(
    cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*as.numeric(quantile(hp)[4])
  )),
add=T,
lty=4
)
#Finally, add a curve for when Hp is at its maximum
curve(
  exp(
  cars$coefficients[1]+
    cars$coefficients[2]*x+
    cars$coefficients[3]*max(hp)
)/
  (1+exp(
    cars$coefficients[1]+
      cars$coefficients[2]*x+
      cars$coefficients[3]*max(hp)
  )
  ),
add=T,
lty=5
)
#Now we need a nice legend to tie it together
legend(
  "bottomright",
  legend=c(min(hp),as.numeric(quantile(hp)[2]),as.numeric(quantile(hp)[3]),as.numeric(quantile(hp)[4]),max(hp)),
  lty=c(1,2,3,4,5),
  title="Horsepower"
       )

#Great, now let's move into finding marginal effects
#Marginal effects are the effects of changing one variable while holding the others constant
#For linear models, this is usually pretty easy,
#because it's usually just the coefficient of the variable.
#In a log odds model, however, the marginal effects are much more complex because they are
#neither constant nor independent of the other variables.

#margional effects compaires the previous opint to the point adhead of it. It takes the differnce to that we kno what the chnage in the curve (the porpabaility is)


marginal_effects <- rep(NA,length(min(mpg):max(mpg)))
for(i in 1:length(min(mpg):max(mpg))){
marginal_effects[i] <- exp(
  cars$coefficients[1]+
    cars$coefficients[2]*((min(mpg):max(mpg))[i]+1)+
    cars$coefficients[3]*mean(hp)
)/
  (1+
     exp(
    cars$coefficients[1]+
      cars$coefficients[2]*((min(mpg):max(mpg))[i]+1)+
      cars$coefficients[3]*mean(hp)
  )
  )-
  exp(
    cars$coefficients[1]+
      cars$coefficients[2]*(min(mpg):max(mpg))[i]+
      cars$coefficients[3]*mean(hp)
  )/
  (1+
     exp(
       cars$coefficients[1]+
         cars$coefficients[2]*(min(mpg):max(mpg))[i]+
         cars$coefficients[3]*mean(hp)
     )
  )
};marginal_effects

plot(
  marginal_effects,
  main="Marginal Effects of MPG on Probabilty of Manual Transmission",
  xlab="MPG",
  ylab = "Effect on Probabilty of a 1 unit increase in MPG",
  type="l",
  axes = F
  )
axis(
  1,
  at= (0:6)*4,
  labels=round(((max(mpg)-min(mpg))/6)*0:6+min(mpg))
    )
axis(2)
box()
