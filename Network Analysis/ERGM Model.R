#ERGM MODEL


#Here is my model!
Model5.ergm <- ergm(network.113 ~ edges  + nodematch("party", diff = T) + nodematch("state")) 
summary(Model5.ergm)

#This code turns the logged odds into odds ratios
ergm.5.Odds <- exp(coef(Model5.ergm))

#Ok, so this is where it gets interesting. This code tells you the probability of having a tie in the network (beacsue we are starting with "edges").
#think of this as the intercept or the basline starting probability of having a tie with all other paraments excluded
plogis(coef(Model5.ergm)[['edges']]) 

#So if we take the above code and add it to one of our coefficients of interests it tells us  "What's the probability of having a tie + (in my case) being a democratic and having a tie to another democrat (aka. democratic party homophily)
#If you start with the edge code you can add any coefficient to it and get the probability of having a tie in the network 
plogis(coef(Model5.ergm)[['edges']] + coef(Model5.ergm)[["nodematch.party.D"]])


#PS: uses this code: coef(Model5.ergm) to get the names of the coefficients to put into: coef(Model5.ergm)[["HERE"]])

#Like I said, I think the odds ratios will be more than sufficient. 

#This is very helpful and is how I learned about ERGM 
#https://www.r-bloggers.com/ergm-tutorial/
  
