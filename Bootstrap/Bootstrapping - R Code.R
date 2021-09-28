#*******************************Bootstrap Function*************************

boot.beta <- function(data, indices){
  
  #recreate the data set by pulling out Samples from the data set
  d <- data[indices,] # allows boot to select sample 
  
  #run a ols model, using our new data, "data"
  mod <- lm(dexp ~ dinteg_pc10 +
              dworkoth_pc10 +
              dsolve_pc10 + 
              dqualif_pc10 + 
              dserv_pc10 +
              dgrasp_pc10 +
              dpres +
              rexp +
              incran10+
              chexp10
            , data=d) 
  
  #then extract the our beta
  mod$coef 
}

#*****************************The Bootstrap**************************************

#(our original data, our creatted function, # of times)
bootstrap.dexp <- boot(data = House.Valence, 
                       statistic=boot.beta, 
                       1000
)
bootstrap.dexp

hist(bootstrap.dexp)

#****************** Bootstrap P-Values **************

#For each Coefficient 
2*pt(abs("original")/"std. error", "n"-"Number of Coefficients", lower = FALSE)

#Examples:

#Intercept*** P < 0.0001
2*pt(33.30/6.36, 137-5, lower = FALSE)

#Incumbent Valence* P < 0.05
2*pt(2.782/1.385, 137-5, lower = FALSE)

# Incumbetn Party*** P < 0.0001
2*pt(abs(-17.657)/4.994, 137-5, lower = FALSE)

#District Partisanship*** P < 0.0001
2*pt(0.674/0.075, 137-5, lower = FALSE)

#Interaction Term .  P < 0.1
2*pt(abs(-2.821)/1.456, 137-5, lower = FALSE)
