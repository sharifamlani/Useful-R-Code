#====================================== Lets Bootstrap ===================================================#

junk <- list(NA)
for (i in 1:1000){  
  data <- aggregate_after
  indices <- sample(1:nrow(data), nrow(data), replace=TRUE)
  #recreate the data set by pulling out Samples from the data set
  d <- data[indices,] # allows boot to select sample 
  
  #run a ols model, using our new data, "data"
  mod <- lm(cc~ 
              regiona + 
              centrality2 + 
              democracy + 
              log(GDPppp) + 
              regiona*centrality2 + factor(abbreva),
            data=d) 
  
  #then extract the our beta
  junk[[i]] <- mod$coef
}

# Look at the standard errors
A <-do.call(rbind, junk)
apply(A, 2, mean)
apply(A, 2, sd)

apply(A, 2, function(x){quantile(x, probs=.025, na.rm = TRUE)})

.8*1.96
-4.24-1.568