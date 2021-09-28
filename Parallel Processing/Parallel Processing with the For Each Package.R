#Parallel Processing with the For each Package


###################### Parallel Process Each Decade ###################
library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(rdd)) # If you use any packages, put them in here

foreach(exponent = seq(min(House.5$year), (max(House.5$year) -10), by = 10), #Make sure the sequence is logical 
        .combine = list,
        .multicombine = TRUE)  %dopar%  
  rdd::RDestimate(dv1 ~ dv, data = subset(House.5, House.5$year %in% seq(exponent, (exponent + 10), by = 10)),  cutpoint = 50.0)

stopCluster(cl)