#Predicted values from clustered Standard error model

library(sandwich)
library(lmtest)
library(multiwayvcov)

#Create Lagged Variable 
workingdata_2 <- workingdata  %>% arrange(country, start_yr) %>% group_by(country)  %>% mutate(amend_m1 = lag(amend))   

basicmodel=glm(amend ~ as.factor(vetos) + amend_m1, family=binomial(link="logit"), data=workingdata_2)
summary(basicmodel)

cluster.vcov(basicmodel, cluster = workingdata_2$country)

newdat <- data.frame(amend_m1 = c(rep(0,4), rep(1,4)),
                     vetos = c(-1,0,1,2))

cbind(newdat,predict(basicmodel,
                     vcov= cluster.vcov(basicmodel, cluster = workingdata_2$country),
                     newdat, 
                     type = "response",
                     se.fit = T))
