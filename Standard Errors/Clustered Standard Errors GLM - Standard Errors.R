y <- sample(c(0,1), 100, replace = T)
gender <- sample(c(0,1), 100, replace = T)
m <- sample((3:1300), 100, replace = T)
cluster <- sample(c("A", "B", "C"), 100, replace = T)
  
df <- data.frame(y, gender, m, cluster)


Results <- glm(y ~ gender*m, data = df, family = "binomial")
summary(Results)


exp(cbind(OR = coef(Results), confint(Results)))

############### Clustered Standard Errors #############
library(rms)

fit=lrm(y ~ gender*m, x=T, y=T, data=df)

fit

robcov(fit, cluster=df$cluster)

bootcov(fit,cluster=df$cluster)

hist(m)
hist(scale(m))
