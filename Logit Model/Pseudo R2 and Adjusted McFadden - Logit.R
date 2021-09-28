
# Pseudo-R2

pR2("Model Name")


# Obtain Predicted Values

yhat.logit3 <- predict("Model Name", type="response")

summary(yhat.logit3)


(mckR2."Model Name" <- var(yhat.logit3) / (var(yhat.logit3) + (pi^2/3)))


# Adjusted McFadden

L.full3 <- logLik("Model Name")
P3 <- attr(L.full3, "df")

McFadden.R2.full3 <- 1 - (L.full3 / L.base); McFadden.R2.full3
McFadden.Adj.R2.full3 <- 1 - ((L.full3 - P3) / L.base); McFadden.Adj.R2.full3


AIC(logit.field)  #We can see a large differnce betwee 1 and 2 , > than 10 thereofer 2 is "better"
AIC(logit.field2)

AIC("Model Name")

BIC(logit.field)
BIC(logit.field2)
BIC("Model Name")
