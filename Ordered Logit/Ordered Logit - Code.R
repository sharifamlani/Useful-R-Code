################# Ordered Logit ###########
library(MASS)

## fit ordered logit model and store results 'm'
Ologit.1 <- polr(Contrib.Public ~ log(Donor.Amount), data = CCES.2, Hess=TRUE)

## view a summary of the model
summary(Ologit.1)

#Predicted Probabilities
newdat <- data.frame(
  Donor.Amount = seq(1, 5000, 1))

Prediction.1 <- cbind(newdat, predict(Ologit.1, newdat, type = "probs"))

##show first few rows
head(Prediction.1)

lnewdat <- melt(Prediction.1, id.vars = c("Donor.Amount"),
                variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat)

#Graph it
library(ggplot2)
ggplot(lnewdat, aes(x = Donor.Amount, y = Probability, colour = Level)) +
  geom_line() #+ facet_wrap(. ~ Level)