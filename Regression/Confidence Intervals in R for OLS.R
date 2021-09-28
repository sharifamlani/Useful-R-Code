#Confidence Intervals in R for OLS


Model.1 <- lm(Norm.Num.Bill.CS ~ Norm.Num.Com.Donors + ideo.diff + Sameparty + Samestate, data = Money.Cosponsor.1)
as.data.frame(confint(Model.1, 'Norm.Num.Com.Donors', level=0.95))
summary(Model.1)