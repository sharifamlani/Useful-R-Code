#Extracting Objects from lm/glm
Model.1 <- lm(Norm.Num.Bill.CS ~ Norm.Num.Com.Donors + ideo.diff + Sameparty + Samestate, data = M.Loop)
summary(Model.1)

Model.Output <- as.data.frame(matrix(ncol=1, nrow=1))
Model.Output$Coeff <- Model.1$coefficients[2]
Model.Output$sd <- coef(summary(Model.1))[, "Std. Error"] [2]
Model.Output$p <- coef(summary(Model.1))[, "Pr(>|t|)"] [2]