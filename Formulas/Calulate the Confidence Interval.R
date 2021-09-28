
#Calulate the Confidence Interval


# Model.1 = Regresstion Model
# 
# Norm.Num.Com.Donors = Variable of INterest 

x <- as.data.frame(confint(Model.1, 'Norm.Num.Com.Donors', level=0.95))

Model.Output$lower <- x$`2.5 %`
Model.Output$upper <- x$`97.5 %`