#Packages
library(stargazer)
library(lmtest)
library(sandwich)

#Set Seed
set.seed(1993)

#Create Sample Data
X <- sample(seq(1, 1000), 1000, replace = T)
Y <- sample(seq(1, 1000), 1000, replace = T)
Cluster <- sample(letters, 1000, replace = T)

df <- data.frame(X, Y, Cluster)

#Model 1 - OLS
Base <- lm(Y ~ X, data = df)

#Model 2 - Coeftest
Coeff_Test <- coeftest(Base, vcovCL, cluster = df$Cluster)

library(broom)
Coeff_Test_DF <- tidy(Coeff_Test)
Coeff_Test_DF$std.error

#Stargazer table
stargazer(list(Base, Base), type = "text",omit=c("Cluster"),
          model.names = FALSE,
          se = list(NULL, Coeff_Test_DF$std.error),
          p = list(NULL, Coeff_Test_DF$p.value),
          dep.var.caption = c("Dependent Variable:"),
          dep.var.labels   = c("VAP Turnout (%)"), #Write the DV Here
          omit.stat = c("f","rsq"))


################## Solution #######################
Key_Variables <-unique(c(tidy(B_DID.1)$term, tidy(B_DID.VS.1)$term))

library(broom)
#Coef_DF <- subset(tidy(B_DID.3), term %in% Key_Variables)
B_DID.3_DF <- tidy(B_DID.3)
B_DID.3_DF$std.error

stargazer(list(B_DID.Base, B_DID.1, B_DID.2, B_DID.3), type = "text",omit=c("state.name", "county_name"))

stargazer(list(B_DID.Base, B_DID.1, B_DID.2, B_DID.2 #This is a place holder (Coefs and Model Summary are entered - These Don't Change)
               ), type = "text",omit=c("state.name", "county_name"),
          se = list(NULL, NULL, NULL, B_DID.3_DF$std.error),
          p = list(NULL, NULL, NULL, B_DID.3_DF$p.value))

