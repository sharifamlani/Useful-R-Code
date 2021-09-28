set.seed(1999)

c <- sample(c("A","B", "C", "D"), size = 100, replace = T)
w <- sample(1:100, size = 100, replace = T)
x <- sample(1:100, size = 100, replace = T)
y <- sample(1:100, size = 100, replace = T)

df <- data.frame(x,y,w,c)

Results <- lm(y ~ x*w, data = df)

library(sjPlot)
library(ggplot2)

plot_model(Results, type = "int", legend.title = "GII", mdrt.values = c("minmax")) + 
  labs(title="Figure 2: Plotting the interaction term", x = "Gender", y= "Probability of Naturalization")

plot_model(Results, type = "pred", terms = c("x","w"), mdrt.values = c("minmax"), legend.title = "GII") + 
  labs(title="Figure 2: Plotting the interaction term", x = "Gender", y= "Probability of Naturalization")


plot_model(Results, type = "int", legend.title = "GII",vcov.fun = "CL", vcov.type = "HC1", vcov.args = list(cluster = df$c)) + 
  labs(title="Figure 2: Plotting the interaction term", x = "Gender", y= "Probability of Naturalization")


tab_model(Results, show.se = TRUE)

tab_model(Results, vcov.fun = "CL", vcov.type = "HC1", show.se = TRUE)

tab_model(Results, vcov.fun = "CL", vcov.type = "HC1", vcov.args = list(cluster = df$x), show.se = TRUE)
