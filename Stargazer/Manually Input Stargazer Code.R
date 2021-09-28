library(stargazer)

# coefficients data
d_lm <- data.frame(var = letters[1:4],
                   est = runif(4),
                   sd = runif(4),
                   t = runif(4),
                   p = runif(4))

# fake data
d <- data.frame(y = runif(30),
                a = runif(30),
                b = runif(30),
                c = runif(30),
                d = runif(30))

# fake regression
lm <- lm(y ~ a + b + c + d -1, d)

stargazer(lm,
          coef = list(d_lm$est),
          se = list(d_lm$sd),
          t = list(d_lm$t), # if not supplied stargazer will calculate t values for you
          p = list(d_lm$p), # if not supplied stargazer will calculate p values for you
          type = "text")
