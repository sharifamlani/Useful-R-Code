#How to convert zelig object to a data frame

library(zeligverse)

# ~~~~~~~~~~~ fitted values ~~~~~~~~~~~~~~~~~~~ #
Eff.1 <- zelig_qi_to_df(s.fuelefficiency.ind.glm)
Renew.1 <- zelig_qi_to_df(s.renewable.ind.glm)





