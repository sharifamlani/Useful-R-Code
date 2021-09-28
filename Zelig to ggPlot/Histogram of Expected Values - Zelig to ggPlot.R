#Zelig - Histogram From Expected values from GGplot!

#Zelig to ggPlot

# ~~~~~~~~~~~ fitted values ~~~~~~~~~~~~~~~~~~~ #

#Extract Zelig bootstraped values from Zelig to a Data Frame
Eff.1 <- zelig_qi_to_df(s.fuelefficiency.ind.glm)
Renew.1 <- zelig_qi_to_df(s.renewable.ind.glm)

#make sure the data frame is a "data.frame"
class(Eff.1)

t.test(Eff.1$expected_value,
       Renew.1$expected_value,  alternative="greater" )


head(Eff.1)

head(Renew.1)

#Create group Identifiers for each question
Eff.1$Group <- "Fuel"

Renew.1$Group <- "Renewable"


#Becasue the column names are the same ...
colnames(Eff.1) == colnames(Renew.1)

#...You can Rbind them together into a single data frame
Eff.Renew <- rbind(Eff.1, Renew.1)

#Then plot the the density plot for a single covarate profile 
ggplot(Eff.Renew, aes(expected_value, fill = Group)) + geom_density(alpha = 0.2)
