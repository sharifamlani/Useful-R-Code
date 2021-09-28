#For Loop, ifelse statement


######################## Presidental Election ######################
Money.Cosponsor.1$Pres.year <- 0

for(i in seq(from = 1972, to = 2012, by = 4)){
  
  Money.Cosponsor.1$Pres.year <- ifelse(Money.Cosponsor.1$year == i, 1, Money.Cosponsor.1$Pres.year)
}

table(Money.Cosponsor.1$year, Money.Cosponsor.1$Pres.year)
