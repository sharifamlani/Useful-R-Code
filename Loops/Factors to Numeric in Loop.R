#Changing Factors to Number
#For Multiple Variables
#In an Loop

################## Changing Factors Into Numbers #################
ANES2016.Traits.2 <- ANES2016.Traits
nrow(ANES2016.Traits.2)

Traitnames<- c("V161159", "V161160","V161161", "V161162",
               "V161163",
               "V161164", 
               "V161165", 
               "V161166", 
               "V161167", 
               "V161168", 
               "V161169", 
               "V161170")
ANES2016.Traits.2[Traitnames] <- sapply(ANES2016.Traits.2[Traitnames],as.numeric)
sapply(ANES2016.Traits.2, class)

table(ANES2016.Traits$V161159)
table(ANES2016.Traits.2$V161159)

#Scaled from 3 - 7: Good to Bad
