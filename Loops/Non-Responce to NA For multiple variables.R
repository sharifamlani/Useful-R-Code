#Chnaging Non-Responces to Missing 
#For multiple Variables 
#In a loop

##################### Changing Non-Responce to Missing ###############

Traitnames<- c("V161159", "V161160","V161161", "V161162",
               "V161163",
               "V161164", 
               "V161165", 
               "V161166", 
               "V161167", 
               "V161168", 
               "V161169", 
               "V161170")

for(i in Traitnames) {
  ANES2016.Traits[i][ANES2016.Traits[i] == "-9. Refused"] <- NA
  ANES2016.Traits[i][ANES2016.Traits[i] == "-8. Don't know (FTF only)"] <- NA
  print(table(ANES2016.Traits[i]))
}

