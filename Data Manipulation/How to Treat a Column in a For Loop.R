# How to Treat a Column in a For Loop

#NOTE: that, in a loop, column names are refered to as: dataframe[i].
#Example: 
          #Data set = ANES2016.Traits
          #Column names = ANES2016.Traits[i]


##################### FOR LOOP For Droping Undesriable re

# (1)Put the Column names in a Vector
Traitnames<- c("V161159", "V161160","V161161", "V161162",
               "V161163",
               "V161164", 
               "V161165", 
               "V161166", 
               "V161167", 
               "V161168", 
               "V161169", 
               "V161170")



#If you are subsetting in a data frame then be sure to create a data from
Test <- data.frame()

#Example 1: Subsetting values in a data frame 
for(i in Traitnames) {
  Test <- subset(ANES2016.Traits, ANES2016.Traits[i] != "-9. Refused" & 
                   ANES2016.Traits[i] !="-8. Don't know (FTF only)" )
  
}

View (Test)


#Example 2: Chnaging the names of a varaible in a data frame (loop) 

for(i in Traitnames) {
  ANES2016.Traits[i][ANES2016.Traits[i] == "-9. Refused"] <- NA
  ANES2016.Traits[i][ANES2016.Traits[i] == "-8. Don't know (FTF only)"] <- NA
  print(table(ANES2016.Traits[i]))
}
