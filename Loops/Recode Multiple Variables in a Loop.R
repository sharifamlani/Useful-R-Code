#Recode Multiple Variables in a Loop

################# Recode Numeric Varaibles 1-5: Bad to Good ##############
Traitnames<- c("V161159", "V161160","V161161", "V161162",
               "V161163",
               "V161164", 
               "V161165", 
               "V161166", 
               "V161167", 
               "V161168", 
               "V161169", 
               "V161170")

#Formula
7*-1
-7 + 8

(7*-1) +8


ANES2016.Traits.3a <- ANES2016.Traits.2

for(i in Traitnames) {
  ANES2016.Traits.3a[i] <- ((ANES2016.Traits.2[i] * -1) + 8)
  print(table(ANES2016.Traits.3a[i]))
}

#Step 1
table(ANES2016.Traits$V161159)

#Step 2
table(ANES2016.Traits.2$V161159)

#Step 3
table(ANES2016.Traits.3a$V161159)

#Perfect! All Check Out!!