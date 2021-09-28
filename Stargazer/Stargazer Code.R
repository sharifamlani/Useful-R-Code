#Stargazer Code

setwd("C:/Users/Shari/OneDrive/University of California, Davis/Fourth Year/Dissertation Prospectus/Out of State Donors/Tables/In-District Donor Ideology Validity")

table7 <-stargazer(list(m8, m7), type = "html",
                   title    = "Table 5: Determinants of Turnout Inequality (R)",
                   covariate.labels = c(
                     "Voting Polarization",
                     "Redistribution Salience",
                     "PR", 
                     "Concurrent Elections", 
                     "Compusory Voting", 
                     "Polity",
                     "Infant Mortality", 
                     "Gini (Gross)",
                     "Homicide Rate", 
                     "Ethnic Fractionalization",
                     "Constant"),
                   dep.var.caption = c("Dependent Variable:"),
                   dep.var.labels   = c("[1]","[7]"), #Write the DV Here
                   omit.stat = c("f","rsq"),
                   out = "Table Output.html")