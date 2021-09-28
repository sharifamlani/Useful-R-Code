#textbook loop recode

#This code recodes and sets the renames the variables 
###################### Approval #######################
Name_Approval_Variables <-  c("Approval_Trump", "Approval_Congress", "Approval_SCOTUS", "Approval_Governor", "Approval_LegName", "Approval_HouseMember", "Approval_Senator1", "Approval_Senator2")
Approval_Variable <- c("CC20_320a", "CC20_320b", "CC20_320c", "CC20_320d", "CC20_320e", "CC20_320f", "CC20_320g", "CC20_320h")
Approval_Name_DF <- data.frame(Approval_Variable, Name_Approval_Variables)

#i <- "CC20_320a"
for(i in unique(Approval_Name_DF$Approval_Variable)){
  
  CCES.1[[i]][CCES.1[[i]] %in% c("Not sure", "skipped", "not asked")] <- NA
  CCES.1[[subset(Approval_Name_DF, Approval_Variable == i)$Name_Approval_Variables]] <- factor(CCES.1[[i]], levels = c("Strongly disapprove", "Somewhat disapprove", "Somewhat approve", "Strongly approve"))
  
}

#Check
for(i in unique(Approval_Name_DF$Approval_Variable)){
  print(table(CCES.1[[subset(Approval_Name_DF, Approval_Variable == i)$Name_Approval_Variables]], CCES.1[[i]]))
  
}

table(CCES.1$Approval_Congress)