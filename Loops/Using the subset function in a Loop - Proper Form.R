#Using the subset function in a Loop

#this script includes the proper colunm notation

CNames1982 <- colnames(MC.3)[3:ncol(MC.3)] #Name of the columns
for(c in CNames1982){
  
  MC.loop <- subset(MC.3, MC.3[,c] == "dog")
  
}



Spill.Values <- unique(MC.2$Spillover) #Lopp wihtin the loop and the values asscated with it 

for(c in CNames1982){
  for (s in Spill.Values){
    
  
MC.loop <- subset(MC.3, MC.3[,c] == s)

  }
}