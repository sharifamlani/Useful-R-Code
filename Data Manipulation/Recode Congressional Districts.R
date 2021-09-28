################### Recode Districts ######################

#**************** Recode AL to 1 ********************
Women_1$District[Women_1$District == "AL"] <- 1 

#************** Add Zero Before the District Number if Single ***************
for(i in as.numeric(unique(sort(Women_1$District)))){
  if(i <= 9){
    Women_1$District_0[Women_1$District == i] <- paste0(0, i, sep = "")
  }
  
  if(i >= 10){
    Women_1$District_0[Women_1$District == i] <- i
  }
  
  
}

unique(Women_1$District_0)
table(Women_1$District_0, Women_1$District)

#**************** Paste Updated Congressional Districts **************
colnames(Women_1)

Women_1$CD <- paste(Women_1$State, Women_1$District_0, sep = "")