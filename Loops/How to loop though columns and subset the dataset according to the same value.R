
#How to loop though columns and subset the dataset according to the same value

#https://stackoverflow.com/questions/56944190/how-to-loop-though-columns-and-subset-the-dataset-according-to-the-same-value

#Answer 1
for(j in race){
  for (i in race){
    
    df_1 <- CMPS.2[which(CMPS.2[[i]] == 1),]
    df_2 <- CMPS.2[which(CMPS.2[[j]] == 1),]
    print(paste(i, j, sep = " "))
    
    print(t.test(df_1$DescripRep.Natural, df_2$DescripRep.Natural) )
    
  }
}

#Answer 2
for(j in race){
  for (i in race){
    
    df_1 <- subset(CMPS.2, CMPS.2[,i] == 1)
    df_2 <- subset(CMPS.2, CMPS.2[,j] == 1)
    print(paste(i, j, sep = " "))
    print(t.test(df_1$DescripRep.Natural, df_2$DescripRep.Natural) )
    
    
  }
}

#Answer 3
for(j in race){
  for (i in race){
    
    df_1 <- subset(CMPS.2, get(i) == 1)
    df_2 <- subset(CMPS.2, get(j)  == 1)
    print(paste(i, j, sep = " "))
    print(t.test(df_1$DescripRep.Natural, df_2$DescripRep.Natural) )
    
    
  }
}