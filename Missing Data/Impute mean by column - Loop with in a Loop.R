# Loop and Loop with in a loop

# This loop subsets each country, and 
# imputes the mean of each countries columns in for missing data

Master.Data.pure <- NULL
Countries.loop <- unique(Rep.Agg.Data.Master.1$country)
Columns.loop <- colnames(Rep.Agg.Data.Master.1)

for (i in Countries.loop) {
  
  a1 <- subset(Rep.Agg.Data.Master.1, country == i)
  
  for (c in Columns.loop){
    a1[,c][is.na(a1[,c])] <- mean(a1[,c], na.rm = T)
    
  }
  
  Master.Data.pure <- rbind(Master.Data.pure, a1)
  
}


View(Master.Data.pure)

colnames(Master.Data.pure)

