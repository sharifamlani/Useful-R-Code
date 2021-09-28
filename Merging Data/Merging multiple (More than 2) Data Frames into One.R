#Merging multiple (More than 2) Data Frames into One

library(plyr)

df3 <- data.frame(df3)
df4 <- data.frame(df4)

df1$rn <- rownames(df1)
df2$rn <- rownames(df2)
df3$rn <- rownames(df3)
df4$rn <- rownames(df4)

#If by only 1 name
df <- join_all(list(df1,df2,df3,df4), by = 'rn', type = 'full')

#If by more than 1 rowname
df <- join_all(list(df1,df2,df3,df4), by = c('rn', "rn2"), type = 'full')