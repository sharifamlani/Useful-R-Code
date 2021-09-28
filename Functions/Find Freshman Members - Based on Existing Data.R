# Create a Dataframe from scratch

IDs.Final <- NULL
for (i in seq(10, 110, by = 10)){
print(i)
IDs <- seq(1,50, by=1) + i
  
IDs.Final <- c(IDs.Final, IDs)
}

IDs.Final

year <- sort(rep(seq(1990, 2010, by = 2), 50))


Test <-data.frame(IDs.Final, year)

head(Test)

############### Finding the first appearance of every number in each year #######

Test.1990 <- subset(Test, year == 1990)
Test.1992 <- subset(Test, year == 1992)
Test.1994 <- subset(Test, year == 1994)

unique(Test.1990$IDs.Final) 
unique(Test.1992$IDs.Final)


Test.1992$Freshman <- 0
for (i in setdiff(unique(Test.1992$IDs.Final), unique(Test.1990$IDs.Final))){
  
  Test.1992$Freshman <- ifelse(Test.1992$IDs.Final == i, 1, Test.1992$Freshman)
}

Test.1992


setdiff(unique(Loop_t1$IDs.Final), unique(Loop_t$IDs.Final))

#*********** Running For LOOP
Loop_Final <- NULL
for(i in seq(min(Test$year), max(Test$year)-2, by = 2)){
  Loop_t <- subset(Test, year == i)
  Loop_t1 <- subset(Test, year == (i + 2))
  
  Loop_t1$Freshman <- 0
  for (k in setdiff(unique(Loop_t1$IDs.Final), unique(Loop_t$IDs.Final))){
    
    Loop_t1$Freshman <- ifelse(Loop_t1$IDs.Final == k, 1, Loop_t1$Freshman)
  
  }
  
  Loop_Final <- rbind(Loop_Final, Loop_t1)
}

table(Loop_Final$Freshman)


#View(Loop_Final)

#*********** FUnction

First_Freshman <- function(data){
  Loop_Final <- NULL
  for(i in seq(min(data$year), max(data$year)-2, by = 2)){
    Loop_t <- subset(data, year == i)
    Loop_t1 <- subset(data, year == (i + 2))
    
    Loop_t1$Freshman <- 0
    for (k in setdiff(unique(Loop_t1$IDs.Final), unique(Loop_t$IDs.Final))){
      
      Loop_t1$Freshman <- ifelse(Loop_t1$IDs.Final == k, 1, Loop_t1$Freshman)
      
    }
    
    Loop_Final <- rbind(Loop_Final, Loop_t1)
  }
  
  return(Loop_Final)

}

Test.Final <- First_Freshman(Test)
table(Test.Final$Freshman)
