#Sharif Amlani

#*********** Function Purpose***************

#Function for Counting up the Missing data in each column of your data


#************* Function Call **************
Count_Missing <- function(data){
  
  Missing <- NULL
  Missing.Final <- NULL
  for(i in colnames(data)){
    
    Missing$Column <- i
    Missing$Count <- sum(is.na(data[,i]))
    
    Missing.Final <- rbind(Missing.Final, Missing)
    
  }
  return(Missing.Final)
  
}

#************* Example ****************
Count_Missing(Money.Cosponsor.1)

  # Money.Cosponsor.1 = The name of the data frame.