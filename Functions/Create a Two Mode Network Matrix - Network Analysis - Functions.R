# Create a Two mode network Matrix

#Call: 
#two_mode_matrix(Data, "from", "to")

  #Data = Data
  #From = Name of from column
  #to = Name of recieving column

################### Function Code ############

two_mode_matrix <- function(data, from, to){
  

#****************** Create Dyatic Data Frame *************  
House.Party.Pure <- data.frame(data[, from], data[, to])
House.Party.1 <- House.Party.Pure

#****************Make sure all values are Unique
library(dplyr)
Money.Network.2 <- distinct(Money.Network.1, .keep_all = F)  
  
#************* Matrix Algebra ***********
library('Matrix')
B <- spMatrix(nrow=length(unique(data[, from])),
              ncol=length(unique(data[, to])),
              i = as.numeric(factor(data[, from])),
              j = as.numeric(factor(data[, to])),
              x = rep(1, length(as.numeric(data[, from]))) )
row.names(B) <- levels(factor(data[, from]))
colnames(B) <- levels(factor(data[, to]))
B

#************ Create Data Frame **************
CommXMC <- as.data.frame(as.matrix(B))

return(CommXMC)
}


#Function Example
two_mode_matrix(Money.2, "CMTE_ID", "ICPSR2")


