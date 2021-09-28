#Function for Creating a Affilation Matrix.

#Make sure all values are distinct before you use the function
library(dplyr)
data <- distinct(data, .keep_all = F)

#Call:
#affiliation_dyatic(data, units, affilation) 

#Function Code
affiliation_matrix <- function(data, units, affiliation){

  
  
  #*********************** Turn the Dyatic into NxKAdjency matrix *********************
  library('Matrix')
  A <- spMatrix(nrow=length(unique(data[, units])),
                ncol=length(unique(data[, affiliation])),
                i = as.numeric(factor(data[, units])),
                j = as.numeric(factor(data[, affiliation])),
                x = rep(1, length(as.numeric(data[, units]))) )
  row.names(A) <- levels(factor(data[, units]))
  colnames(A) <- levels(factor(data[, affiliation]))
  
  #*****************Create a NxN Matrix for MCs and for Committees ******************
  
  #******************************Members x Members
  #Formula
  Arow <- tcrossprod(A) #Faster Code
  
  #Checks
  head(Arow)
  dim(Arow)
  
  #Convert into data frame
  McxMC.Party <- as.data.frame(as.matrix(Arow))
  
  
  return(McxMC.Party)
  
}



#Call Example:
affiliation_matrix(CanNames.2012.W.House.Control,"ICPSR2", "party")


#Creating Object
Test1 <- affiliation_matrix(CanNames.2012.W.House.Control,"ICPSR2", "party")

