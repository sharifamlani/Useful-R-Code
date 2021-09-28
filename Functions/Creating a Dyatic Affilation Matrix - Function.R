#Function for Creating a Dyatic Affilation Matrix.

#Make sure all vlaues are distince beore you use the function
library(dplyr)
data <- distinct(data, .keep_all = F)

#Call:
#affiliation_dyatic(data, units, affilation) 

#Function Code
affiliation_dyatic <- function(data, units, affiliation){


#*********************** Turn the Dyatic into NxKAdjency matrix *********************
library('Matrix')
A <- spMatrix(nrow=length(unique(data[, units])),
              ncol=length(unique(data[, affiliation])),
              i = as.numeric(factor(data[, units])),
              j = as.numeric(factor(data[, affiliation])),
              x = rep(1, length(as.numeric(data[, units]))) )
row.names(A) <- levels(factor(data[, units]))
colnames(A) <- levels(factor(data[, affiliation]))

#Formula
Arow <- tcrossprod(A) #Faster Code

#Convert into data frame
McxMC.attribute <- as.data.frame(as.matrix(Arow))


McxMC.attribute$From <- rownames(McxMC.attribute)

library(reshape2)

McxMC.attribute.dyatic <- melt(McxMC.attribute, id.vars = c("From"),
                           variable.name = "To", 
                           value.name = paste("Same", affiliation, sep = ""))

return(McxMC.attribute.dyatic)

}



#Call Example:
#affiliation_dyatic(CanNames.2012.W.House.Control,"ICPSR2", "party")


#Creating Object
Test1 <- affiliation_dyatic(CanNames.2012.W.House.Control,"ICPSR2", "state")

head(Test1)
