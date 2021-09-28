
####################### Create Control Data Frame #################

CanNames.2012.W.House.Control <- CanNames.2012.W.House

####################### CONTROLS: SAME dwnom1 #####################

colnames(CanNames.2012.W.House.Control)

#***************************** Create Dyad *******************
table(CanNames.2012.W.House.Control$comtype)

House.dwnom1.Pure <- with(CanNames.2012.W.House.Control, data.frame(dwnom1, ICPSR2))
House.dwnom1.1 <- House.dwnom1.Pure

head(House.dwnom1.1)

#*********************** Turn the Dyatic into NxKAdjency matrix *********************
library('Matrix')
A <- spMatrix(nrow=length(unique(House.dwnom1.1$ICPSR2)),
              ncol=length(unique(House.dwnom1.1$dwnom1)),
              i = as.numeric(factor(House.dwnom1.1$ICPSR2)),
              j = as.numeric(factor(House.dwnom1.1$dwnom1)),
              x = rep(1, length(as.numeric(House.dwnom1.1$ICPSR2))) )
row.names(A) <- levels(factor(House.dwnom1.1$ICPSR2))
colnames(A) <- levels(factor(House.dwnom1.1$dwnom1))
A


Mcxdwnom1.1 <- as.data.frame(as.matrix(A))
#View(Mcxdwnom1.1)

ncol(Mcxdwnom1.1) #4210
nrow(Mcxdwnom1.1) #450
dim(Mcxdwnom1.1) #450



#*******************Create a NxN Matrix for MCs and for Committees ******************

#******************************Members x Members
#Formula
Arow <- A %*% t(A) #Real Formula
Arow <- tcrossprod(A) #Faster Code

#Checks
head(Arow)
dim(Arow)

#Convert into data frame
McxMC.dwnom1 <- as.data.frame(as.matrix(Arow))
View(McxMC.dwnom1)


#********************* MC x MC -- > Into Dyatic Matix ***************
head(McxMC.dwnom1)
dim(McxMC.dwnom1)
View(McxMC.dwnom1)

McxMC.dwnom1$From <- rownames(McxMC.dwnom1)

library(reshape2)
McxMC.dwnom1.dyatic <- melt(McxMC.dwnom1, id.vars = c("From"),
                           variable.name = "To", 
                           value.name = "Samedwnom1")

table(McxMC.dwnom1.dyatic$Samedwnom1)

View(McxMC.dwnom1.dyatic)
