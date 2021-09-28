#Different Types of Network Matrixes

#Prelude
########################## Create Network ###################
colnames(Money.2)

table(Money.2$ICPSR2)

Money.Network.Pure <- with(Money.2, data.frame(CMTE_ID, ICPSR2))
Money.Network.1 <- Money.Network.Pure


######################### Erase Douplicated Rows ###############
table(duplicated(Money.Network.1))

library(dplyr)
Money.Network.2 <- distinct(Money.Network.1, .keep_all = F)



#Different Matrixs
##################### *** 2 Mode Network, N x K *** ################

library('Matrix')
B <- spMatrix(nrow=length(unique(Money.Network.2$CMTE_ID)),
              ncol=length(unique(Money.Network.2$ICPSR2)),
              i = as.numeric(factor(Money.Network.2$CMTE_ID)),
              j = as.numeric(factor(Money.Network.2$ICPSR2)),
              x = rep(1, length(as.numeric(Money.Network.2$CMTE_ID))) )
row.names(B) <- levels(factor(Money.Network.2$CMTE_ID))
colnames(B) <- levels(factor(Money.Network.2$ICPSR2))
B


CommXMC <- as.data.frame(as.matrix(B))

#write.csv(CommXMC, "Committee x MC - Number of MCs in Common.csv")




######################### Turn the Dyatic into NxKAdjency matrix ##########################
library('Matrix')
A <- spMatrix(nrow=length(unique(Money.Network.2$ICPSR2)),
              ncol=length(unique(Money.Network.2$CMTE_ID)),
              i = as.numeric(factor(Money.Network.2$ICPSR2)),
              j = as.numeric(factor(Money.Network.2$CMTE_ID)),
              x = rep(1, length(as.numeric(Money.Network.2$ICPSR2))) )
row.names(A) <- levels(factor(Money.Network.2$ICPSR2))
colnames(A) <- levels(factor(Money.Network.2$CMTE_ID))
A


McxComm.1 <- as.data.frame(as.matrix(A))
#View(McxComm.1)

ncol(McxComm.1) #4210
nrow(McxComm.1) #450
dim(McxComm.1) #450



########################## NxN Affilation Matrix ###################

#******************************Members x Members
#Formula
Arow <- A %*% t(A) #Real Formula
Arow <- tcrossprod(A) #Faster Code

#Checks
head(Arow)
dim(Arow)

#Convert into data frame
McxMC.1 <- as.data.frame(as.matrix(Arow))
#View(McxMC.1)

#Write a CSV of this Data
#write.csv(McxMC.1, "MC x MC - Number of Donors in Common.csv")

#******************************Committees x Committees 
#Formula
Acol <- t(A) %*% A #Real Formula
Acol <- tcrossprod(t(A)) #Faster Code

#Checks
dim(Acol)
head(Acol)

#Convert into data frame
ComxCom.1 <- as.data.frame(as.matrix(Acol))
#View(ComxCom.1)

#Write a CSV of this Data
#write.csv(ComxCom.1, "Committee x Committee - Number of MCs in Common.csv")

############################### MC x MC -- > Into Dyatic Matix ##############
#This is how you would turn it into a dyatic Matrix for use in Regression

McxMC.1$From <- rownames(McxMC.1)

library(reshape2)
McxMC.dyatic <- melt(McxMC.1, id.vars = c("From"),
                     variable.name = "To", 
                     value.name = "Num.Com.Donors")

McxMC.dyatic
View(McxMC.dyatic)