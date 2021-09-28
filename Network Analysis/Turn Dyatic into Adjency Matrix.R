
# Turn the Dyatic into NxKAdjency matrix

#Prelude - Create dyatic Network
Money.Network.Pure <- with(Money.2, data.frame(CMTE_ID, ICPSR2))
Money.Network.1 <- Money.Network.Pure

#Turn Dyatic network into Matrix
library('Matrix')
A <- spMatrix(nrow=length(unique(Money.Network.1$ICPSR2)),
              ncol=length(unique(Money.Network.1$CMTE_ID)),
              i = as.numeric(factor(Money.Network.1$ICPSR2)),
              j = as.numeric(factor(Money.Network.1$CMTE_ID)),
              x = rep(1, length(as.numeric(Money.Network.1$ICPSR2))) )
row.names(A) <- levels(factor(Money.Network.1$ICPSR2))
colnames(A) <- levels(factor(Money.Network.1$CMTE_ID))
A

McxComm.1 <- as.data.frame(as.matrix(A))
View(McxComm.1)

ncol(McxComm.1) #4210
nrow(McxComm.1) #450

########################## Create a NxN Matrix for MCs and for Committees ###################

#******************************Members x Members
#Formula
Arow <- A %*% t(A) #Real Formula
Arow <- tcrossprod(A) #Faster Code

#Checks
head(Arow)
dim(Arow)

#Convert into data frame
McxMC.1 <- as.data.frame(as.matrix(Arow))
View(McxMC.1)

#******************************Committees x Committees 
#Formula
Acol <- t(A) %*% A #Real Formula
Acol <- tcrossprod(t(A)) #Faster Code

#Checks
dim(Acol)
head(Acol)

#Convert into data frame
ComxCom.1 <- as.data.frame(as.matrix(Acol))

View(ComxCom.1)
