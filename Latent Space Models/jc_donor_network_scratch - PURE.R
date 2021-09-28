# Jane's notes:
# discosp is if Num.Bill.CS >0

######################## 0. Setup ###############################

#Set Working Directory
setwd("~/Documents/DSI/consultations/")

library(dplyr)
library(foreign)
library(lolog)
library(ergm)
library(latentnet)

######################## 1. Upload Data ###############################

rm(list=ls(all=TRUE))

# Options

options(stringsAsFactors = FALSE)
options(scipen = 3)


# Upload Campaign Finace and Cosponsorship Data

# load(file = "Master Data - Money in Politics--W2.rda") #4422046      31
load(file = "Money and Cosponsorship - Merged2 - Freshman True.rda") #49110    29
load(file = "Money and Cosponsorship - Merged2 - Freshman and ALL.rda") #543942     29

fresh_data = Money.Cosponsor.1_Freshman_True; rm(Money.Cosponsor.1_Freshman_True) #==Money.Cosponsor.2
all_data = Money.Cosponsor.1_Freshman; rm(Money.Cosponsor.1_Freshman)
names(fresh_data)
names(all_data)

######################## 2. Make Network  ###############################

fresh_subdata = filter(fresh_data, !is.na(Num.Bill.CS), year == 2002)
fresh_subdata_attr = fresh_subdata %>% group_by(From) %>%  select(contains(".x")) %>%
  summarize_all(first)

# Create base network
fresh_subnet = network(fresh_subdata[,c("From", "To")],
                             directed = FALSE, vertex.attr = fresh_subdata_attr)
N = network.size(fresh_subnet)
  
# Check if we needed to reorder attributes when adding to network (if all true OK):
fresh_subdata_attr$From == fresh_subnet%v%"vertex.names"

# Add edge values
fresh_subnet%e%"Num.Bill.CS" = fresh_subdata$Num.Bill.CS
fresh_subnet%e%"ideo.diff" = fresh_subdata$ideo.diff
fresh_subnet%e%"Num.Com.Donors" = fresh_subdata$Num.Com.Donors
fresh_subnet%e%"Log.Num.Com.Donors" = log(fresh_subdata$Num.Com.Donors)
fresh_subnet%e%"Num.Com.Donors_factor" = as.character(cut(fresh_subnet%e%"Num.Com.Donors",breaks = 
                                            quantile(fresh_subnet%e%"Num.Com.Donors", seq(0, 1,.25)),
                                            right = TRUE, include.lowest = TRUE))

# Add more vertex attributes?
fresh_subnet%v%"vertex.names" = fresh_subnet%v%"name.x" #<- to make name labels


#check: isSymmetric(as.sociomatrix(fresh_subnet, attrname = "ideo.diff"))

######################## 3. Model  ###############################

fresh_submodel = ergmm(fresh_subnet ~ euclidean(d = 2) + #try with one and two dimensions, got interesting differences
                    rsociality + 
                    nodematch("party.x") + nodematch("state.x") +
                    edgecov(as.sociomatrix(fresh_subnet, attrname = "ideo.diff"),
                            attrname = "ideo.diff") +
                    edgecov(as.sociomatrix(fresh_subnet, attrname = "Num.Com.Donors"), 
                            attrname = "Num.Com.Donors"),  
      response = "Num.Bill.CS",
      family = "Poisson",
      control = control.ergmm(sample.size = 5000, burnin = 50000, interval = 20)) #prob go even high

######################## 4. Look at results  ###############################
mcmc.diagnostics(fresh_submodel)

summary(fresh_submodel)

data.frame(name = fresh_subnet%v%"vertex.names", soc = fresh_submodel$mkl$sociality,
           state = fresh_subnet%v%"state.x") %>% arrange(soc)


plot(fresh_submodel, vertex.col = "party.x", plot.means = F, plot.vars = F,
     vertex.cex = (fresh_submodel$mkl$sociality - min(fresh_submodel$mkl$sociality) + 1),
     #edge.col = 3 + as.numeric(as.factor(fresh_subnet%e%"Num.Com.Donors_factor")),
     labels = TRUE)

# residuals ----
hist((as.sociomatrix(fresh_subnet, attrname = "Num.Bill.CS") - predict(fresh_submodel))[lower.tri(matrix(0,N,N))])

#standardized residuals
hist((as.sociomatrix(fresh_subnet, attrname = "Num.Bill.CS") - predict(fresh_submodel))[lower.tri(matrix(0,N,N))]/ (predict(fresh_submodel)[lower.tri(matrix(0,N,N))]))

#observed vs. predicted vlue
plot(x = (as.sociomatrix(fresh_subnet, attrname = "Num.Bill.CS")[lower.tri(matrix(0,N,N))]), 
(predict(fresh_submodel)[lower.tri(matrix(0,N,N))]) , xlab = "observed", ylab = "predicted")

#correlation between latent distance and ideological distance?
# I think these are lined up correctly?
cor(as.sociomatrix(fresh_subnet, attrname = "ideo.diff")[lower.tri(matrix(0,N,N), diag = F)], 
    as.matrix(dist(fresh_submodel$mkl$Z))[lower.tri(matrix(0,N,N), diag = F)], use = "pairwise")

