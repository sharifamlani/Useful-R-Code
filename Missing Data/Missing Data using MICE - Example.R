#Sharif Amlani
#Fall Quarter 2019
#3.6.1

############################ Code Summary ########################
#This code recodes group names.

############################# Preamble ##############################
rm(list=ls(all=TRUE))

options(stringsAsFactors = FALSE)
options(scipen = 3)
options(max.print = 5000)

############################# Create Data ##############################

Party <- c("Democrat", "Democrat", "Democrat", "Democrat", "Democrat",
           "Republican", "Republican", "Republican", "Republican", "Republican")

Expert_Survey <- c(7,NA,5,6,5,
                   NA,2,NA,2,NA)

DW_Nominate <- c(-.4,-.125,-0.2,-.47,-.158,
                 .02, .12, .21, .16, .05)

CCES <- c(6,4,6,6,5,
          3,1,2,3,1)

Ideology <- data.frame(Party, Expert_Survey, DW_Nominate, CCES)

########################### Data Management ####################
#Reverse The scales
Ideology$Expert_Survey <- (Ideology$Expert_Survey*-1) +8
Ideology$CCES <- (Ideology$CCES*-1) +8

################# Missing Data Techniques ###################
Ideology

###################### MICE ###################

#https://cran.r-project.org/web/packages/mice/mice.pdf
#From pages 66 - 72
#Differnet Methods for imputation: 68 to 69

library(mice)
imp <- mice(Ideology, meth=c('norm.boot'));complete(imp)

# first completed data matrix
complete(imp)


#################### KNN ###########################

