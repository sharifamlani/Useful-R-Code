#Read in Multiple Datasets using a loop

##################### Upload Data ###############################

rm(list=ls(all=TRUE))

#*******************************************Upload Data************************************

options(stringsAsFactors = FALSE)
options(scipen = 3)


#Set Working Directory
setwd("C:/Users/Sharif/OneDrive/University of California, Davis/Third Year/Money In Politics Network Analysis/Cosponsorship Data/House")



#Note 105 and 104 COngress in the Dataset is the same -- Ask Zeev

filenames <- c("H93.csv",
               "H94.csv",
               "H95.csv",
               "H96.csv",
               "H97.csv",
               "H98.csv",
               "H99.csv",
               "H100.csv",
               "H101.csv",
               "H102.csv",
               "H103.csv",
               "H104.csv",
               "H105.csv",
               "H106.csv",
               "H107.csv",
               "H108.csv",
               "H109.csv",
               "H110.csv"
)

for (i in filenames){
  
  Cosponsor <- read.csv(i)  
  
  i <- gsub(".csv","",i)  
  
  nam <- paste("Cosponsor", i, sep = ".")
  assign(nam, Cosponsor)
  
}