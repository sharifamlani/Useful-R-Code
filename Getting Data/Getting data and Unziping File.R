#******************** LES Data **********************
setwd("C:/Users/Shari/OneDrive/Data/Legislative Effectiveness Data")

#Download Data
library(foreign); library(readstata13)
download.file(url = "https://thelawmakers.org/wp-content/uploads/2019/07/CELHouse93to115Reduced.zip", #Note you may have to update the Congress range
              destfile = "C:/Users/Shari/OneDrive/Data/Legislative Effectiveness Data/LES_Data.zip")

unzip(zipfile = "C:/Users/Shari/OneDrive/Data/Legislative Effectiveness Data/LES_Data.zip")
file.remove("LES_Data.zip")

#Upload Data
LES.Pure <- read.dta13("CELHouse93to115Reduced.dta"); LES.1 <- LES.Pure
