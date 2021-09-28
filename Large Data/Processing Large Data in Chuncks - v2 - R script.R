#Sharif Amlani
#R 3.6.2
#Summer 2020

######################## Code Summary ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)

######################### Tutorial #####################


#Processing Large Data Files in chucks
#Tutoral Available here: https://www.youtube.com/watch?v=Z5rMrI1e4kM

######################## Upload Data ##################

#Set Working Directory
setwd("C:/Users/Shari/Downloads/csv_pus")

####################### Read In By 100K rows #############
#Set File Name
File_Name <- paste("psam_pusa.csv", sep = "")

index <- 0
chunkSize <- 100000

################# Read in First Dime File #####################
con <- file(description = File_Name, open = "r")
dataChunk <- read.table(con, nrows = chunkSize, header = T, fill = T, sep = ",")
actualColumnNames <- names(dataChunk)

################ Repeat ##################
repeat{
  ############## Set Up ##################
  index <- index + 1
  print(paste('Processing rows:', index * chunkSize))
  
  ############## Action ##################
  
  Census_1 <- rbind(Census_1, subset(dataChunk, CIT %in% c(4,5)))
  
  ############## Final Break Chunck ###################
  if(nrow(dataChunk) != chunkSize){
    print("Processed all files")
    break
  }
  
  ############### Read In Next Chunck of Data ###################
  dataChunk <- read.table(con, nrows = chunkSize, skip = 0, header = F, fill = T, sep = ",",
                          col.names = actualColumnNames)
  
  
  
  #if (index > 2) break  
}

close(con)

head(Census_1)

