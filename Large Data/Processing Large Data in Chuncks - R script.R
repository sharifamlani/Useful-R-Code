
#Processing Large Data Files in chucks
#Tutoral Available here: https://www.youtube.com/watch?v=Z5rMrI1e4kM

################### Process 2016 Data Rows #################

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/Data/DIME/2016 Contributors")

file.info("contribDB_2016.csv")$size

file.info("contribDB_2016.csv")$size /2^30

readLines("contribDB_2016.csv", n = 3)

Dime.2016.File <- "contribDB_2016.csv"
index <- 0
chunkSize <- 100000
con <- file(description = Dime.2016.File, open = "r")
dataChunk <- read.table(con, nrows = chunkSize, header = T, fill = T, sep = ",")
actualColumnNames <- names(dataChunk)
repeat{
  index <- index + 1
  print(paste('Processing rows:', index * chunkSize))
  
  
  
  if(nrow(dataChunk) != chunkSize){
    print("Processed all files")
    break
  }
  dataChunk <- read.table(con, nrows = chunkSize, skip = 0, header = F, fill = T, sep = ",",
                          col.names = actualColumnNames)
  
  
  
  break
  
}
close(con)