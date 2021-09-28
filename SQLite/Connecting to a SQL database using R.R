#Sharif Amlani
#R 4.0.2
#Summer 2020

######################## Code Summary ##################
#How to connect to and SQL databace using RSQLite

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################

################## Create Connection to Database ##################
library(RSQLite)

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/Data/DIME/DIME Contribution SQL Database")

#Establish Connection
conn <- dbConnect(RSQLite::SQLite(), "dime.sqlite3")

#Get a list of all the tables in the SQL Data Base
alltables = dbListTables(conn)

###################### Pull Data ########################
House_Contributions.1 <- dbGetQuery(conn, "SELECT * FROM contribDB WHERE seat = 'federal:house' ")

####################### Disconnect ################
#call dbDisconnect() when finished working with a connection 
dbDisconnect()
