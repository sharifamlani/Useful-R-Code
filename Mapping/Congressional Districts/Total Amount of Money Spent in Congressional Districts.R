# Sharif Amlani
# Summer 2019
# R 3.5.3
#Pol 51 - Summer 2019

#This r script is motivation for learning R. 
#Dont worry about understanding the code
#Just sit back and enjoy what cool things you can do in R


######################## Upload Data ###############################

rm(list=ls(all=TRUE))

#*************************Upload Data************************************

options(stringsAsFactors = FALSE)
options(scipen = 3)


########################## MAP ##################################
#Lets map the amount of money raised in the 2018 Congressional election

#****************** Upload Data
#Set Working Directory
setwd("C:/Users/Sharif/OneDrive/University of California, Davis/Third Year/Summer/Pol 51/Working Directory")

#Read in the data
library(foreign)
load("Election Results 2018.rda")
head(House_2018)
View(House_2018)
#***************** Upload Map ******************

require(rgdal)
require(maptools)
require(rgeos)
library(ggplot2)
library(dplyr)
library("maps")
library(USAboundaries)


#Download Congressional Map
cd <- USAboundaries::us_congressional(resolution = "high")

#Change At Large COngressional Districts

cd$cd115fp[cd$cd115fp == "00"] <- "01"


cd$ID <- paste(cd$state_abbr, cd$cd115fp)


#*************** Merge Graph and the Data *****************

#Add a zero before the single digets 
House_2018$cd_new <- House_2018$cd
for(i in seq(1, 9, by=1)){
  print(paste0(0, i))
  
  House_2018$cd_new <- ifelse(House_2018$cd == i, paste0(0, i), House_2018$cd_new)
  
}
table(House_2018$cd_new)

House_2018$ID <- paste(House_2018$state, House_2018$cd_new)

#Merge the map data
cd_map <- merge(x = cd, y= House_2018, all.x = T, all.y = T)

#*************** Add Total Money ****************

cd_map$Money_Total <- (cd_map$dexp + cd_map$rexp)

summary(cd_map$Money_Total)
View(cd_map)
#*************** Plot ******************

#Subset Names that are off the main land US
cd_main <- subset(cd_map, !(state_name %in% c("Alaska", "Virgin Islands", "Hawaii", "American Samoa", "Northern Mariana Islands", "Guam", "Puerto Rico")))

ggplot(cd_main) + geom_sf(aes(fill = Money_Total)) +
  coord_sf(crs = st_crs(cd_main), datum = NA) +
  scale_fill_gradient("Amount ($)",
                      low = "dodgerblue",
                      high = "green1", 
                      na.value = "grey86",
                      breaks=c(238235,5000000,10000000, 15000000, 20000000),
                      labels=c("200K","","10M", "", "20M")) +
  ggtitle("Total Amount of Money Spent in Congressional Districts") + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
   # text = element_blank(), 
    plot.background = element_rect(fill = "White"),
   plot.title = element_text(hjust = 0.5),
   legend.position="bottom")

  
