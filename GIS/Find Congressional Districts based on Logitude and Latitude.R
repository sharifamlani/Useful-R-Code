############# Find Congressional Districts based on Longitude and Latitude ##################

#See: https://stackoverflow.com/questions/33766715/converting-coordinates-to-congressional-districts-using-dstks-api-in-r

#*********************** Set Up ************************
#Assumes working from a CSV with two columns for Lat/Long and one with an ID
# Load the required packages. 
library(rgdal)
library(sp)
library(maps)
library(tigris)
library(sf)

#*********************** Read In Shape File **********************
setwd("C:/Users/Shari/OneDrive/Data/Shape Files/Congressional Districts/districts096/districtShapes")

shape <- read_sf(dsn = ".", layer = "districts096")

districts <- as(shape, 'Spatial')

#********************** Find Congressional Districts *******************
#Upload data to a dataframe
geos<-na.omit(data.frame(bonica.cid = Bonica.1980$bonica.cid, cycle =Bonica.1980$cycle, longitude = Bonica.1980$longitude, latitude = Bonica.1980$latitude))

#converts the Latitude and Lontitude columns into a Geospatial Data Frame
coordinates(geos) <- c("longitude", "latitude")

#Sets Proj4Strings of geos to that of districts
proj4string(geos)<-proj4string(districts)

#determines which districts contain geos
inside.district <- !is.na(over(geos, as(districts, "SpatialPolygons")))

#Checks the fraction of geos inside a district
mean(inside.district)

#*********************** Assign Congressional Districts ****************
#Takes the values for District and adds them to your geos data
geos$District <- over(geos,districts)$DISTRICT

#Takes the values for State and adds them to your geos data
geos$State <- over(geos,districts)$STATENAME

#*********************** Data Management ***********************

#********************* Fix States 
State_Dictonary <- data.frame(state.abb, state.name)

geos.2 <- merge(x = geos, y = State_Dictonary, by.x = c("State"), by.y = c("state.name"))

#********************* Fix Districts
#Paste Zero infront of Single Districts
geos.2$district_code <- as.character(geos.2$District)
geos.2$district_code[geos.2$district_code == "0"] <- "01"
geos.2$district_code[geos.2$district_code == "1"] <- "01"
geos.2$district_code[geos.2$district_code == "2"] <- "02"
geos.2$district_code[geos.2$district_code == "3"] <- "03"
geos.2$district_code[geos.2$district_code == "4"] <- "04"
geos.2$district_code[geos.2$district_code == "5"] <- "05"
geos.2$district_code[geos.2$district_code == "6"] <- "06"
geos.2$district_code[geos.2$district_code == "7"] <- "07"
geos.2$district_code[geos.2$district_code == "8"] <- "08"
geos.2$district_code[geos.2$district_code == "9"] <- "09"

geos.2$Congressional_District <- paste(geos.2$state.abb, geos.2$district_code, sep = "")

#********************** Make Final Dataset *****************
geos.final <- data.frame(bonica.cid = geos.2$bonica.cid, cycle= geos.2$cycle, Congressional_District = geos.2$Congressional_District)
head(geos.final)