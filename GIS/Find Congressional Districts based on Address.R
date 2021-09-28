start_time <- Sys.time()
#******************** Geocode the Locations ***************
library(tidygeocoder)

rs <- Descriptive_2[sample(nrow(Descriptive_2), 100), ]

#First try the whole address
Descriptive_2a <- tidygeocoder::geocode(rs, contributor.address.full, method = "cascade")

#The try the city, state and zip if full address returns NAs
Attempt_2 <- subset(Descriptive_2a, is.na(Descriptive_2a$lat) == T)
Attempt_2$lat <- NULL
Attempt_2$long <- NULL 
Attempt_2$geo_method <- NULL

Descriptive_2b <- tidygeocoder::geocode(Attempt_2, contributor.address.half, method = "cascade")

#Add Method
Descriptive_2a$Method <- "full"
Descriptive_2b$Method <- "half"

#Rbind together
Descriptive_2c <- rbind(subset(Descriptive_2a, !is.na(Descriptive_2a$lat) == T), Descriptive_2b)

#print(nrow(Descriptive_2) == nrow(Descriptive_2c))
#***************** Fill in Missing Lat Long ****************

Descriptive_2c$latitude2 <- ifelse(is.na(Descriptive_2c$latitude) == T, Descriptive_2c$lat, Descriptive_2c$latitude)
Descriptive_2c$longitude2 <- ifelse(is.na(Descriptive_2c$longitude) == T, Descriptive_2c$long, Descriptive_2c$longitude)

#**************** Find Congressional Districts *********************
#Assumes working from a CSV with two columns for Lat/Long and one with an ID
# Load the required packages. 
library(rgdal)
library(sp)
library(maps)
library(tigris)
library(sf)

#*********************** Read In Shape File **********************
setwd("C:/Users/Shari/OneDrive/Data/Shape Files/Congressional Districts/districts116/districtShapes")

shape <- read_sf(dsn = ".", layer = "districts116")

districts <- as(shape, 'Spatial')


#********************** Find Congressional Districts *******************
#Upload data to a dataframe
#Take only unique Donors becasue donors repeat becasue they give to multiple members
geos<- unique(na.omit(data.frame(bonica.cid = Descriptive_2c$bonica.cid, cycle =2018, longitude = Descriptive_2c$longitude2, latitude = Descriptive_2c$latitude2)))

#************************ Get Congressional Districts *************
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
geos$District <- over(geos,districts)$CD116FP

#Takes the values for State and adds them to your geos data
geos$State <- over(geos,districts)$STATEFP 

#*********************** Data Management ***********************

#********************* Add State Name ****************
setwd("C:/Users/Shari/OneDrive/Data/State FIPS")
State_Dictonary <- read.csv("State Fips Code.csv")

#******************** Match States FIPS *****************
#Paste Zero infront of Single Districts
State_Dictonary$FIPS <- as.character(State_Dictonary$FIPS)
State_Dictonary$FIPS[State_Dictonary$FIPS == "0"] <- "01"
State_Dictonary$FIPS[State_Dictonary$FIPS == "1"] <- "01"
State_Dictonary$FIPS[State_Dictonary$FIPS == "2"] <- "02"
State_Dictonary$FIPS[State_Dictonary$FIPS == "3"] <- "03"
State_Dictonary$FIPS[State_Dictonary$FIPS == "4"] <- "04"
State_Dictonary$FIPS[State_Dictonary$FIPS == "5"] <- "05"
State_Dictonary$FIPS[State_Dictonary$FIPS == "6"] <- "06"
State_Dictonary$FIPS[State_Dictonary$FIPS == "7"] <- "07"
State_Dictonary$FIPS[State_Dictonary$FIPS == "8"] <- "08"
State_Dictonary$FIPS[State_Dictonary$FIPS == "9"] <- "09"


#***************** Merge State Snames 
geos.2 <- merge(x = data.frame(geos), y = State_Dictonary, by.x = c("State"), by.y = c("FIPS"), all.x = T)

#********************* Fix Districts
#Paste Zero infront of Single Districts
geos.2$district_code <- as.character(geos.2$District)
geos.2$district_code[geos.2$district_code == "00"] <- "01"
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

geos.2$Congressional_District <- paste(geos.2$Postal.Code, geos.2$district_code, sep = "")

#********************** Make Final Dataset *****************
geos.final <- data.frame(bonica.cid = geos.2$bonica.cid, cycle= geos.2$cycle, Congressional_District = geos.2$Congressional_District)

#Rename Columns
colnames(geos.final)[3] <- "contributor.district2018"

#Remove Cycle
geos.final$cycle <- NULL

#Merge
Descriptive_3 <- merge(x = Descriptive_2c, y = geos.final, by = c("bonica.cid"), all.x = T)

#Check
print(nrow(Descriptive_2c) == nrow(Descriptive_3))

#Replace Lat and Long
Descriptive_3$latitude <- Descriptive_3$latitude2; Descriptive_3$latitude2 <- NULL; Descriptive_3$lat <- NULL
Descriptive_3$longitude <- Descriptive_3$longitude2; Descriptive_3$longitude2 <- NULL; Descriptive_3$long <- NULL

#****************** Add At Large Districts *****************
Descriptive_3$contributor.district2018[Descriptive_3$contributor.state == "DC"] <- "DC01"
Descriptive_3$contributor.district2018[Descriptive_3$contributor.state == "AK"] <- "AK01"
Descriptive_3$contributor.district2018[Descriptive_3$contributor.state == "DE"] <- "DE01"
Descriptive_3$contributor.district2018[Descriptive_3$contributor.state == "MT"] <- "MT01"
Descriptive_3$contributor.district2018[Descriptive_3$contributor.state == "ND"] <- "ND01"
Descriptive_3$contributor.district2018[Descriptive_3$contributor.state == "SD"] <- "SD01"
Descriptive_3$contributor.district2018[Descriptive_3$contributor.state == "VT"] <- "VT01"
Descriptive_3$contributor.district2018[Descriptive_3$contributor.state == "WY"] <- "WY01"

#****************** Drop NANA Districts ******************
Descriptive_3$contributor.district2018[Descriptive_3$contributor.district2018 == "NANA"] <- NA

View(with(Descriptive_3, data.frame(contributor.address.full, latitude, longitude, contributor.district.10s, contributor.district2018, Method)))

sum(is.na(Descriptive_3$contributor.district2018))

end_time <- Sys.time()

end_time - start_time

