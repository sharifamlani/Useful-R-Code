#Sharif Amlani
#R 4.0.3
#Winter 2021

######################## Code Summary ##################
#https://cran.r-project.org/web/packages/cartogram/readme/README.html

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################
'%!in%' <- function(x,y)!('%in%'(x,y))

######################### Library #####################
library(sf)
library(tmap)
library(cartogram)
library(ggplot2)
library(ggmap)
library(maptools)


###################### Get Congress Map ###################
get_congress_map <- function(cong=113) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}


cd114 <- get_congress_map(114)

################ Create Congressional District ID ###################

State_Dictonary <- rbind(data.frame(state.abb, STATENAME= state.name), data.frame(state.abb= "DC", STATENAME= "District Of Columbia"))

#Add DC
cd114.1 <- merge(State_Dictonary, cd114, by = c("STATENAME"))
unique(cd114$STATENAME)
#**************** Paste Zero infront of Single Districts ****************
cd114.1$district_code <- as.character(cd114.1$DISTRICT)
cd114.1$district_code[cd114.1$district_code == 98] <- "01" #Account for the weird code for DC
cd114.1$district_code[cd114.1$district_code == 0] <- "01"  #Account for at large districts
cd114.1$district_code[cd114.1$district_code == 1] <- "01"
cd114.1$district_code[cd114.1$district_code == 2] <- "02"
cd114.1$district_code[cd114.1$district_code == 3] <- "03"
cd114.1$district_code[cd114.1$district_code == 4] <- "04"
cd114.1$district_code[cd114.1$district_code == 5] <- "05"
cd114.1$district_code[cd114.1$district_code == 6] <- "06"
cd114.1$district_code[cd114.1$district_code == 7] <- "07"
cd114.1$district_code[cd114.1$district_code == 8] <- "08"
cd114.1$district_code[cd114.1$district_code == 9] <- "09"

#************** Create CD in  Data ****************
cd114.1$CD <- paste(cd114.1$state.abb, cd114.1$district_code, sep = "")


################ Create Fake Data TO Simulate My Own ########################
CD <- unique(cd114.1$CD)
Values <- sample(1:100, 436, replace = T)

Df <- data.frame(CD, Values)


################### Merge Shape File to Donor #####################
#******************** Merge Descriptive Stats *********************
Df.2 <- merge(x = Df, y = cd114.1, by = c("CD"), all.y = T)

################### Make SF Object ####################
SF.DF.1 <- st_as_sf(Df.2)
SF.DF.2 <- st_make_valid(SF.DF.1)
class(SF.DF.2)

################### Make cartogram ####################
#Follow Proceedure Laid out: https://cran.r-project.org/web/packages/cartogram/readme/README.html

# I use st_transform instead of spTransform beause I'm using an SF Ojbect
library(dplyr)

SF.DF.3 <- st_transform(SF.DF.1, CRS("+init=epsg:3395")) %>% 
  filter(!st_is_empty(.))

sf_ncont <- cartogram_cont(SF.DF.3, "Values",itermax = 3) #Continuous Area Cartogram
#sf_ncont <- cartogram_ncont(SF.DF.3, "Values") #Non-contiguous Area Cartogram

# tm_shape(sf_ncont) + tm_polygons("Values", style = "jenks") +
#   tm_layout(frame = FALSE, legend.position = c("left", "bottom"))

tm_shape(subset(sf_ncont,  state.abb  %!in% c("HI","AK"))) + tm_polygons("Values", style = "jenks") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))

