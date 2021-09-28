#Sharif Amlani
#R 3.6.2
#Spring 2019


####################### Code Summary ##################
#This code maps donors accross congressional districts

######################## Prelude ###############################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)

###################### Functions ##################
get_congress_map <- function(cong=113) {
  tmp_file <- tempfile()
  tmp_dir  <- tempdir()
  zp <- sprintf("http://cdmaps.polisci.ucla.edu/shp/districts%03i.zip",cong)
  download.file(zp, tmp_file)
  unzip(zipfile = tmp_file, exdir = tmp_dir)
  fpath <- paste(tmp_dir, sprintf("districtShapes/districts%03i.shp",cong), sep = "/")
  st_read(fpath)
}

##################### Library ################
library(tidyverse)
library(ggplot2)
library(sf)
library(ggmap)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(gthemes)

##################### Upload Data ##################

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/University of California, Davis/Fourth Year/Dissertation Prospectus/Out of State Donors/Data")

#Upload Data
load(file = "House Contributor and Recipients 2016 -- w3.rda"); Bonica.1<- Bonica.Final

#Upload Map
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

############### Aggregate Number and Total Amount of Contributions ################
library(plyr)
colnames(Bonica.1)

#******************** Distribution of Donors *******************
Bonica.1$Count <- 1

Bonica.2<- join_all(list(
  setNames(aggregate(Count ~ contributor.district.10s, data = Bonica.1, sum), c("contributor.district.10s", "N_donors")),
  setNames(aggregate(Count ~ contributor.district.10s, data = Bonica.1, function(x) sum(x) /nrow(Bonica.1)), c("contributor.district.10s", "N_donors_Percent")),
  setNames(aggregate(amount ~ contributor.district.10s, data = Bonica.1, sum), c("contributor.district.10s", "Amount")),
  setNames(aggregate(amount ~ contributor.district.10s, data = Bonica.1, function(x) sum(x) /sum(Bonica.1$amount)), c("contributor.district.10s", "Amount_Percent")),
  setNames(aggregate(contributor.cfscore ~ contributor.district.10s, data = Bonica.1, mean), c("contributor.district.10s", "C_CFScore_Mean")),
  setNames(aggregate(contributor.cfscore ~ contributor.district.10s, data = Bonica.1, median), c("contributor.district.10s", "C_CFScore_Median")),
  setNames(aggregate(recipient.cfscore ~ contributor.district.10s, data = Bonica.1, mean), c("contributor.district.10s", "R_CFScore_Mean")),
  setNames(aggregate(recipient.cfscore ~ contributor.district.10s, data = Bonica.1, median), c("contributor.district.10s", "R_CFScore_Median")),
  setNames(aggregate(dwnom1 ~ contributor.district.10s, data = Bonica.1, mean), c("contributor.district.10s", "R_DW1_Mean")),
  setNames(aggregate(dwnom1 ~ contributor.district.10s, data = Bonica.1, median), c("contributor.district.10s", "R_DW1_Median"))
  
),
by = c("contributor.district.10s"))


#**************** Fundraising by Member ***************
Donor_Location <- merge(
  x = setNames(as.data.frame(table(Bonica.1$district, Bonica.1$Donor_Location.District)), c("District", "Location", "Count")),
  y = setNames(as.data.frame(table(Bonica.1$district)), c("District", "Total_Count")),
  by = "District")

Donor_Location$Percentage <- Donor_Location$Count / Donor_Location$Total_Count
head(Donor_Location)

#################### Merge Shape File to Donor #####################
colnames(Bonica.2)

#******************** Merge Descriptive Stats *********************
Bonica.3 <- merge(x = Bonica.2, y = cd114.1, by.x = c("contributor.district.10s"), by.y = c("CD"))

#checks
nrow(Bonica.3) == nrow(Bonica.2) #FALSE But that is alright

setdiff(unique(Bonica.2$contributor.district.10s), unique(Bonica.3$contributor.district.10s))
#We drop "No District Reported" "ILZZ" 
#Checks out

#******************* Merge In and Out of state donors ********************
Donor_Location.1 <- merge(x = Donor_Location, y = cd114.1, by.x = c("District"), by.y = c("CD"))

################### Data Management on Maps #####################

#************** Drop Alaska and Hawaii ***************
colnames(Bonica.3)
#Subset Names that are off the main land US
cd_main <- subset(Bonica.3, !(STATENAME %in% c("Alaska", "Virgin Islands", "Hawaii", "American Samoa", "Northern Mariana Islands", "Guam", "Puerto Rico")))

Donor_Location_main <- subset(Donor_Location.1, !(STATENAME %in% c("Alaska", "Virgin Islands", "Hawaii", "American Samoa", "Northern Mariana Islands", "Guam", "Puerto Rico")))

################# Map ########################
library(tmap)
library(sf)
library(lwgeom)

tmap_mode("view")

#************* Distribution of Donors *******************
#Check Geosatial Data
cd_sf <- st_as_sf(cd_main)
cd_sf.1 <- st_make_valid(cd_sf)

#Number of Donors
tm_shape(cd_sf.1) +
  tm_polygons("N_donors", id = "contributor.district.10s", palette  = "Greens", title = "Number of Donors")

#Amount Contributed 
tm_shape(cd_sf.1) +
  tm_polygons("Amount", id = "contributor.district.10s", palette  = "Greens", title = "Amount of Donations")


#*************** Members Fundraising Behavior *******************
#Check Geosatial Data
Donor_Location_Map1 <- st_as_sf(Donor_Location_main)
Donor_Location_Map2 <- st_make_valid(Donor_Location_Map1)

#Subset Relevant Data
Donor_Location_Map2.In <- subset(Donor_Location_Map2, Location == "In District")
Donor_Location_Map2.Out <- subset(Donor_Location_Map2, Location == "Out of District")

#Percentage of Outside Donors
tm_shape(Donor_Location_Map2.Out) +
  tm_polygons("Percentage", id = "District", palette  = "Reds", title = "Percent of Out-District Donors")

#Percentgae of Inside Donors
tm_shape(Donor_Location_Map2.In) +
  tm_polygons("Percentage", id = "District", palette  = "Blues", title = "Percent of In-District Donors")



#***************** Ideology **********************
library(ggplot2)

ggplot(cd_main) + geom_sf(aes(geometry = geometry, fill = C_CFScore_Mean)) +
  coord_sf(crs = st_crs(cd_main), datum = NA) +
  scale_fill_gradient2("Ideology", midpoint = mean(cd_main$C_CFScore_Mean, na.rm = T),
                       low = "blue4",
                       mid = "white",
                       high = "red1", 
                       na.value = "grey86",
                       breaks=c(-0.5,0,0.9),
                       labels=c("Liberal","", "Conservative")) +
  ggtitle("CF Score For Donors (2016 Election)") + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    # text = element_blank(), 
    plot.background = element_rect(fill = "White"),
    plot.title = element_text(hjust = 0.5),
    legend.position="bottom")
