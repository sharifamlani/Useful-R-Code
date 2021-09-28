
#Latitude Longitude Coordinates to County in R

#https://stackoverflow.com/questions/23068780/latitude-longitude-coordinates-to-county-in-r
#https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r/8751965#8751965

library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county

counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))

# Convert pointsDF to a SpatialPoints object 
pointsSP <- SpatialPoints(testPoints, 
                          proj4string=CRS("+proj=longlat +datum=WGS84"))

# Use 'over' to get _indices_ of the Polygons object containing each point 
indices <- over(pointsSP, counties_sp)

# Return the county names of the Polygons object containing each point
countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
countyNames[indices]

data(county.fips)

#Get FIPS Codes
FIPS <- with(county.fips, fips[match(countyNames[indices], polyname)])

Final <- data.frame(testPoints, state_county_name=countyNames[indices], FIPS)

return(Final)


}

# Test the function using points in Wisconsin and Oregon.
testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))


latlong2county(testPoints)




