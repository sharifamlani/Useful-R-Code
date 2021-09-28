

############ Verson 3 ####################
CD_Intersect <- function(df1, df2){
  # Calculate area and tidy up
  
  intersect_pct <- st_intersection(df1, df2) %>% 
    mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
    dplyr::select(Congressional_District, Congressional_District_Minus10 = Congressional_District.1, Year, intersect_area)
  
  # Create a fresh area variable for counties
  #Grab CD Area for recient year
  nc <- mutate(df1, CD_area = st_area(df1))%>%   # create new column with shape area
    dplyr::select(Congressional_District, CD_area) %>%   # only select columns needed to merge
    st_drop_geometry()  # drop geometry as we don't need it
  
  
  #Grab CD Area for past year
  nc_m1 <- mutate(df2, CD_area_m1 = st_area(df2))%>%   # create new column with shape area
    dplyr::select(Congressional_District_Minus10 = Congressional_District,  CD_area_m1, Compare_Year = Year) %>%   # only select columns needed to merge
    st_drop_geometry()  # drop geometry as we don't need it
  
  # Merge by county name
  nc <- merge(nc, intersect_pct, by = "Congressional_District", all.x = TRUE)
  nc <- merge(nc, nc_m1, by = "Congressional_District_Minus10", all.x = TRUE)
  
  # Calculate coverage
  nc <- nc %>% 
    mutate(coverage = as.numeric(intersect_area/CD_area))
  
  #Final Dataset
  return(st_sf(nc))
  
}

############ Verson 2 #################
CD_Intersect <- function(df1, df2){
  
  # Calculate area and tidy up
  intersect_pct <- st_intersection(df1, df2) %>% 
    mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
    dplyr::select(Congressional_District, Congressional_District_Minus10 = Congressional_District.1,  intersect_area) %>%   # only select columns needed to merge
    st_drop_geometry()  # drop geometry as we don't need it
  
  # Create a fresh area variable for counties
  #Grab CD Area for recient year
  nc <- mutate(df1, CD_area = st_area(df1))
  
  #Grab CD Area for past year
  nc_m1 <-with(mutate(df2, CD_area_m1 = st_area(df2)), data.frame(Congressional_District, CD_area_m1))
  
  # Merge by county name
  nc <- merge(nc, intersect_pct, by = "Congressional_District", all.x = TRUE)
  nc <- merge(nc, nc_m1, by = "Congressional_District", all.x = TRUE)
  
  # Calculate coverage
  nc <- nc %>% 
    mutate(coverage = as.numeric(intersect_area/CD_area))
  
  #Subset Valueable Infromation
  Final <- with(nc, data.frame(Congressional_District,Congressional_District_Minus10, Year, Compare_Year = (unique(nc$Year)-10),CD_area_m1, CD_area, intersect_area, coverage, geometry))  
  return(st_sf(Final))
  
}




############ Verson 1 ####################
CD_Intersect <- function(df1, df2){
  
  # Calculate area and tidy up
  intersect_pct <- st_intersection(df1, df2) %>% 
    mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
    dplyr::select(Congressional_District, Congressional_District_Minus10 = Congressional_District.1,  intersect_area) %>%   # only select columns needed to merge
    st_drop_geometry()  # drop geometry as we don't need it
  
  # Create a fresh area variable for counties
  nc <- mutate(df1, county_area = st_area(df1))
  
  # Merge by county name
  nc <- merge(nc, intersect_pct, by = "Congressional_District", all.x = TRUE)
  
  # Calculate coverage
  nc <- nc %>% 
    mutate(coverage = as.numeric(intersect_area/county_area))
  
  #Subset Valueable Infromation
  Final <- with(nc, data.frame(Congressional_District,Congressional_District_Minus10, Year=unique(nc$Year), Compare_Year = (unique(nc$Year)-10), county_area, intersect_area, coverage))  
  return(Final)
  
}


############# Testable Data #############

df1 <- subset(districts_12, STATENAME %in% c("Iowa"))
df2 <- subset(districts_02, STATENAME %in% c("Iowa"))