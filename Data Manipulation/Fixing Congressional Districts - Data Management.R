State_Dictonary <- rbind(data.frame(state.abb, state.name), data.frame(state.abb = "DC", state.name = "District Of Columbia"))

House.Vote.2 <- merge(x = House.Vote.2, y = State_Dictonary, by.x = c("State"), by.y = c("state.name"))

#********************* Fix Districts
#Paste Zero infront of Single Districts
House.Vote.2$district_code <- as.character(House.Vote.2$District_Number)
House.Vote.2$district_code[House.Vote.2$district_code == "0"] <- "01"
House.Vote.2$district_code[House.Vote.2$district_code == "1"] <- "01"
House.Vote.2$district_code[House.Vote.2$district_code == "2"] <- "02"
House.Vote.2$district_code[House.Vote.2$district_code == "3"] <- "03"
House.Vote.2$district_code[House.Vote.2$district_code == "4"] <- "04"
House.Vote.2$district_code[House.Vote.2$district_code == "5"] <- "05"
House.Vote.2$district_code[House.Vote.2$district_code == "6"] <- "06"
House.Vote.2$district_code[House.Vote.2$district_code == "7"] <- "07"
House.Vote.2$district_code[House.Vote.2$district_code == "8"] <- "08"
House.Vote.2$district_code[House.Vote.2$district_code == "9"] <- "09"

House.Vote.2$CD <- paste(House.Vote.2$state.abb, House.Vote.2$district_code, sep = "")