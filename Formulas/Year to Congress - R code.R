

#************** Function v2
Congress_Time <- function(i, return = NULL, type =NULL){
  
  if(return == "Congress"){
    
    if(type == "Congress Elected"){
      Cong <- (i-1786)/2
      message("You've entered year that elected this Congress")
      message(paste("Such that, the ", Cong, "th Congress was elected in ", i, " Election.", sep = ""))
      
      return(Cong)
    }
    
    if(type == "Congress Reelection"){
      Cong <- ((i-1786)/2) -1
      message("You've entered reelection year occuring this Congress.")
      message(paste("Such that, during the ", Cong, "th Congress, members were preparing for the ", i, " Election.", sep = ""))
      return(Cong)
    }
    
  }
  if(return == "Year"){
    
    #The Year the Congress was Elected
    if(type == "Year Elected"){
      year <- 1786 + i*2
      message(paste("The ", i, "th Congress was elected in the ", year, " Election.", sep = ""))
      return(year)
    }
    
    #The Year the Congress Starts
    if(type == "Year Start"){
      year <- (1786 + i*2) + 1
      message(paste("The ", i, "th Congress began in January ", year, ".", sep = ""))
      return(year)
    }
    
    #The relection cycle this congress is working inside
    if(type == "Reelection Cycle"){
      year <- (1786 + i*2) + 2
      message(paste("The ", i, "th Congress was preparing for the ", year, " Election.", sep = ""))
      return(year)
    }
    
  }
}


#************** Function
Congress_Time <- function(i, type =NULL){
  if(type == "Congress"){
    Cong <- (i-1786)/2
    message("You've entered year that elected this Congress")
    return(Cong)
  }
  
  #The Year the Congress was Elected
  if(type == "Year Elected"){
    year <- 1786 + i*2
    return(year)
  }
  
  #The Year the Congress Starts
  if(type == "Year Start"){
    year <- (1786 + i*2) + 1
    return(year)
  }
  
  #The relection cycle this congress is working inside
  if(type == "Reelection Cycle"){
    year <- (1786 + i*2) + 2
    return(year)
  }
  
}

Congress_Time(116, type ="Year Elected")
Congress_Time(116, type ="Year Start")
Congress_Time(116, type ="Reelection Cycle")
Congress_Time(2018, type ="Congress")




#This is the formula for getting the year of the elected congress from just knowing the congress number 

#Let's create a year variable that indicates the year the congress was elected *
#see the document "Counting Congresses . . .

#************* FORMULA

#Year
year = 1786 + cong*2

#Congress
Congress_Elected_To =  (year-1786)/2 #Elected to Congress in this year

Congress_Elected_From =  ((year-1786)/2) -1 #Facing relection in this year


#*********** FUNCTION

#Congress to Year
Cong_To_Year <- function(cong){
  year <- 1786 + cong*2
  return(year)
}

#Year to Congress
Year_To_Congress <- function(year){
  Cong <- (year-1786)/2
  return(Cong)
}