
#Function to derive painter Working directory

#Function
Painter_WD <- function(wd, to = "Painter"){
  
  if(to =="Painter"){
    wd_new <- gsub("C:/Users/Sharif/", "//tsclient/C/Users/Sharif/", wd)
    
  }
  
  if(to !="Painter"){
    wd_new <- gsub( "//tsclient/C/Users/Sharif/", "C:/Users/Sharif/", wd)
    
  }
  
  return(wd_new)
}


#Example
Painter_WD("C:/Users/Sharif/OneDrive/Data/FEC - PAC to Canidate")
