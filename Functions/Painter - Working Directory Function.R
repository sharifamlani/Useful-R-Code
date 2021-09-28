
#Function to derive painter Working directory

Painter_WD <- function(wd, to = "Painter"){
  wd <- wd
  if(to =="Painter"){
    wd_new <- gsub("C:/Users/Shari/", "//tsclient/C/Users/Shari/", wd)
    
  }
  
  if(to !="Painter"){
    wd_new <- gsub( "//tsclient/C/Users/Shari/", "C:/Users/Shari/", wd)
    
  }
  
  return(wd_new)
}

#Example
setwd(Painter_WD("C:/Users/Shari/OneDrive/Data/FEC - PAC to Canidate/Master Contribution File"))

#NOT:
#Painter_WD(setwd("C:/Users/Sharif/OneDrive/Data/FEC - PAC to Canidate"))

