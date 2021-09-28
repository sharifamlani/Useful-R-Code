
#Function for checking whether a character is a number 

Is_Number <- function(Test){
  if(suppressWarnings(is.na(as.numeric(Test))) == TRUE){
    return(FALSE)
  }
  
  if(as.numeric(Test) == as.numeric(gsub("(?<![0-9])0+", "", Test, perl = TRUE))){
    return(TRUE)
  }
  
  if(as.numeric(Test) == as.numeric(Test)){
    return(TRUE)
    
  }
}

Test <- "05"
Is_Number(Test)

Test <- "OH05"
Is_Number(Test)

Test <- "5"
Is_Number(Test)
