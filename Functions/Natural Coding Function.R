#Natural Coding
Natural.Code <- function(data.column, type){
  
  if(type == "o"){
  magic_number <- length(unique(na.omit(data.column))) 
  Natural_Column <- (((data.column*-1) + magic_number) + 1) 
  return(Natural_Column) 
  }
  
  if(type == "c"){
    Natural_Column <- ((data.column*-1) + abs(max(data.column)) + 1) 
    return(Natural_Column)  
  }
}


#Example

#continuous
x <- c(1.2, 2, .3, 3, 5)
Natural.Code(x, type ="c")
table(Natural.Code(x, type = "c"), x)


#Ordinal
x <- c(0, 1,2, 3, 4, 5)
Natural.Code(x, type = "o")
table(Natural.Code(x, type = "o"), x) 

