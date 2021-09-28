#This function takes the correaltion fo all numeric and integer columns in the data
#against a Dependent Varibale 

cor_hack <- function(Data, DV){
  df_cor_Final <- NULL
  for (i in colnames(Data)){
    
    if(sum(is.na(Data[[i]])) != length(Data[[i]])){
      
      if(class(Data[[i]]) == "numeric" | class(Data[[i]]) == "integer"){
        
        if(sd(Data[[i]], na.rm = T) >  0){
          Correlation <- cor(Data[[i]], Data[DV], use = "complete.obs")
          
          if(Correlation >= .2 | Correlation <= -.2){
            df_cor<- data.frame(Corr = Correlation)
            df_cor$variable <- i
            df_cor_Final <- rbind(df_cor_Final, df_cor)
          }
        }
      }
    }
  }
  
  return(df_cor_Final)
  
}

#Example
test <- cor_hack(CMPS.1, "social_belonging")
