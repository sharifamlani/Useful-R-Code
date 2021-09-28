#Merging Datasets

LES.Valence <- merge(x = first data set, y = second data set, 
                     #if the variables are the same
                     by = c("Same Variable Name in Both Data Sets"), 
                     
                     #If the varibales have the different names
                     by.x = c("Name in First Data Set"), 
                     by.y = c("Name in Second Data Set"),
                    
                     #If you want to keep all unmatched data opservations in the merged dataset
                     all.x=T
                     
)