
################### Rename the Columns ###############

colnames(df)[colnames(df) == 'old.var.name'] <- 'new.var.name'


#or


library(dplyr)


New Data Set <- dplyr::rename(Data Set, 
                                New Name = Old Name, 
                                New Name = Old Name,
                                New Name = Old Name,
                                New Name = Old Name)
                                