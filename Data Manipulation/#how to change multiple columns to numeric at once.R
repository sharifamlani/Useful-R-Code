#how to change multiple columns to numeric at once 

#Data = Name of the Data


#Put the name of the column into one vector
cols.name <- c("Name of Column","V161160", "Name of Column", "Name of Column")

#Use sapply to convert the vector of column names into Numeric Numbers
#WARNING: Make sure That the numbers make sence. 
#Remember in the ANES, How "Refesued" was coded at 1, Double check the numbers
Data[cols.name] <- sapply(Data[cols.name],as.numeric)

#Confirm numberic changes
sapply(ANES2016.Traits, class)

#Don't Forget to double check the number coding