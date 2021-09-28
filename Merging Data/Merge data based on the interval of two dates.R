
#Merge data based on the interval of two dates

Cabinet_MID.1 <- subset(merge(Cabinet.5,MID.5, by =c("country_id")), 
                        start_date_mid >= start_date_cabinet & start_date_mid <= end_date_cabinet)