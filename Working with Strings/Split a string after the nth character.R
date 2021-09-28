
#How to split a string after the nth character in r

District <- c("AR01", "AZ03", "AZ05", "AZ08", "CA01", "CA05", "CA11", "CA16", "CA18", "CA21")
#split district  starting at the first and ending at the second
state <- substr(District,1,2)
#split district starting at the 3rd and ending at the 4th
district <- substr(District,3,4)
#put in data frame if needed.
st_dt <- data.frame(state = state, district = district, stringsAsFactors = FALSE)