#Lag a Variable by arranging date and grouping by

library(dplyr)

UP_2016.2  %>% arrange(state_county_fips, period)  %>% group_by(state_county_fips) %>% mutate(Unemployment_Rate_m1 = lag(Unemployment_Rate))   

