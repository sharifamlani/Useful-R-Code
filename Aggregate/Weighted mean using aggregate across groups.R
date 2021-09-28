#Weighted mean using aggregate across groups in r

set.seed(1980)
Group_1 <- sample(letters[1:4], 50, TRUE)
Group_2 <- sample(letters[8:13], 50, TRUE)
Weight <- sample(seq(1,50), 50, TRUE)
Value <- sample(seq(1,50), 50, TRUE)

DF <- data.frame(Group_1, Group_2, Weight, Value)

head(DF)

#Solution 
#https://stackoverflow.com/questions/64419276/weighted-mean-using-aggregate-across-groups-in-r#64419289

library(dplyr)
DF %>% dplyr::group_by(Group_1, Group_2) %>% dplyr::summarise(wt_mean = weighted.mean(Value, Weight))

#Extra
setNames(DF %>% dplyr::group_by(Group_1, Group_2) %>% dplyr::summarise(wt_mean = weighted.mean(Value, Weight), .groups = 'drop'), c("ID_Cycle", "Overall_Type", "contributor.cfscore_Wmean"))
