#lag and Lead a Variable by Groups

library(dplyr)
Bonica.1b <- Bonica.1a %>% dplyr::group_by(bonica.rid, Donor_Location, Overall_Type) %>% dplyr::mutate(contributor.cfscore_mean_m1 = lag(contributor.cfscore_mean),
                                                                                         recipient.cfscore.dyn_m1 = lag(recipient.cfscore.dyn))   

#See: https://stats.stackexchange.com/questions/25889/lagging-over-a-grouped-time-series