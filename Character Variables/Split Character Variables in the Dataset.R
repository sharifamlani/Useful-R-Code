#Split the character variables in the dataset
library(dplyr)
library(tidyr)
Cong.114.1 <- Cong.114.1 %>%
  separate(Race..Ethnicity, c("Race", "Ethnicity"), "-")