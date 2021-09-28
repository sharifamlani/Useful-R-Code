# This neat snippet sets your working directory to wherever the script is located!

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#From: Enos, Ryan D.; Kaufman, Aaron R.; Sands, Melissa L., 2019, "Replication Data for: Can Violent Protest Change Local Policy Support? Evidence from the Aftermath of the 1992 Los Angeles Riot", https://doi.org/10.7910/DVN/9B8HQN, Harvard Dataverse, V1