#Sharif Amlani
#R 3.6.2
#Spring 2019

####################### Code Summary ##################

######################## Prelude ###############################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)

##################### Upload Data ##################

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/University of California, Davis/Fourth Year/Money and Network Analysis Project/Data/Working Data/Member Network")

#Upload Data
load(file = "Member Network - PAC Contributions - 1982 - 2018--W2.rda"); #MC_Network_Final

##################### Examine Data #################
head(MC_Network_Final)

##################### Data Management ###############

#Create Connectedness Measure
MC_Network_Final$Connectedness <- ifelse(MC_Network_Final$Fowler_weight > 0, 1/MC_Network_Final$Fowler_weight, NA)
summary(MC_Network_Final$Connectedness)


##################### Use Relevant Data #############

data.loop.1 <- subset(MC_Network_Final, year == 2012)
Edgelist.1 <- with(data.loop.1, data.frame(From, To, Connectedness))

##################### Dijkstra shortest distance algorithm #############

library(igraph)
library(reshape2)

#Sharif Amlani
#R 3.6.2
#Spring 2019

####################### Code Summary ##################

######################## Prelude ###############################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)

##################### Upload Data ##################

#Set Working Directory
setwd("C:/Users/shari/OneDrive/Data/FEC - PAC to Canidate/Master Contribution File")

#Upload Data
load(file = "Contributions Master.rda"); Contribution.1 <- Contribution_Pure


##################### Subset 2018 Cycle ################
head(Contribution.1)

Contribution.2018 <- subset(Contribution.1, cycle == 2018)

##################### Aggregate Contributions ##################
colnames(Contribution.2018)
Contribution.2018.Agg <- aggregate(transaction_amt ~ cmte_id + cand_id, data = Contribution.2018, sum)

#################### Make into an Adjency Matrix ###############
library(igraph)
library(reshape2)

#Make into an Igraph Object
mygraph <- graph.data.frame(Contribution.2018.Agg)

#Convert to an Adjacency Matrix
Adjacency.1 <- get.adjacency(mygraph, sparse = FALSE, attr=attribute)

#Make into a Adjacency Network Object 
g <- graph.adjacency(Adjacency.1, weighted=TRUE)

#Calulate Dijkstra shortest distances
s.paths <- shortest.paths(g, algorithm = "dijkstra")

#Make back into an edgelist
Dijkstra_edgelist.1 <- setNames(melt(s.paths, id.vars = c("From"),
                                     variable.name = "To", 
                                     value.name = "dijkstra"), c("From", "To", "Dijkstra"))

head(Dijkstra_edgelist.1)


################ Functions ################
Dijkstra <- function(Edgelist, attribute){
  library(igraph)
  library(reshape2)
  #Make into an Igraph Object
  mygraph <- graph.data.frame(Edgelist)
  
  #Convert to an Adjacency Matrix
  Adjacency.1 <- get.adjacency(mygraph, sparse = FALSE, attr=attribute)
  
  #Make into a Adjacency Network Object 
  g <- graph.adjacency(Adjacency.1, weighted=TRUE)
  
  #Calulate Dijkstra shortest distances
  s.paths <- shortest.paths(g, algorithm = "dijkstra")
  
  #Make back into an edgelist
  Dijkstra_edgelist.1 <- setNames(melt(s.paths, id.vars = c("From"),
                                       variable.name = "To", 
                                       value.name = "dijkstra"), c("From", "To", "Dijkstra"))
  
  return(Dijkstra_edgelist.1)
  
}