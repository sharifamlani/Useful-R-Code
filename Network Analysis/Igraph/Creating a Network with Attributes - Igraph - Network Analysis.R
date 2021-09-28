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
setwd("C:/Users/Shari/OneDrive/POL279-2018/Congress/Roll Call/Senate")

#Upload Data
Senate_Character_pure <- read.csv("senator characteristics.csv"); Senate_Character.1 <- Senate_Character_pure

Senate_Di_pure <- read.csv("senate roll call agreement--dichotomized.csv"); Senate_Di.1 <- Senate_Di_pure

##################### Examine the Data #######################
head(Senate_Character.1)
head(Senate_Di.1)

#################### Data management ##################

#Every unique US Senator
Senate_Character.2 <- unique(with(Senate_Character.1, data.frame(congress, senatori, bionamei, statei, 
                                                                 partyi,  partynamei, tenurei)))



#Make -1 , 0s
Senate_Di.1$dichagree[Senate_Di.1$dichagree == -1] <- 0

##################### Subset years we want ####################

'%!in%' <- function(x,y)!('%in%'(x,y))

Senate_Di.92 <- subset(Senate_Di.1, congress == 92  & Senate_Di.1$senatori %!in% c(14107, 14108) & Senate_Di.1$senatorj %!in% c(14107, 14108))

Senate_Character.92 <- subset(Senate_Character.2, congress == 92 & Senate_Character.2$senatori %!in% c(14107, 14108) )

#Double check for unique values
subset(data.frame(table(Senate_Character.92$senatori)), Freq >1)

########### Build the Network #########
#We create the dataframe with essental infromation
MC_Network_Edgelist <- data.frame(senatori = Senate_Di.92$senatori,
                                  senatorj = Senate_Di.92$senatorj, 
                                  dichagree = Senate_Di.92$dichagree)

#We take out those nodes that do not have an edge & self ties
MC_Network_Edgelist_N <- subset(MC_Network_Edgelist, dichagree !=0 & 
                                  senatori != senatorj)


#Kill the column representing the value of the node
MC_Network_Edgelist_N$dichagree <- NULL

#Make the IDs into chracters -- This is important 
MC_Network_Edgelist_N$senatori <- as.character(MC_Network_Edgelist_N$senatori)
MC_Network_Edgelist_N$senatorj <- as.character(MC_Network_Edgelist_N$senatorj)

#Check to see if they are characters
apply(MC_Network_Edgelist_N, 2, class)

########### Make Network Object #########

# Turn it into igraph object
library(igraph)
network <- graph_from_data_frame(d=MC_Network_Edgelist_N, directed=F) 

plot(network)

#What are the nodes
V(network)

#Calulate the Network stats
igraph::degree(network)
igraph::closeness(network)
igraph::betweenness(network)
igraph::eigen_centrality(network)$vector


##################### Add Attributes ########################
#Make sure the network nodes and the attribute data frame are in the SAME order!

vertex_attr(network, "name") == Senate_Character.92$senatori

network_With_Attributes <- network %>% 
  set_vertex_attr("BioName", value = Senate_Character.92$bionamei) %>%
  set_vertex_attr("PartyName", value = Senate_Character.92$partynamei)%>%
  set_vertex_attr("State", value = Senate_Character.92$statei)


##################### Plot ########################

plot(network_With_Attributes, 
     vertex.color=c( "red", "blue")[1+(V(network_With_Attributes)$PartyName=="Democrat")])

