#Sharif Amlani
#R 3.6.2
#Winter 2019

################# Code Summary ##################

#This code creates network data, gets it in shape, makes it into a network object and then runs an ERGM.

#GOAL:
#If it possible you can write a sample code to me through a cartoon example? 
#For example, how do you create a network object with 5 nodes, and (1,2) are connected, and (4,5) are connected. 
#Also, (1,2,3) are females and (4,5) are males. (1,2) is phd, (3) is mater, (4,5)are undergrad.


#Step 1: Make the Network Data

#Step 2: Make Edglist into a Network Object

#Step 3: Run ERGM

######################## Prelude ###############################


rm(list=ls(all=TRUE))

options(stringsAsFactors = FALSE)
options(scipen = 3)

########################## STEP 1 #########################
#Step 1: Make the Network Data

######################### Upload Data #######################
setwd("C:/Users/Shari/OneDrive/Friend Help/Tzu Liu/Network Example")

Network_pure <- read.csv("Example - Network Data.csv"); Network_1 <- Network_pure

######################## Check Data ###################
head(Network_1)

################## Make the Edgelist ###################
library(reshape2)
Network_Edgelist.1 <- melt(Network_1)

colnames(Network_Edgelist.1) <- c("From", "To", "Tie")
head(Network_Edgelist.1)

################# Add Attrbiutes to Edglist ###############

#********************* Create From Attributes to Edgelist *********************
Network_Edgelist.1$Gender.From <- ifelse(Network_Edgelist.1$From == "x1" | Network_Edgelist.1$From == "x2" |Network_Edgelist.1$From == "x3", "Female", NA)
Network_Edgelist.1$Gender.From <- ifelse(Network_Edgelist.1$From == "x4" | Network_Edgelist.1$From == "x5", "Male", Network_Edgelist.1$Gender.From )

Network_Edgelist.1$Education.From <- ifelse(Network_Edgelist.1$From == "x1" | Network_Edgelist.1$From == "x2", "Phd", NA)
Network_Edgelist.1$Education.From <- ifelse(Network_Edgelist.1$From == "x3", "Masters", Network_Edgelist.1$Education.From)
Network_Edgelist.1$Education.From <- ifelse(Network_Edgelist.1$From == "x4" | Network_Edgelist.1$From == "x5", "Undergrad", Network_Edgelist.1$Education.From)

#Check
table(Network_Edgelist.1$From, Network_Edgelist.1$Gender.From)
table(Network_Edgelist.1$From, Network_Edgelist.1$Education.From)

#********************* Create To Attributes to Edgelist *********************
Network_Edgelist.1$Gender.To <- ifelse(Network_Edgelist.1$To == "x1" | Network_Edgelist.1$To == "x2" |Network_Edgelist.1$To == "x3", "Female", NA)
Network_Edgelist.1$Gender.To <- ifelse(Network_Edgelist.1$To == "x4" | Network_Edgelist.1$To == "x5", "Male", Network_Edgelist.1$Gender.To )

Network_Edgelist.1$Education.To <- ifelse(Network_Edgelist.1$To == "x1" | Network_Edgelist.1$To == "x2", "Phd", NA)
Network_Edgelist.1$Education.To <- ifelse(Network_Edgelist.1$To == "x3", "Masters", Network_Edgelist.1$Education.To)
Network_Edgelist.1$Education.To <- ifelse(Network_Edgelist.1$To == "x4" | Network_Edgelist.1$To == "x5", "Undergrad", Network_Edgelist.1$Education.To)

#Check
table(Network_Edgelist.1$To, Network_Edgelist.1$Gender.To)
table(Network_Edgelist.1$To, Network_Edgelist.1$Education.To)

#*#################### Completed Edglist ###########################
#It is not uncommon to come accross a network that looks something like this!
head(Network_Edgelist.1)

####################### Step 2##########################
#Step 2: Make Edglist into a Network Object

##################### Step 2: Make Network  ###############################
library(dplyr)
library(ergm)
library(network)

#Data Management on the Adjency Matrix. Here were are using: Network_1
rownames(Network_1) <- Network_1$Nodes
Network_1$Nodes <- NULL
Network_1

#This code makes the nodal attributes. Here we are using: Network_Edgelist.1
Network_Edgelist_attr = Network_Edgelist.1 %>% group_by(From) %>%  select(contains(".From")) %>%
  summarize_all(first)

# Create base network
Network_Object.1 = network(Network_1,                       #This is just the nodes in the data
                       directed = FALSE,                   #Is the network Directoed
                       vertex.attr = Network_Edgelist_attr, #This adds the attributes.
                       loops = F)                          #Self Ties Yes or No


N = network.size(Network_Object.1)

#**************** Checks *****************
summary(Network_Object.1)

as.sociomatrix(Network_Object.1)

# Check if we needed to reorder attributes when adding to network (if all true OK):
Network_Edgelist_attr$From == Network_Object.1%v%"vertex.names"

########################## Step 3 ###############################
#Step 3: Run ERGM

#See for examples: https://cran.r-project.org/web/packages/ergm/vignettes/ergm.pdf 

#To see how to add covariates and network terms see: https://rdrr.io/cran/ergm/man/ergm-terms.html
########################## Run ERGM ###########################
library(ergm)

#Base Model
Model.1 <- ergm(Network_Object.1~edges) # fit model
summary(Model.1)

#Model With Attributes
Model.2 <- ergm(Network_Object.1~edges + nodematch("Gender.From") + nodematch("Education.From")) # fit model
summary(Model.2)

