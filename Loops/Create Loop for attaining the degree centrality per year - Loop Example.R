# Create Loop for attaining the degree centrality per year
#```{r}
# Loop 

##This is set as Null because we need to clear out the rbind everytime we manually restart the loop
Final.Degree <- NULL

# Create a loop for i in unique values of trade dataset with year as the unique variable
for (i in unique(trade$year)){
  
  # Begin part one of loop
  ##loop. Trade.loop.1 is a object of trade when dichtrade ==1, we need to do this because igraph only takes two columns. It will create all the missing as null.
  Trade.loop.1 <-  subset(trade, dichtrade == 1)
  
  # Begin part two of the loop
  ## put object created above into new loop, for when year ==i (i is the unique values we set above)
  Trade.loop <-  subset(Trade.loop.1, year == i)
  
  
  # Select two Columns
  T.Network <- with(Trade.loop, data.frame(statea, stateb))
  
  # Making the country codes charaters
  T.Network[,1]=as.character(T.Network[,1])#Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems. 
  T.Network[,2]=as.character(T.Network[,2])
  
  # Making network object
  T.Network.Graph <- graph.edgelist((as.matrix(T.Network, type = "both", attr = NULL,
                                               edges = FALSE, names = TRUE)))
  
  # Calulating degree centrality
  T.Network.Graph.Degree <-degree(T.Network.Graph, mode = "out", loop = F)
  
  
  # Make into table with row names from column
  Degree.year <- as.data.frame(T.Network.Graph.Degree)
  
  # Change the Rownames of the table as column names
  Degree.year$statea <- rownames(Degree.year)
  Degree.year$year <- i
  Degree.year.final <- Degree.year
  
  # Use Rbind to crreate a final table
  Final.Degree <- rbind(Final.Degree, Degree.year.final)
  
}

# Write table out to CSV
Final.Degree.Subset <-subset(Final.Degree, year >=1950 & year<=2010)
write.csv(Final.Degree.Subset, file="centrality.csv")


```