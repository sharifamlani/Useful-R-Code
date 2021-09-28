# library
library(igraph)

# create data:
links=data.frame(
  source=c("A","A", "A", "A", "A","J", "B", "B", "C", "C", "D","I"),
  target=c("B","B", "C", "D", "J","A","E", "F", "G", "H", "I","I")
)

# Turn it into igraph object
network <- graph_from_data_frame(d=links, directed=F) 

# Count the number of degree for each node:
deg <- degree(network, mode="all")

# Plot
plot(network, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5) )


g <- make_ring(10) %>%
  set_vertex_attr("label", value = LETTERS[1:10])
g
plot(g)
