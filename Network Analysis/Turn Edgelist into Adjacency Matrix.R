#How to turn a weighted edgelist into an adjacency matrix in r
edgelist <- setNames(cbind(expand.grid(letters[1:2], letters[1:2]), runif(4)), c("From", "To", "Weight"))
edgelist

library(igraph)
mygraph <- graph.data.frame(edgelist)
get.adjacency(mygraph, sparse = FALSE, attr='Weight')