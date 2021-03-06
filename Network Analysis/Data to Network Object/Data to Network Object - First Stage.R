# read in a relational data adjacency matrix

# LOADING IN A MATRIX
## Not run: 
# can download matrix file from 
# https://statnet.csde.washington.edu/trac/raw-attachment/wiki/Resources/relationalData.csv
# and download vertex attribute file from
# https://statnet.csde.washington.edu/trac/raw-attachment/wiki/Resources/vertexAttributes.csv

library(data.table)

# load in relation matrix from file
relations <- fread("https://statnet.csde.washington.edu/trac/raw-attachment/wiki/Resources/relationalData.csv", header=FALSE,stringsAsFactors=FALSE)

# convert to matrix format from data frame
relations <- as.matrix(relations) 

# load in vertex attributes
nodeInfo <- fread("https://statnet.csde.washington.edu/trac/raw-attachment/wiki/Resources/vertexAttributes.csv",header=TRUE,stringsAsFactors=FALSE)

## End(Not run)


print(relations) # peek at matrix 
print(nodeInfo)  # peek at attribute data

# Since our relational data has no row/column names, let's set them now
rownames(relations) <- nodeInfo$name
colnames(relations) <- nodeInfo$name

# create undirected network object from matrix
nrelations<-network(relations,directed=FALSE)
summary(nrelations)

# it read in vertex names from matrix col names ...
network.vertex.names(nrelations)

# ATTACHING VERTEX ATTRIBUTES

# ... but could also set vertex.names with 
nrelations%v%'vertex.names'<- nodeInfo$name

# load in other attributes 
nrelations%v%"age" <- nodeInfo$age
nrelations%v%"sex" <- nodeInfo$sex
nrelations%v%"handed" <- nodeInfo$handed
nrelations%v%"lastDocVisit" <- nodeInfo$lastDocVisit

# Note: order of attributes in the data frame MUST match vertex ids
# otherwise the attribute will get assigned to the wrong vertex

# check that they got loaded
list.vertex.attributes(nrelations)


# what if we had an adjaceny  matrix like:
valuedMat<-matrix(c(1,2,3, 2,0,9.5,1,5,0),ncol=3,byrow=TRUE)
valuedMat

# make a network from it
valuedNet<-network(valuedMat,loops=TRUE,directed=TRUE)

# print it back out ...
as.matrix(valuedNet)

# wait, where did the values go!!?

# LOADING A MATRIX WITH VALUES

# to construct net from matrix with values:
valuedNet<-network(valuedMat,loops=TRUE,directed=TRUE,
                   ignore.eval=FALSE,names.eval='myEdgeWeight')
summary(valuedNet)
# also have to specify the name of the attribute when converting to matrix
as.matrix(valuedNet,attrname='myEdgeWeight')

# ATTACHING EDGE ATTRIBUTES FROM A MATRIX

# maybe we have edge attributes of a different sort in another matrix like:
edgeAttrs<-matrix(c("B","Z","Q","W","A","E","L","P","A"),ncol=3,byrow=TRUE)
edgeAttrs

# we can still attach them
valuedNet<-set.edge.value(valuedNet,'someLetters',edgeAttrs)

# and extract them
as.matrix(valuedNet,attrname='someLetters')
valuedNet%e%'someLetters'

# but notice that some of the values didn't get used 
# the ("A"s are missing) because there were no corresponding edges (loops)
# for the attribute to be attached to


# ATTACHING EDGE ATTRIBUTES FROM A LIST

# it is also possible to attach edge attributes directly from a list
edgeCols<-c("red","green","blue","orange","pink","brown","gray")
valuedNet<-set.edge.attribute(valuedNet,"edgeColors",edgeCols)

# but this can be risky, because we may not know the ordering of the edges,
# (especially if some have been deleted).  Does "green" go with the edge from 
# 1 to 2, or from 3 to 1?

# Usually if the edge data is only availible in list form, it is safer to construct
# the network from an edgelist in the first place

# LOADING IN AN EDGELIST

# pretend we just loaded in this data.frame from a file
elData<-data.frame(
  from_id=c("1","2","3","1","3","1","2"),
  to_id=c("1", "1", "1", "2", "2", "3", "3"),
  myEdgeWeight=c(1, 2, 1, 2, 5, 3, 9.5),
  someLetters=c("B", "W", "L", "Z", "P", "Q", "E"),
  edgeCols=c("red","green","blue","orange","pink","brown","gray"),
  stringsAsFactors=FALSE
)

# peek at data
# each row corresponds to a relationship (edge) in the network
elData

# to make a network we just use the first two id columns
valuedNet2<-network(elData[,1:2],matrix.type='edgelist')

# print it out
as.matrix(valuedNet2)

# has right edges, but no values

# to include values (with names from the columns)

valuedNet2<-network(elData,matrix.type='edgelist',ignore.eval=FALSE, names.evals = "myEdgeWeight")
summary(valuedNet2)
list.edge.attributes(valuedNet2)
as.matrix(valuedNet2, attrname='someLetters')




#******************* ERGM *************************
EModel1 <- ergm(valuedNet2 ~ edges() + edgecov(valuedNet2, attrname='edgeCols'), responce = "myEdgeWeight")


summary(EModel1)
