# Sharif Amlani
# Winter Quarter 2018
# R 3.5.1
# 
# This R script is a tutotial for a valued ERGM

#Link to tutorial can be found here:
#https://statnet.org/trac/raw-attachment/wiki/Sunbelt2015/Valued.pdf

######################## Prelude ###############################

rm(list=ls(all=TRUE))

#*************************Upload Data************************************

options(stringsAsFactors = FALSE)
options(scipen = 3)


library(ergm.count)
library(latentnet)


############# Method 1: From a sociomatrix ############
data(samplk)
ls()

as.matrix(samplk1)[1:5, 1:5]

# Create a sociomatrix totaling the nominations.
samplk.tot.m <- as.matrix(samplk1) + as.matrix(samplk2) + as.matrix(samplk3)
samplk.tot.m[1:5, 1:5]

# Create a network where the number of nominations becomes an
# attribute of an edge.
samplk.tot <- as.network(samplk.tot.m, directed = TRUE, matrix.type = "a",
                         ignore.eval = FALSE, names.eval = "nominations" # Important!
)
# Add vertex attributes. (Note that names were already
# imported!)
samplk.tot %v% "group" <- samplk1 %v% "group" # Groups identified by Sampson
samplk.tot %v% "group"

# We can view the attribute as a sociomatrix.
as.matrix(samplk.tot, attrname = "nominations")[1:5, 1:5]

# Also, note that samplk.tot now has an edge if i nominated j
# *at least once*.
as.matrix(samplk.tot)[1:5, 1:5]


################ Method 2: Form an edgelist ##################

samplk.tot.el <- as.matrix(samplk.tot, attrname = "nominations",
                           matrix.type = "edgelist")
samplk.tot.el[1:5, ]


# and an initial empty network.
samplk.tot2 <- samplk1 # Copy samplk1

delete.edges(samplk.tot2, seq_along(samplk.tot2$mel)) # Empty it out
samplk.tot2 #We could also have used network.initialize(18)

samplk.tot2[samplk.tot.el[, 1:2], names.eval = "nominations",
            add.edges = TRUE] <- samplk.tot.el[, 3]
as.matrix(samplk.tot2, attrname = "nominations")[1:5, 1:5]


############# Visualizing a valued network ###########

par(mar = rep(0, 4))
samplk.ecol <- matrix(gray(1 - (as.matrix(samplk.tot, attrname = "nominations")/3)),
                      nrow = network.size(samplk.tot))
plot(samplk.tot, edge.col = samplk.ecol, usecurve = TRUE, edge.curve = 1e-04,
     displaylabels = TRUE, vertex.col = as.factor(samplk.tot %v%
                                                    "group"))


par(mar = rep(0, 4))
valmat <- as.matrix(samplk.tot, attrname = "nominations") #Pull the edge values
samplk.ecol <- matrix(rgb(0, 0, 0, valmat/3), nrow = network.size(samplk.tot))
plot(samplk.tot, edge.col = samplk.ecol, usecurve = TRUE, edge.curve = 1e-04,
     displaylabels = TRUE, vertex.col = as.factor(samplk.tot %v%
                                                    "group"), edge.lwd = valmat^2)


data(zach)
zach.ecol <- gray(1 - (zach %e% "contexts")/8)
zach.vcol <- rainbow(5)[zach %v% "faction.id" + 3]
par(mar = rep(0, 4))
plot(zach, edge.col = zach.ecol, vertex.col = zach.vcol, displaylabels = TRUE)

############# Valued ERGMs #############

summary(samplk.tot ~ sum)
#produces an error (because no such term has been implemented for binary mode),
#while

summary(samplk.tot ~ sum, response = "nominations")
#gives the summary statistics. We will introduce more statistics shortly. First, we need to
#introduce the notion of valued ERGMs.


#************* Valued ERGM
help("ergm-references")

y <- network.initialize(2, directed = FALSE) # A network with one dyad!
## Discrete Uniform reference 0 coefficient: discrete uniform
sim.du3 <- simulate(y ~ sum, coef = 0, reference = ~DiscUnif(0,
                                                             3), response = "w", statsonly = TRUE, nsim = 1000)
# Negative coefficient: truncated geometric skewed to the
# right
sim.trgeo.m1 <- simulate(y ~ sum, coef = -1, reference = ~DiscUnif(0,
                                                                   3), response = "w", statsonly = TRUE, nsim = 1000)
# Positive coefficient: truncated geometric skewed to the
# left
sim.trgeo.p1 <- simulate(y ~ sum, coef = +1, reference = ~DiscUnif(0,3), response = "w", statsonly = TRUE, nsim = 1000)
# Plot them:
par(mfrow = c(1, 3))
hist(sim.du3, breaks = diff(range(sim.du3)) * 4)
hist(sim.trgeo.m1, breaks = diff(range(sim.trgeo.m1)) * 4)
hist(sim.trgeo.p1, breaks = diff(range(sim.trgeo.p1)) * 4)


## Binomial reference 0 coefficient: Binomial(3,1/2)
sim.binom3 <- simulate(y ~ sum, coef = 0, reference = ~Binomial(3),
                       response = "w", statsonly = TRUE, nsim = 1000)
# -1 coefficient: Binomial(3, exp(-1)/(1+exp(-1)))
sim.binom3.m1 <- simulate(y ~ sum, coef = -1, reference = ~Binomial(3),
                          response = "w", statsonly = TRUE, nsim = 1000)
# +1 coefficient: Binomial(3, exp(1)/(1+exp(1)))
sim.binom3.p1 <- simulate(y ~ sum, coef = +1, reference = ~Binomial(3),
                          response = "w", statsonly = TRUE, nsim = 1000)
# Plot them:
par(mfrow = c(1, 3))
hist(sim.binom3, breaks = diff(range(sim.binom3)) * 4)
hist(sim.binom3.m1, breaks = diff(range(sim.binom3.m1)) * 4)
hist(sim.binom3.p1, breaks = diff(range(sim.binom3.p1)) * 4)

#********** Valued ERGM terms

samplk.tot.nm <- ergm(samplk.tot ~ sum + nodematch("group", diff = TRUE,
                                                   form = "sum"), response = "nominations", reference = ~Binomial(3))
## Warning in logLikNull.ergm(mainfit): Null model likelihood calculation is
#not implemented for valued ERGMs at this time.
mcmc.diagnostics(samplk.tot.nm)
