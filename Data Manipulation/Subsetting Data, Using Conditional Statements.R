#Subseting Data

x <- c(1,1,1,1,1,1,1,1,1,1,2,1,1,1,1)
y <- c(1,1,1,1,0,1,0,1,1,1,1,1,1,1,1)

nrow(d)

d <- data.frame(x,y)
d

d1 <- d[!d$x == 2 | !d$y == 1,]


d1 <- subset(d,!d$x == 2 | !d$y == 1)

d1

nrow(d1)
