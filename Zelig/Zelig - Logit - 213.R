# Read in data

setwd("C:/Users/Sharif/OneDrive/University of California, Davis/First Year/Spring Quarter/Methods 213/Class data")

# Read in data

mydata <- read.table("FA.tx.txt", header=TRUE, sep="\t")

#*******************************Zelig

# Logit Model of Support -> ****The canned MOdel of that shit we did above!**

#The Model
z.out <- zelig(same_sex ~ independent + republican + born_christian + age,
               model="logit", data=field)


# Simulate expected probabilities, first differences

#set x = Set my x to ...
x.out1a <- setx(z.out, independent = 0, republican = 0, born_christian = 0, age = 53)

#Model  #Set x
s.out1a <- sim(z.out, x = x.out1a)

summary(s.out1a)
#Pay attention to the: 
#mean = Predicted probabilty.
#Sd, the uncertianly around that prediction

plot(s.out1a)

#The Second person
x.out1b <- setx(z.out, independent = 0, republican = 1, born_christian = 0, age = 53)

s.out1b <- sim(z.out, x = x.out1b)

summary(s.out1b)
plot(s.out1b)


s.out1 <- sim(z.out, x = x.out1a, x1 = x.out1b)
summary(s.out1)


#********************Plot estimated probabilities with Condifence Intervals
#******************************For Multiple varibales

#Democrats                                                              #Set the age between:
x.out2a <- setx(z.out, independent = 0, republican = 0, born_christian = 0, age = 18:94)

#Republicans
x.out2b <- setx(z.out, independent = 0, republican = 1, born_christian = 0, age = 18:94)

s.out2 <- sim(z.out, x = x.out2b, x1 = x.out2a)


plot(s.out2)


ci.plot(s.out2, xlab = "Age", ylab = "Predicted Probability of Support", 
        main = "Effect of Partisanship and Age", leg=NULL, ci=c(95))
text(67, .65, "Democrats", col="red")
text(40, .45, "Republicans", col="blue")