#######  Monte Hall's Lets Make a Deal Problem ##############
#Should you switch doors?

sims <- 1000; WinNoSwitch <- 0; WinSwitch <- 0; doors <- c(1,2,3)
for(i in 1:sims){
  WinDoor <- sample(doors, 1)
  choice <- sample(doors, 1)
  
  if(WinDoor == choice)
    WinNoSwitch <- WinNoSwitch + 1      #no switch
    doorsLeft <- doors[doors != choice] #switch
    
  if(any(doorsLeft == WinDoor))
    WinSwitch <- WinSwitch +1 
  
}

cat("Prob(Car | no switch) = ", WinNoSwitch/sims)
cat("Prob(Car | switch) = ", WinSwitch/sims)


############# The Birthday Problem ###################
#Given a room with 24 randomly selected people, what is the probability that at least two have the same birthday?
sims <- 1000
people <- 24
alldays <- seq(1,365,1)
sameday <- 0
for(i in 1:sims){
  room <- sample(alldays, people, replace = T)
  if(length(unique(room)) < people) #same birthday
  sameday <- sameday +1
}

cat("Probability >=2:", sameday/sims, "\\n")