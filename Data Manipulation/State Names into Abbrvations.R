
#Change state names into abbrvations


#*********Raw Code
State_Dictonary <- data.frame(state.abb, state.name)

for(n in levels(factor(State_Dictonary$state.name))){
  for(i in levels(factor(CCES.2$State))){
    if(i == n ){
      
      CCES.2$State_abb[CCES.2$State == i] <- State_Dictonary$state.abb[State_Dictonary$state.name == i]
      
    }
  }
}

table(CCES.2$State_abb, CCES.2$State)

