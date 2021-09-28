######################## Prelude ###############################

rm(list=ls(all=TRUE))

###################### Upload Data #############################

options(stringsAsFactors = FALSE)
options(scipen = 3)


#Set Working Directory
setwd("C:/Users/Sharif/OneDrive/University of California, Davis/Second Year/Fall Quarter/American Political Behavior/Final Project/Working Directory")

#Upload Data
ANES2016.Pure <- read.dta("anes_timeseries_2016_Stata12.dta")

ANES2016 <- ANES2016.Pure

########################## Rename Variables #################

#Varaibles Of Interest
ANES2016.Traits <- with(ANES2016, data.frame(
                          #Valence Characterisitsc 
                          V161159, #PREs Dem cand trait strong leadership
                          V161160, #PREs Dem cand trait really cares
                          V161161, #PREs Dem cand trait knowledgeable
                          V161162, #PREs Dem cand trait honest
                          V161163, #PREs Dem cand trait speaks mind
                          V161164, #PREs Rep cand trait strong leadership
                          V161165, #PREs Rep cand trait really cares
                          V161166, #PREs Rep cand trait knowledgeable
                          V161167, #PREs Rep cand trait honest
                          V161168, #PREs Rep cand trait speaks mind
                          V161169, #PREs Dem cand even-tempered
                          V161170, #PREs Rep cand even-tempered
                          
                          #Party ID
                          V161158x, #Pre: Numeric PRE: SUMMARY - Party ID
                          
                          #President - Vote Choice 
                          V162034a, #POST: For whom did R vote for President
                          V162035, #POST: Preference strong for Pres cand for whom R voted
                          
                          #Congress - Vote Choice
                          V162039, #POST: Did R vote for U.S. House of Representatives
                          V162046, #POST: Did R vote for U.S. Senate
                          
                          #PRE: TRUST IN GOVENRMENT 
                          V161215, #PRE: REV How often trust govt in Wash to do what is right
                          V161216, #PRE: Govt run by a few big interests or for benefit of all
                          V161217, #PRE: Does government waste much tax money
                          V161218, #PRE: How many in government are corrupt
                          V161219, #PRE: How often can people be trusted
                          V161220, #PRE: Elections make govt pay attention
                          
                          #POST: TRUST IN GOVENERNMENT
                          V162259, #POST: Compromise in politics is selling out on one's principles
                          V162260, #POST: Most politicians do not care about the people
                          V162261, #POST: Most politicians are trustworty
                          V162262, #POST: Politicians are the main problem in the U.S.
                          V162263, #POST: Strong leader is good for U.S. even if bends rules to get things done
                          V162264, #POST: People not politicians should make most important policy decisions
                          V162265, #POST: Most politicians only care about interests of rich and powerful
                          
                          #Control Variables*************
                          #Age
                          V161267,  #PRE: Respondent age
                          
                          #Education
                          V161270,  #PRE: Highest level of Education
                          
                          #Income
                          V161361x, #PRE FTF CASI/WEB: Pre income summary
                          V161308x, #PRE: SUMMARY - R self-identified social class
                          
                          #Race
                          V161310x, #PRE: SUMMARY - R self-identified race
                          
                          #Gender
                          V161002,  #PRE: FTF ONLY: INTERVIEWER : Is R male or female (Observation)
                          V161342,  #PRE FTF CASI / WEB: R self-identified gender
                          
                          #Religion
                          V161264x, #Numeric PRE: SUMMARY - Full religion summary
                          
                          #Political Sophication
                          V161003, #PRE: How often does R pay attn to politics and elections
                          V161004, #PRE: How interested in following campaigns
                          V161005, #PRE: Did R vote for President in 2012
                          V161006, #PRE: Recall of last (2012) Presidential vote choice
                          
                          #Union Member
                          V161302,  #PRE: Anyone in HH belong to labor union
                          
                          #Presidental Approval
                          V161082x,
                          
                          #Ideological Self Placement
                          V161126,
                          
                          #Clinton Ideology Placement
                          V161128,
                    
                          #Trump Ideology Placement
                          V161129, 
                          
                          #Economic Evaluations
                          V162280, 
                          
                          #Policy Question*******************
                             #1-7
                          V161196x, #PRE: SUMMARY - Build wall with Mexico
                          V161195x, #PRE: SUMMARY - Children brought illegally
                          V161214x, #PRE: SUMMARY - Allow Syrian refugees
                          V161225x, #PRE: SUMMARY - Govt action about rising temperatures
                          V161226x, #PRE: SUMMARY - require employers to offer paid leave to new parents
                          V161227x, #PRE: SUMMARY - Services to same sex couples
                          V161228x, #PRE: SUMMARY - Transgender policy
                          V161204x, #PRE: SUMMARY - Favor or oppose affirmative action in universities
                         
                              #1-4
                          V161229x, #PRE: SUMMARY - Laws to protect gays and lesbians against job discrim
                          V161233x, #PRE: SUMMARY - Favor or oppose death penalty
                          
                          # 1-3
                          V161187,   #PRE: Should fed govt make it more difficult to buy a gun
                              
                          #1-2
                          V161221,  #PRE: Is global warming happening or not
                          
                          
                          #Federal Budget Question *********
                          V161205, #PRE: Federal Budget Spending: Social Security
                          V161206, #PRE: Federal Budget Spending: public schools
                          V161207, #PRE: Federal Budget Spending: science and technology
                          V161208, #PRE: Federal Budget Spending: dealing with crime
                          V161209, #PRE: Federal Budget Spending: welfare programs
                          V161210, #PRE: Federal Budget Spending: child care
                          V161211, #PRE: Federal Budget Spending: aid to the poor
                          V161212, #PRE: Federal Budget Spending: protecting the environment
                          
                          #Weights***************
                          V160102,
                          
                           #Possible Instramental Variables***************
                          V162104,  #POST: Feeling thermometer: CONGRESS
                          V161080x, #PRE: SUMMARY - Approval/disapproval Congress handling job
                          V162102   #POST: Feeling thermometer: THE U.S. SUPREME COURT
  ))


table(ANES2016.Traits$V161159)
nrow(ANES2016.Traits)
table(ANES2016.Traits$V161160)
table(ANES2016.Traits$V162039)

nrow(subset(ANES2016.Traits, V161159 != "-9. Refused" & 
                             V161159 !="-8. Don't know (FTF only)" ))

##################### ***PRESIDENTAL VALENCE*** #####################
##################### Changing Non-Responce to Missing ###############

Traitnames<- c("V161159", "V161160","V161161", "V161162",
                "V161163",
                "V161164", 
                "V161165", 
                "V161166", 
                "V161167", 
                "V161168", 
                "V161169", 
                "V161170")

for(i in Traitnames) {
  ANES2016.Traits[i][ANES2016.Traits[i] == "-9. Refused"] <- NA
  ANES2016.Traits[i][ANES2016.Traits[i] == "-8. Don't know (FTF only)"] <- NA
  print(table(ANES2016.Traits[i]))
}

################## Changing Factors Into Numbers #################
ANES2016.Traits.2 <- ANES2016.Traits
nrow(ANES2016.Traits.2)

Traitnames<- c("V161159", "V161160","V161161", "V161162",
               "V161163",
               "V161164", 
               "V161165", 
               "V161166", 
               "V161167", 
               "V161168", 
               "V161169", 
               "V161170")
ANES2016.Traits.2[Traitnames] <- sapply(ANES2016.Traits.2[Traitnames],as.numeric)
sapply(ANES2016.Traits.2, class)

table(ANES2016.Traits$V161159)
table(ANES2016.Traits.2$V161159)

#Scaled from 3 - 7: Good to Bad

################# Recode Numeric Varaibles 1-5: Bad to Good ##############
Traitnames<- c("V161159", "V161160","V161161", "V161162",
               "V161163",
               "V161164", 
               "V161165", 
               "V161166", 
               "V161167", 
               "V161168", 
               "V161169", 
               "V161170")

#Formula
7*-1
-7 + 8

(7*-1) +8


ANES2016.Traits.3a <- ANES2016.Traits.2

for(i in Traitnames) {
  ANES2016.Traits.3a[i] <- ((ANES2016.Traits.2[i] * -1) + 8)
  print(table(ANES2016.Traits.3a[i]))
}

#Step 1
table(ANES2016.Traits$V161159)

#Step 2
table(ANES2016.Traits.2$V161159)

#Step 3
table(ANES2016.Traits.3a$V161159)

#Perfect! All Check Out!!

################# ***VOTE FOR PRESIDENT*** #################
ANES2016.Traits.3b <- ANES2016.Traits.3a

table(ANES2016.Traits.3b$V162034a)

#Subset Only Donald Trump and Hilary Clinton
ANES2016.Traits.3c <- subset(ANES2016.Traits.3b, V162034a == 1 | V162034a == 2)

table(ANES2016.Traits.3c$V162034a)

#Recode Presidental Vote to 0 and 1
#0 = Vote for Hilary
#1 = Vote for Trump

#Lost 4271-2468 = 1803 Observations becasue of 
#People who did not vote in the Presidental Election

ANES2016.Traits.3c$V162034a <- with(ANES2016.Traits.3c, V162034a - 1)
table(ANES2016.Traits.3c$V162034a)

#Test: Vote for Trump, Rep favorable quality - Check!!!
cor(ANES2016.Traits.3c$V162034a,ANES2016.Traits.3c$V161165, use = "complete")

ANES2016.Traits.3 <- ANES2016.Traits.3c
################### ***Post: TRUST IN GOVERNMENT*** #################
#POST: TRUST IN GOVENERNMENT
#V162259, #POST: Compromise in politics is selling out on one's principles
#V162260, #POST: Most politicians do not care about the people
#V162261, #POST: Most politicians are trustworty
#V162262, #POST: Politicians are the main problem in the U.S.
#V162263, #POST: Strong leader is good for U.S. even if bends rules to get things done
#V162264, #POST: People not politicians should make most important policy decisions
#V162265 #POST: Most politicians only care about interests of rich and powerful

table(ANES2016.Traits.3$V162259) 
table(ANES2016.Traits.3$V162260) 
table(ANES2016.Traits.3$V162261) 
table(ANES2016.Traits.3$V162262) 
table(ANES2016.Traits.3$V162263) 
table(ANES2016.Traits.3$V162264) 
table(ANES2016.Traits.3$V162265)

##################### Changing Non-Responce to Missing ###############

Traitnames<- c("V162259", 
               "V162260", 
               "V162261", 
               "V162262", 
               "V162263", 
               "V162264", 
               "V162265",
               "V161215",
               "V161218",
               "V161219")

for(i in Traitnames) {
  ANES2016.Traits.3[i][ANES2016.Traits.3[i] == "-9. Refused"] <- NA
  ANES2016.Traits.3[i][ANES2016.Traits.3[i] == "-8. Don't know"] <- NA
  ANES2016.Traits.3[i][ANES2016.Traits.3[i] == " -6. No post-election interview"] <- NA
  ANES2016.Traits.3[i][ANES2016.Traits.3[i] == "-7. No post data, deleted due to incomplete IW"] <- NA
    print(table(ANES2016.Traits.3[i]))
}

table(is.na(ANES2016.Traits.3$V162259))
#Check, NA's Perfect!!!


################## Changing Factors Into Numbers #################
ANES2016.Traits.4 <- ANES2016.Traits.3

Traitnames<- c("V162259", 
               "V162260", 
               "V162261", 
               "V162262", 
               "V162263", 
               "V162264", 
               "V162265",
               "V161215",
               "V161218",
               "V161219")

ANES2016.Traits.4[Traitnames] <- sapply(ANES2016.Traits.4[Traitnames],as.numeric)
sapply(ANES2016.Traits.4, class)

table(ANES2016.Traits.3$V162259)
table(ANES2016.Traits.4$V162259)

#Perfect, Changed to numbers that correspond! - Check!


#Scaled from 5 - 9: Good to Bad

################# Recode Numeric Varaibles 1-5: Bad to Good ##############
Traitnames<- c("V162259", 
               "V162260", 
               "V162261", 
               "V162262", 
               "V162263", 
               "V162264", 
               "V162265",
               "V161218",
               "V161219")

#Formula
9*-1
-9 + 10

(9*-1) +10


ANES2016.Traits.5a <- ANES2016.Traits.4

for(i in Traitnames) {
  ANES2016.Traits.5a[i] <- ((ANES2016.Traits.4[i] * -1) + 10)
  print(table(ANES2016.Traits.5a[i]))
}

#Step 1
table(ANES2016.Traits.3$V162259)

#Step 2
table(ANES2016.Traits.4$V162259)

#Step 3
table(ANES2016.Traits.5a$V162259)

#Perfect! All Check Out!!

######################### ***CORRELATION MATRIX*** ##############

#Vote For President
table(ANES2016.Traits.5a$V162034a)

#Trust in Government Varables 
table(ANES2016.Traits.5a$V162259) 
table(ANES2016.Traits.5a$V162260) 
table(ANES2016.Traits.5a$V162261) 
table(ANES2016.Traits.5a$V162262) 
table(ANES2016.Traits.5a$V162263) 
table(ANES2016.Traits.5a$V162264) 
table(ANES2016.Traits.5a$V162265)

###################### ***RENAME KEY VARIABLES***#################

ANES2016.Traits.5 <- rename(ANES2016.Traits.5a, 
                      PresVote = V162034a,
                      Comp = V162259, 
                      NoCare = V162260, 
                      AreTrust = V162261, 
                      PProb = V162262, 
                      StrongLead= V162263, 
                      PvPol= V162264, 
                      PolRandP= V162265,
                      Pre.Trust.Gov = V161215,
                      Pre.Corrupt =  V161218,
                      Pre.Trust.People = V161219)

colnames(ANES2016.Traits.5)

table(ANES2016.Traits.5$NoCare)
CrossTable(ANES2016.Traits.5$NoCare)

####################### Correlation Table ###############
TrustInGOV.Vote.Corr <- with(ANES2016.Traits.5, data.frame(PresVote,    
                                                           Comp,       
                                                           NoCare,      
                                                           AreTrust,    
                                                           PProb,       
                                                           StrongLead,  
                                                           PvPol,       
                                                           PolRandP))

rcorr(as.matrix(TrustInGOV.Vote.Corr))

####################### ***Histograms*** #######################

GovTrust <- c("Comp",
              "NoCare",
              "AreTrust",
              "PProb",
              "StrongLead",
              "PvPol",
              "PolRandP")

Trust.Hist <- list()

for(i in GovTrust) {
  
  Trust.Hist[i]<- hist(ANES2016.Traits.5[i])
  
  
}

Trust.Hist

##################### ***SCATTERPLOT*** ###############
GovTrust <- c("Comp",
              "NoCare",
              "AreTrust",
              "PProb",
              "StrongLead",
              "PvPol" ,
              "PolRandP")

Presvote.Trust.Scat <- list()

for(i in GovTrust) {
  
  Presvote.Trust.Scat[[i]]<- scatterplot(ANES2016.Traits.5[i], ANES2016.Traits.5$PresVote)

  
  
}


scatterplot(ANES2016.Traits.5$PresVote~ ANES2016.Traits.5$Comp)
Presvote.Trust.Scat

head(ANES2016.Traits.5)

################## ***Correlation Plot*** ############
TrustInGOV.Vote.Corr <- with(ANES2016.Traits.6, data.frame(PresVote,    
                                                           Comp,       
                                                           NoCare,      
                                                           AreTrust,    
                                                           PProb,       
                                                           StrongLead,  
                                                           PvPol,       
                                                           PolRandP))

TrustInGOV.Vote.Corr.Plot <- cor(TrustInGOV.Vote.Corr, use = "complete",  method =  "spearman")


corrplot(TrustInGOV.Vote.Corr.Plot, method = "number")

################### ***MEAN AND SD*** ################

GovTrust <- c("Comp",
              "NoCare",
              "AreTrust",
              "PProb",
              "StrongLead",
              "PvPol" ,
              "PolRandP")

Trust.Mean <- list()
Trust.SD <- list()
rm(Trust.Mean)
rm(Trust.SD)

for(i in GovTrust) {
  
  Trust.Mean <- mean(ANES2016.Traits.5[i], na.rm = TRUE)

  Trust.SD <- sd(ANES2016.Traits.5[i], na.rm = TRUE)

}

mean(ANES2016.Traits.5$Comp, na.rm = TRUE)
mean(ANES2016.Traits.5$NoCare, na.rm = TRUE)
mean(ANES2016.Traits.5$AreTrust, na.rm = TRUE)
mean(ANES2016.Traits.5$PProb, na.rm = TRUE)
mean(ANES2016.Traits.5$StrongLead, na.rm = TRUE)
mean(ANES2016.Traits.5$PvPol, na.rm = TRUE)
mean(ANES2016.Traits.5$PolRandP, na.rm = TRUE)

sd(ANES2016.Traits.5$Comp, na.rm = TRUE)
sd(ANES2016.Traits.5$NoCare, na.rm = TRUE)
sd(ANES2016.Traits.5$AreTrust, na.rm = TRUE)
sd(ANES2016.Traits.5$PProb, na.rm = TRUE)
sd(ANES2016.Traits.5$StrongLead, na.rm = TRUE)
sd(ANES2016.Traits.5$PvPol, na.rm = TRUE)
sd(ANES2016.Traits.5$PolRandP, na.rm = TRUE)


################### ***PARTY ID *** ##################

table(ANES2016.Traits.5$V161158x)

# Make -9 and -8 NA,
                    
ANES2016.Traits.5$V161158x <- ifelse(ANES2016.Traits.5$V161158x == -9 | ANES2016.Traits.5$V161158x == -8, NA, ANES2016.Traits.5$V161158x)
  
#Change Dataset from 5 to 6

ANES2016.Traits.6 <- ANES2016.Traits.5

table(ANES2016.Traits.6$V161158x)

  
#Combine Democrats 
ANES2016.Traits.6$PartyID <- ifelse(ANES2016.Traits.6$V161158x <= 3, "Democrat", ANES2016.Traits.6$V161158x)

table(ANES2016.Traits.6$PartyID)
table(ANES2016.Traits.6$V161158x)
625 + 324 + 267

#1216 = 1216

#Combine Republicans
ANES2016.Traits.6$PartyID <- ifelse(ANES2016.Traits.6$V161158x >= 5, "Republican", ANES2016.Traits.6$PartyID)

table(ANES2016.Traits.6$PartyID)
table(ANES2016.Traits.6$V161158x)
289+280+495 
#1064 = 1064

#Recode Independents 
ANES2016.Traits.6$PartyID <- ifelse(ANES2016.Traits.6$V161158x == 4, "Independent", ANES2016.Traits.6$PartyID)

table(ANES2016.Traits.6$PartyID)
table(ANES2016.Traits.6$V161158x)
#184 = 184

#Final Check
#Party ID on 3 Point Scale
table(ANES2016.Traits.6$PartyID)

#Rename Party ID on 7pt Scale
ANES2016.Traits.6 <- rename(ANES2016.Traits.6, 
                            PartyID.7p = V161158x)

#Final Check
#Party ID on 3 Point Scale
table(ANES2016.Traits.6$PartyID)
table(ANES2016.Traits.6$PartyID.7p)

#Relevel
ANES2016.Traits.6$PartyID <- factor(ANES2016.Traits.6$PartyID,
                              levels = c("Democrat",
                                          "Independent",
                                          "Republican" 
                              ))

#Create the Partisan Variable
ANES2016.Traits.6$Partisan <- as.factor(ifelse(ANES2016.Traits.6$PartyID == "Democrat" |
                                     ANES2016.Traits.6$PartyID == "Republican",
                                     "Partisan", "Independent"))
table(ANES2016.Traits.6$PartyID)

table(ANES2016.Traits.6$Partisan)

#Relevel
levels(ANES2016.Traits.6$Partisan)

ANES2016.Traits.6$Partisan<- factor(ANES2016.Traits.6$Partisan,
                                    levels = c("Partisan",
                                               "Independent"
                                    ))
levels(ANES2016.Traits.6$Partisan)

#################### ***Table of Dependent Variable ***###############
table(ANES2016.Traits$V162034a)
table(ANES2016.Traits.3$V162034a)
table(ANES2016.Traits.6$PresVote)
CrossTable(ANES2016.Traits.6$PresVote)

1290+1178
195+622+958+28

#################### ***Cross Table*** ########################

CrossTable(ANES2016.Traits.6$Comp, ANES2016.Traits.6$PresVote, prop.chisq=F, prop.t=F)
CrossTable(ANES2016.Traits.6$NoCare, ANES2016.Traits.6$PresVote, prop.chisq=F, prop.t=F)
CrossTable(ANES2016.Traits.6$AreTrust, ANES2016.Traits.6$PresVote, prop.chisq=F, prop.t=F)
CrossTable(ANES2016.Traits.6$PProb, ANES2016.Traits.6$PresVote, prop.chisq=F, prop.t=F)
CrossTable(ANES2016.Traits.6$StrongLead, ANES2016.Traits.6$PresVote, prop.chisq=F, prop.t=F)
CrossTable(ANES2016.Traits.6$PvPol, ANES2016.Traits.6$PresVote, prop.chisq=F, prop.t=F)
CrossTable(ANES2016.Traits.6$PolRandP, ANES2016.Traits.6$PresVote, prop.chisq=F, prop.t=F)



##################### ***OLS: 1v1 Comparision ***#######################


OLS.Comp <- lm(PresVote ~ Comp, data = ANES2016.Traits.6)
summary(OLS.Comp)

OLS.NoCare <- lm(PresVote ~ NoCare, data = ANES2016.Traits.6)
summary(OLS.NoCare)

OLS.AreTrust <- lm(PresVote ~ AreTrust, data = ANES2016.Traits.6)
summary(OLS.AreTrust)

OLS.PProb <- lm(PresVote ~ PProb, data = ANES2016.Traits.6)
summary(OLS.PProb)

OLS.StrongLead <- lm(PresVote ~ StrongLead, data = ANES2016.Traits.6)
summary(OLS.StrongLead)

OLS.PvPol<- lm(PresVote ~ PvPol, data = ANES2016.Traits.6)
summary(OLS.PvPol)

OLS.PolRandP <- lm(PresVote ~ PolRandP, data = ANES2016.Traits.6)
summary(OLS.PolRandP)



GovTrust <- c("Comp",
              "NoCare",
              "AreTrust",
              "PProb",
              "StrongLead",
              "PvPol",
              "PolRandP")


OLS.Trust <- list()

for(i in GovTrust) {
  
  OLS.Trust[i] <- lm(ANES2016.Traits.6$PresVote ~ 
                        ANES2016.Traits.6[i])
  
  summary(OLS.Trust[i])
  

}

##################### ***OLS: PARTY ID CONTROL ***#######################

OLS.Comp <- lm(PresVote ~ Comp + PartyID, data = ANES2016.Traits.6)
summary(OLS.Comp)

OLS.NoCare <- lm(PresVote ~ NoCare + PartyID, data = ANES2016.Traits.6)
summary(OLS.NoCare)

OLS.AreTrust <- lm(PresVote ~ AreTrust + PartyID, data = ANES2016.Traits.6)
summary(OLS.AreTrust)

OLS.PProb <- lm(PresVote ~ PProb + PartyID, data = ANES2016.Traits.6)
summary(OLS.PProb)

OLS.StrongLead <- lm(PresVote ~ StrongLead + PartyID, data = ANES2016.Traits.6)
summary(OLS.StrongLead)

OLS.PvPol<- lm(PresVote ~ PvPol + PartyID, data = ANES2016.Traits.6)
summary(OLS.PvPol)

OLS.PolRandP <- lm(PresVote ~ PolRandP + PartyID, data = ANES2016.Traits.6)
summary(OLS.PolRandP)

##################### ***OLS: PARTY ID INTERACTION ***#######################

ANES2016.Traits.6$PartyID <- as.factor(ANES2016.Traits.6$PartyID)
ANES2016.Traits.6$Comp.f <- as.factor(ANES2016.Traits.6$Comp)

OLS.Comp <- lm(PresVote ~ Comp.f * PartyID, data = ANES2016.Traits.6)
summary(OLS.Comp)

vif(OLS.Comp)

#Effects Plot

plot(
  effect("Comp.f * PartyID", 
         OLS.Comp
  ), 
  xlab = "Compromise in politics is selling out on one's principles",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

OLS.NoCare <- lm(PresVote ~ NoCare * PartyID, data = ANES2016.Traits.6)
summary(OLS.NoCare)

vif(OLS.NoCare)


#Effects Plot
plot(
  effect("NoCare * PartyID", 
         OLS.NoCare
  ), 
  xlab = "Most politicians do not care about the people",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

OLS.AreTrust <- lm(PresVote ~ AreTrust * PartyID, data = ANES2016.Traits.6)
summary(OLS.AreTrust)

vif(OLS.AreTrust)


#Effects Plot

plot(
  effect("AreTrust * PartyID", 
         OLS.AreTrust
  ), 
  xlab = "Most politicians are trustworthy",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

OLS.PProb <- lm(PresVote ~ PProb * PartyID, data = ANES2016.Traits.6)
summary(OLS.PProb)

vif(OLS.PProb)


#Effects Plot

plot(
  effect("PProb * PartyID", 
         OLS.PProb
  ), 
  xlab = "Politicians are the main problem in the U.S.",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

OLS.StrongLead <- lm(PresVote ~ StrongLead * PartyID, data = ANES2016.Traits.6)
summary(OLS.StrongLead)

vif(OLS.StrongLead)


#Effects Plot

plot(
  effect("StrongLead * PartyID", 
         OLS.StrongLead
  ), 
  xlab = "Strong leader is good for U.S. even if bends rules to get things done",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

OLS.PvPol<- lm(PresVote ~ PvPol * PartyID, data = ANES2016.Traits.6)
summary(OLS.PvPol)

vif(OLS.PvPol)


#Effects Plot

plot(
  effect("PvPol * PartyID", 
         OLS.PvPol
  ), 
  xlab = "People not politicians should make most important policy decisions",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

OLS.PolRandP <- lm(PresVote ~ PolRandP * PartyID, data = ANES2016.Traits.6)
summary(OLS.PolRandP)

vif(OLS.PolRandP)


#Effects Plot
plot(
  effect("PolRandP * PartyID", 
         OLS.PolRandP
  ), 
  xlab = "Most politicians only care about interests of rich and powerful",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)


################## *** FACTOR ANALYSIS *** ####################

library(psych)
library(GPArotation)

#Recode Trustworthy

#Subset the data with the Factors to be analysized

ANES2016.Trust <- with(ANES2016.Traits.6, data.frame(Comp,        #V162259, #POST: Compromise in politics is selling out on one's principles
                                                     NoCare,      #V162260, #POST: Most politicians do not care about the people
                                                     AreTrust,    #V162261, #POST: Most politicians are trustworty
                                                     PProb,       #POST: Politicians are the main problem in the U.S.
                                                     StrongLead,  #POST: Strong leader is good for U.S. even if bends rules to get things done
                                                     PvPol,       #POST: People not politicians should make most important policy decisions
                                                     PolRandP,    #POST: Most politicians only care about interests of rich and powerful
                                                     Pre.Trust.Gov # PRE: REV How often trust govt in Wash to do what is right
                                                      ))
head(ANES2016.Trust)


#Parallel Analysis
parallel <- fa.parallel(ANES2016.Trust, fm = 'minres', fa = 'fa')

#Factor Analysis

#One Factor
Onefactor <- fa(ANES2016.Trust,nfactors = 1,rotate = "oblimin",fm="minres")
print(Onefactor)

print(Onefactor$loadings,cutoff = 0.3)
fa.diagram(Onefactor)

#Two Factor*******************
Twofactor <- fa(ANES2016.Trust,nfactors = 2,rotate = "oblimin",fm="minres")
print(Twofactor)

print(Twofactor$loadings,cutoff = 0.3)
fa.diagram(Twofactor)

#Three Factors
threefactor <- fa(ANES2016.Trust,nfactors = 3,rotate = "oblimin",fm="minres")
print(threefactor)

print(threefactor$loadings,cutoff = 0.3)

fa.diagram(threefactor)

threefactor$values	


#Four Factors
Fourfactor <- fa(ANES2016.Trust,nfactors = 4,rotate = "oblimin",fm="minres")
print(Fourfactor)

print(Fourfactor$loadings,cutoff = 0.3)

fa.diagram(Fourfactor)


###################### ***Does Trust in Government Predict Party ID?*** ################

####################### Correlation Table ###############

ANES2016.Traits.6$PartyID.N <- as.numeric(ANES2016.Traits.6$PartyID)

table(ANES2016.Traits.6$PartyID)
table(ANES2016.Traits.6$PartyID.N)

TrustInGOV.PID.Corr <- with(ANES2016.Traits.6, data.frame(PartyID.N,    
                                                           Comp,       
                                                           NoCare,      
                                                           AreTrust,    
                                                           PProb,       
                                                           StrongLead,  
                                                           PvPol,       
                                                           PolRandP))
rcorr(as.matrix(TrustInGOV.PID.Corr))

#################### ***DISTRUST VARIABLE: Combine Together Distruct Varibale*** ##################

ANES2016.7 <- ANES2016.Traits.6

#****************** Recode Trust Variable to negative From Strongly Agree (5) to Strongly Disagree (5) ********************
colnames(ANES2016.7)

table(ANES2016.Traits.6$AreTrust)

(5* -1) + 6

ANES2016.7$AreTrust <- with(ANES2016.7, (AreTrust * -1) + 6)

table(ANES2016.Traits.6$AreTrust)
table(ANES2016.7$AreTrust)

#*************** Rerun Factor Analysis *************
#Subset the data with the Factors to be analysized

ANES2016.Trust <- with(ANES2016.7, data.frame(Comp,        #V162259, #POST: Compromise in politics is selling out on one's principles
                                                     NoCare,      #V162260, #POST: Most politicians do not care about the people
                                                     AreTrust,    #V162261, #POST: Most politicians are trustworty
                                                     PProb,       #POST: Politicians are the main problem in the U.S.
                                                     StrongLead,  #POST: Strong leader is good for U.S. even if bends rules to get things done
                                                     PvPol,       #POST: People not politicians should make most important policy decisions
                                                     PolRandP    #POST: Most politicians only care about interests of rich and powerful
))
head(ANES2016.Trust)

#Parallel Analysis
parallel <- fa.parallel(ANES2016.Trust, fm = 'minres', fa = 'fa')


#Three Factors
threefactor <- fa(ANES2016.Trust,nfactors = 3,rotate = "oblimin",fm="minres")
print(threefactor)

print(threefactor$loadings,cutoff = 0.3)

fa.diagram(threefactor)

#Measure the Cronbach Alpha between my Distrust Variables
Distrust.cron <- with(ANES2016.7, data.frame(NoCare, AreTrust, PProb, PolRandP))
cronbach(Distrust.cron)


#Combines into three dimentions.

#***************** Row Mean *******************
ANES2016.7$Distrust <- rowMeans(ANES2016.7 [,c("NoCare",
                                                  "AreTrust",
                                                  "PProb",
                                                  "PolRandP"
)])

######################## Descriptive Stats: Distruct Variable ##################
summary(ANES2016.7$Distrust)
sd(ANES2016.7$Distrust, na.rm = T)
table(ANES2016.7$Distrust)

hist(ANES2016.7$Distrust, breaks = 10)

######################## Distrust and Presdential Vote ########################
#Correlation
cor(ANES2016.7$PresVote,ANES2016.7$Distrust,  use = "complete")

#Cross table
CrossTable(ANES2016.7$Distrust, ANES2016.7$PresVote)

#scatterplot
scatterplot(ANES2016.7$PresVote ~ ANES2016.7$Distrust)

####################### OLS: Distrust and Presdential Vote ##############
glm()

#1v1 OLS
OLS.Distrust <- lm(PresVote ~ Distrust, data = ANES2016.7)
summary(OLS.Distrust)
vif(OLS.Distrust)
ncvTest(OLS.Distrust)


plot(
  effect("Distrust", 
         OLS.Distrust
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

#OLS with PArty ID as Control
OLS.Distrust <- lm(PresVote ~ Distrust + PartyID, data = ANES2016.7)
summary(OLS.Distrust)
vif(OLS.Distrust)
ncvTest(OLS.Distrust)



plot(
  effect("Distrust", 
         OLS.Distrust
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

#OLS with PArty ID as Interation 
OLS.Distrust <- lm(PresVote ~ Distrust * PartyID, data = ANES2016.7)
summary(OLS.Distrust)
vif(OLS.Distrust)
ncvTest(OLS.Distrust)



plot(
  effect("Distrust * PartyID", 
         OLS.Distrust
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)


################## *** CONTROL VARIABLES *** ######################

################ Control Variables: Rename Varables ###################
ANES2016.7 <- rename(ANES2016.7, 
                       Age = V161267, 
                       Education = V161270,
                       Income = V161361x,
                       Race = V161310x,
                       Gender = V161342,
                       Union = V161302,
                       PS.AttPol = V161003,
                       PS.Camp = V161004,
                       PS.Vote2012 = V161005,
                       PS.VoteChoice2012 = V161006,
                       Econ = V162280,
                       Pres.App = V161082x,
                       Trump.ID = V161129,
                       HC.ID = V161128,
                       Self.ID = V161126
                       )


################ Control Variables: Descriptive Stats #####################
Controls <- c("Race",
              "Gender",
              "PS.AttPol", 
              "PS.Camp", 
              "PS.Vote2012",
              "PS.VoteChoice2012",
              "Education",
              "Union",
              "Age", 
              "Income",
              "Pres.App",
              "Trump.ID",
              "HC.ID",
              "Econ",
              "Self.ID"
            )

#**************** Summary Stats
for(i in Controls) {
  
  print(summary(ANES2016.7[i]))
  
}

#*************** Table Stats
for(i in Controls) {
  
  print(table(ANES2016.7[i]))
  
}

################ Control Variables: Turn Unuseable Codes to Missing ############

ANES2016.8 <- ANES2016.7

for(i in Controls) {
  
  ANES2016.8[i][ANES2016.8[i] == "-9. Refused"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-9. Missing"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-9. RF (-9) in V161196 or V161196a"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-9"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-1"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "95. Other SPECIFY"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "3. Other"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "5. Other SPECIFY"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "6. Other specify - specified as:  Did not vote/did not vote for President in 2012"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-8. Don't know"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-8. DK (-8) in V161196 or V161196a (FTF only)"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-1. Inap, 2,-8,-9 in V161005"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-5. Interview breakoff (sufficient partial IW)"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-8. Don't know (FTF only)"] <- NA
  ANES2016.8[i][ANES2016.8[i] == " -6. No post-election interview"] <- NA
  ANES2016.8[i][ANES2016.8[i] == "-7. No post data, deleted due to incomplete IW"] <- NA
  print(table(ANES2016.8[i]))
  
}

#***********Check for NA's ************

table(is.na(ANES2016.8$Race))
table(is.na(ANES2016.8$Gender))
table(is.na(ANES2016.8$PS.AttPol)) 
table(is.na(ANES2016.8$PS.Camp)) 
table(is.na(ANES2016.8$PS.Vote2012))
table(is.na(ANES2016.8$PS.VoteChoice2012))
table(is.na(ANES2016.8$Education))
table(is.na(ANES2016.8$Union))
table(is.na(ANES2016.8$Age)) 
table(is.na(ANES2016.8$Income))
table(is.na(ANES2016.8$Econ))

################# Control Variables: Fix Education Variable ################

#Education
ANES2016.8$Education[ANES2016.8$Education == "90. Other specify given as: high school graduate"] <- "9. High school graduate- high school diploma or equivalent (for example: GED)"

table(ANES2016.8$Education)

################# Control Variables: MASTER TABLE ####################

#Ordinal
table(ANES2016.8$PS.AttPol) 
table(ANES2016.8$PS.Camp) 

#Dicodimious
table(ANES2016.8$Union)
table(ANES2016.8$Gender)
table(ANES2016.8$PS.Vote2012)

#Continutios categorical
table(ANES2016.8$Education)
table(ANES2016.8$Income)

#Categorical
table(ANES2016.8$Race)

#Continious
table(ANES2016.8$Age) 

#Laged Vote Choice
table(ANES2016.8$PS.VoteChoice2012)

################# **Control Variables: ORDINAL VARIABLES Code Naturally (Strongly Disagree to Strongly Agree) ** ###################
ANES2016.9 <- ANES2016.8

#Ordinal Table
table(ANES2016.8$PS.AttPol) 
table(ANES2016.8$PS.Camp) 

#***************Make Numeric

ANES2016.9$PS.AttPol <- as.numeric(ANES2016.9$PS.AttPol)
ANES2016.9$PS.Camp <- as.numeric(ANES2016.9$PS.Camp)

#New Table
table(ANES2016.9$PS.AttPol) 
table(ANES2016.9$PS.Camp)

#************Flip the Coding
7*-1 + 8
ANES2016.9$PS.AttPol <- with(ANES2016.9, PS.AttPol *-1 + 8)

5*-1+6

ANES2016.9$PS.Camp <- with(ANES2016.9, PS.Camp *-1 + 6)

#New Table
table(ANES2016.9$PS.AttPol) 
table(ANES2016.9$PS.Camp)

#********************Put Factors Back in

#Attention to politics 
ANES2016.9$PS.AttPol[ANES2016.9$PS.AttPol == 1] <- "Never"
ANES2016.9$PS.AttPol[ANES2016.9$PS.AttPol == 2] <- "Some of the time"
ANES2016.9$PS.AttPol[ANES2016.9$PS.AttPol == 3] <- "About half the time"
ANES2016.9$PS.AttPol[ANES2016.9$PS.AttPol == 4] <- "Most of the time"
ANES2016.9$PS.AttPol[ANES2016.9$PS.AttPol == 5] <- "Always"

#Campaign
ANES2016.9$PS.Camp[ANES2016.9$PS.Camp == 1] <- "Not much interested"
ANES2016.9$PS.Camp[ANES2016.9$PS.Camp == 2] <- "Somewhat interested"
ANES2016.9$PS.Camp[ANES2016.9$PS.Camp == 3] <- "Very much interested"


#***************Make into a factor

#Attention to politics 
ANES2016.9$PS.AttPol <- as.factor(ANES2016.9$PS.AttPol)

#Campaign
ANES2016.9$PS.Camp <- as.factor(ANES2016.9$PS.Camp)

#******************Relevel Data

#Attention to politics 
levels(ANES2016.9$PS.AttPol)
ANES2016.9$PS.AttPol <- factor(ANES2016.9$PS.AttPol,
                               levels = c( "Never",
                                           "Some of the time",
                                           "About half the time",
                                           "Most of the time",
                                           "Always"
                                          ))
#Campaign
levels(ANES2016.9$PS.Camp)
ANES2016.9$PS.Camp <- factor(ANES2016.9$PS.Camp,
                             levels = c( "Not much interested",
                                         "Somewhat interested",
                                         "Very much interested"
                             ))
#******************** Table
#Attention to politics 
table(ANES2016.8$PS.AttPol)
table(ANES2016.9$PS.AttPol)

#Campaign
table(ANES2016.8$PS.Camp)
table(ANES2016.9$PS.Camp)

################# **Control Variables: DICHOTOMOUS VARIABLES Code Naturally (Strongly Disagree to Strongly Agree) ** ###################

#table
table(ANES2016.9$Union)
table(ANES2016.9$Gender)
table(ANES2016.9$PS.Vote2012)

#Prepare Loop
DI.Variable <- c("Union",
                 "Gender",
                 "PS.Vote2012")

#Change Dataset
ANES2016.10 <- ANES2016.9

#***************Make Numeric

for (i in DI.Variable) {
  ANES2016.10[DI.Variable] <- sapply(ANES2016.10[DI.Variable],as.numeric)
  sapply(ANES2016.10, class)
  
}

table(ANES2016.10$Union)
table(ANES2016.10$Gender)
table(ANES2016.10$PS.Vote2012)


#*************** Recode ***************

for (i in DI.Variable) {
  ANES2016.10[i] <- ((ANES2016.10[i] *-1) + 4)
  print(table(ANES2016.10[i]))
  }

table(ANES2016.10$Union)
table(ANES2016.10$Gender)
table(ANES2016.10$PS.Vote2012)

table(ANES2016.9$Union)
table(ANES2016.9$Gender)
table(ANES2016.9$PS.Vote2012)

#********************Put Factors Back in
#Gender
ANES2016.10$Gender[ANES2016.10$Gender == 0] <- "Female"
ANES2016.10$Gender[ANES2016.10$Gender == 1] <- "Male"

#Union
ANES2016.10$Union[ANES2016.10$Union == 0] <- "No"
ANES2016.10$Union[ANES2016.10$Union == 1] <- "Yes"

#PS.Vote
ANES2016.10$PS.Vote2012[ANES2016.10$PS.Vote2012 == 0] <- "No"
ANES2016.10$PS.Vote2012[ANES2016.10$PS.Vote2012 == 1] <- "Yes"

#******************** Make Into Factor ************

ANES2016.10$Union <- as.factor(ANES2016.10$Union)
ANES2016.10$Gender <- as.factor(ANES2016.10$Gender)
ANES2016.10$PS.Vote2012 <- as.factor(ANES2016.10$PS.Vote2012)

levels(ANES2016.10$Union)
levels(ANES2016.10$Gender)
levels(ANES2016.10$PS.Vote2012)


#******************* Double Check **************
#Union
table(ANES2016.9$Union)
table(ANES2016.10$Union)

#Gender
table(ANES2016.9$Gender)
table(ANES2016.10$Gender)

#PS.Vote
table(ANES2016.9$PS.Vote2012)
table(ANES2016.10$PS.Vote2012)

################# **Control Variables: EDUCATION AND INCOME into Numeric ###################

#Table
table(ANES2016.10$Education)
table(ANES2016.10$Income)

EI <- c("Education", "Income")

#Change Data Set
ANES2016.11 <- ANES2016.10

#Make Numeric and Subtract by 3
for (i in EI) {
  ANES2016.11[i] <- sapply(ANES2016.11[i],as.numeric)
  sapply(ANES2016.11, class)
  
  ANES2016.11[i] <- (ANES2016.11[i] - 2)
  print(table(ANES2016.11[i]))
  
  
}


################# **Control Variables: RACE** ###################

# Drop and Unused Level
ANES2016.11$Race <- factor(ANES2016.11$Race)
levels(ANES2016.11$Race)
class(ANES2016.11$Race)

#Change to Numeric
ANES2016.11$Race <- as.numeric(ANES2016.11$Race)
table(ANES2016.11$Race)

#Recode Factors with Categoies 
ANES2016.11$Race[ANES2016.11$Race == 1] <- "White"
ANES2016.11$Race[ANES2016.11$Race == 2] <- "Black"
ANES2016.11$Race[ANES2016.11$Race == 3] <- "Asian"
ANES2016.11$Race[ANES2016.11$Race == 4] <- "Native American"
ANES2016.11$Race[ANES2016.11$Race == 5] <- "Hispanic"
ANES2016.11$Race[ANES2016.11$Race == 6] <- "Other or Mixed"

#Table to verify values
table(ANES2016.11$Race)
table(ANES2016.10$Race)
table(is.na(ANES2016.11$Race))

class(ANES2016.11$Race)

#Change into Facots
ANES2016.11$Race <- as.factor(ANES2016.11$Race)
levels(ANES2016.11$Race)

class(ANES2016.11$Race)

ANES2016.11$Race <- factor(ANES2016.11$Race,
                             levels = c( "White",
                                         "Black",
                                         "Asian",
                                         "Native American",
                                         "Hispanic",
                                         "Other or Mixed"
                             ))

levels(ANES2016.11$Race)


summary(ANES2016.11$Race)

######################## ***CONTROL VARIABLES: PRESIDENTAL APPROVAL *** ############
#Check coding direction
table(ANES2016.Traits$V161082x)

table(ANES2016.10$Pres.App)
#Looks likes it is coded from Strong Approve to Strong disapprove

#Recode it naturally (Strongly Disapprove to Strongly Approve)

ANES2016.11$Pres.App <- with(ANES2016.11, (Pres.App * -1) +5)

#double check
table(ANES2016.Traits$V161082x)
table(ANES2016.10$Pres.App)
table(ANES2016.11$Pres.App)
#Checks out!

#Measure the Cronbach Alpha between my distrust variable and Presiental Approval
Politican <- with(ANES2016.11, data.frame(Distrust, Pres.App))
cronbach(Politican)

scatterplot(ANES2016.11$Distrust, ANES2016.11$Pres.App, use = "complete")

######################## ***CONTROL VARIABLES:  Ideological Placement *** ############

#Table
table(ANES2016.11$Self.ID)
table(ANES2016.11$Trump.ID)
table(ANES2016.11$HC.ID)


#Make IDK's into Moderates
ANES2016.11$Self.ID[ANES2016.11$Self.ID == "99. Haven't thought much about this (FTF ONLY: DO NOT PROBE)"] <- "4. Moderate, middle of the road"

table(ANES2016.10$Self.ID)

487 + 393
table(ANES2016.11$Self.ID) #Perfect Checks out!

#Drop Unused factors
ANES2016.11$Trump.ID <- factor(ANES2016.11$Trump.ID)
ANES2016.11$HC.ID <- factor(ANES2016.11$HC.ID)
ANES2016.11$Self.ID <- factor(ANES2016.11$Self.ID)

table(ANES2016.11$Self.ID)
table(ANES2016.11$Trump.ID)
table(ANES2016.11$HC.ID)


#Make Both Numeric
Ideology <- c("Self.ID", "Trump.ID", "HC.ID")

ANES2016.11[Ideology] <- sapply(ANES2016.11[Ideology],as.numeric)
sapply(ANES2016.11, class)

table(ANES2016.11$Self.ID)
table(ANES2016.11$Trump.ID)
table(ANES2016.11$HC.ID)



#********************* Proximity Rule ************
#Formula Based on the Proximity Rule
#Negative Indicates Democratic Valence Advantage
#Postive Indicates Republcian Valence Advatage

ANES2016.11$Proximity <- with (ANES2016.11, abs(Self.ID - HC.ID) - abs(Self.ID - Trump.ID))

summary(ANES2016.11$Proximity)
hist(ANES2016.11$Proximity, breaks = 20)
bwplot(ANES2016.11$Proximity)

abs(1 - 4) - abs(1 - 7)

######################## CONTROL VARIABLE: ECONOMY #########################

table(ANES2016.10$Econ)

#Factor away unused evels
ANES2016.11$Econ <- factor(ANES2016.11$Econ)
table(ANES2016.11$Econ)

#Code it naturally from worse to better (1 to 5)
ANES2016.11$Econ <- as.numeric(ANES2016.11$Econ)

ANES2016.11$Econ <- with(ANES2016.11, (Econ * -1) + 6)

table(ANES2016.11$Econ)
table(ANES2016.10$Econ)


################# ***SPENDING DIMENSION *** #########################
Spending <- c("V161205", #PRE: Federal Budget Spending: Social Security
            "V161206", #PRE: Federal Budget Spending: public schools
            "V161207", #PRE: Federal Budget Spending: science and technology
            "V161208", #PRE: Federal Budget Spending: dealing with crime
            "V161209", #PRE: Federal Budget Spending: welfare programs
            "V161210", #PRE: Federal Budget Spending: child care
            "V161211", #PRE: Federal Budget Spending: aid to the poor
            "V161212" #PRE: Federal Budget Spending: protecting the environment
            ) 

################# Spending Dimension: Remove NA's and Clean Data, Make Numeric #########################

#Remove NA's ****************
for(i in Spending) {  
  ANES2016.11[i][ANES2016.11[i] == "-9. Refused"] <- NA
  ANES2016.11[i][ANES2016.11[i] == "-8. Don't know (FTF only)"] <- NA
}

table(ANES2016.10$V161212)
table(ANES2016.11$V161212)

#Make Numeric *********************
ANES2016.11[Spending] <- sapply(ANES2016.11[Spending],as.numeric)
sapply(ANES2016.11, class)

table(ANES2016.10$V161212)
table(ANES2016.11$V161212)

#Put Factors Back in
for(i in Spending) {  
  ANES2016.11[i][ANES2016.11[i] == 3] <- "Increased"
  ANES2016.11[i][ANES2016.11[i] == 4] <- "Decreased"
  ANES2016.11[i][ANES2016.11[i] == 5] <- "Kept the same"
  
}

table(ANES2016.10$V161212)
table(ANES2016.11$V161212)

#Relevel
for(i in Spending) {  
  ANES2016.11[[i]] <- factor(ANES2016.11[[i]],
                             levels = c( "Decreased", #(1)
                                         "Kept the same", #(2)
                                         "Increased" #(3)
                             ))
}

table(ANES2016.10$V161212)
table(ANES2016.11$V161212)


#Make Numeric Again For Factor Analysis  *********************
ANES2016.11[Spending] <- sapply(ANES2016.11[Spending],as.numeric)
sapply(ANES2016.11, class)

table(ANES2016.10$V161212)
table(ANES2016.11$V161212)

################# Spending Dimension: Factor Analysis #########################
ANES2016.Spending <- with(ANES2016.11, data.frame(V161205, #PRE: Federal Budget Spending: Social Security
                                                        V161206, #PRE: Federal Budget Spending: public schools
                                                        V161207, #PRE: Federal Budget Spending: science and technology
                                                        V161209, #PRE: Federal Budget Spending: welfare programs
                                                        V161210, #PRE: Federal Budget Spending: child care
                                                        V161211, #PRE: Federal Budget Spending: aid to the poor
                                                        V161212 #PRE: Federal Budget Spending: protecting the environment
))
head(ANES2016.Spending)
#Drop Crime

#Parallel Analysis
parallel <- fa.parallel(ANES2016.Spending, fm = 'minres', fa = 'fa')

#Factor Analysis

#Factor
factor <- fa(ANES2016.Spending,nfactors = 1,rotate = "oblimin",fm="minres")
print(factor)

print(factor$loadings,cutoff = 0.3)
fa.diagram(factor)

cronbach(ANES2016.Spending)


################## Spending Dimension: Row Mean ######################
ANES2016.11$Spending <- rowMeans(ANES2016.11 [,c("V161205", #PRE: Federal Budget Spending: Social Security
                                                 "V161206", #PRE: Federal Budget Spending: public schools
                                                 "V161207", #PRE: Federal Budget Spending: science and technology
                                                 "V161209", #PRE: Federal Budget Spending: welfare programs
                                                 "V161210", #PRE: Federal Budget Spending: child care
                                                 "V161211", #PRE: Federal Budget Spending: aid to the poor
                                                 "V161212" #PRE: Federal Budget Spending: protecting the environment
)])

################## Spending Dimension: Descriptive Stats ######################
hist(ANES2016.11$Spending)
bwplot(ANES2016.11$Spending)
summary(ANES2016.11$Spending)
sd(ANES2016.11$Spending, na.rm = T)



################## *** POLICY DIMENSTION *** #########################

Policy <- c(#1-7
            "V161196x", #PRE: SUMMARY - Build wall with Mexico L
            "V161195x", #PRE: SUMMARY - Children brought illegally L
            "V161214x", #PRE: SUMMARY - Allow Syrian refugees C
            "V161225x", #PRE: SUMMARY - Govt action about rising temperatures C
            "V161226x", #PRE: SUMMARY - require employers to offer paid leave to new parents C
            "V161227x", #PRE: SUMMARY - Services to same sex couples L
            "V161228x", #PRE: SUMMARY - Transgender policy L
            "V161204x", #PRE: SUMMARY - Favor or oppose affirmative action in universities C
            
            #1-4
            "V161229x", #PRE: SUMMARY - Laws to protect gays and lesbians against job discrim C
            "V161233x", #PRE: SUMMARY - Favor or oppose death penalty L
            
            #1 to 3
            "V161187",  #PRE: Should fed govt make it more difficult to buy a gun L/C
            #1-2
            "V161221"  #PRE: Is global warming happening or not C
  )

for (i in Policy) {
  print(table(ANES2016.11[i]))
  
}


table(ANES2016.11$V161196x)
#Make Numeric**********************************

Policy <- c(#1-7
  "V161196x", #PRE: SUMMARY - Build wall with Mexico L
  "V161195x", #PRE: SUMMARY - Children brought illegally L
  "V161214x", #PRE: SUMMARY - Allow Syrian refugees C
  "V161225x", #PRE: SUMMARY - Govt action about rising temperatures C
  "V161226x", #PRE: SUMMARY - require employers to offer paid leave to new parents C
  "V161227x", #PRE: SUMMARY - Services to same sex couples L
  "V161228x", #PRE: SUMMARY - Transgender policy L
  "V161204x", #PRE: SUMMARY - Favor or oppose affirmative action in universities C
  
  #1 to 3
  "V161187",  #PRE: Should fed govt make it more difficult to buy a gun L/C
  #1-2
  "V161221"  #PRE: Is global warming happening or not C
)


ANES2016.11[Policy] <- sapply(ANES2016.11[Policy],as.numeric)
sapply(ANES2016.11, class)

table(ANES2016.10$V161196x)
table(ANES2016.11$V161196x)

#Remove NA's and Recode ****************

for(i in Policy) {  
  ANES2016.11[i][ANES2016.11[i] == 1] <- NA
  ANES2016.11[i][ANES2016.11[i] == 2] <- NA
  
  ANES2016.11[i] <- (ANES2016.11[i] - 2)
  
}

table(ANES2016.10$V161196x)
table(ANES2016.11$V161196x)

for (i in Policy) {
  print(table(ANES2016.11[i]))
  
}

#Recode 1-4 Policies
Policy.4 <- c(
              "V161229x", #PRE: SUMMARY - Laws to protect gays and lesbians against job discrim C
              "V161233x" #PRE: SUMMARY - Favor or oppose death penalty L
              )

for(i in Policy.4) {  
  ANES2016.11[i][ANES2016.11[i] == -1] <- NA
  
}

for (i in Policy.4) {
  print(table(ANES2016.11[i]))
  
}


# Recode LIberal POlicies in the conservaitve direction


Policy.Liberal <- c(#1-7
  "V161196x", #PRE: SUMMARY - Build wall with Mexico L
  "V161195x", #PRE: SUMMARY - Children brought illegally L
  "V161227x", #PRE: SUMMARY - Services to same sex couples L
  "V161228x", #PRE: SUMMARY - Transgender policy L
  
  #1-4
  "V161233x" #PRE: SUMMARY - Favor or oppose death penalty L
  
)

for (i in Policy.Liberal) {
  ANES2016.11[i] <- ((ANES2016.11[i] * -1) + 7)
  
  
}

for (i in Policy.Liberal) {
  print(table(ANES2016.11[i]))
  
}

#Do 1 to 4
ANES2016.11$V161233x <- ((ANES2016.11$V161233x -2))

#Add one to Wall
ANES2016.11$V161196x <- ((ANES2016.11$V161196x +1))


table(ANES2016.10$V161233x)

table(ANES2016.11$V161233x)

#Recode Death Penality


ANES2016.11$V161187[ANES2016.11$V161187 == 2] <- "Easier"
ANES2016.11$V161187[ANES2016.11$V161187 == 3] <- "Same"

ANES2016.11$V161187[ANES2016.11$V161187 == "Easier"] <- 3
ANES2016.11$V161187[ANES2016.11$V161187 == "Same"] <- 2

ANES2016.11$V161187 <- as.numeric(ANES2016.11$V161187)

table(ANES2016.10$V161187)
table(ANES2016.11$V161187)

#Final Check
for (i in Policy) {
  print(table(ANES2016.11[i]))
  
}

####################### POLICY: Factor Analysis ########################
ANES2016.Policy <- with(ANES2016.11, data.frame(
  V161196x, #PRE: SUMMARY - Build wall with Mexico L
  V161195x, #PRE: SUMMARY - Children brought illegally L
  V161214x, #PRE: SUMMARY - Allow Syrian refugees C
  V161225x, #PRE: SUMMARY - Govt action about rising temperatures C
  V161226x, #PRE: SUMMARY - require employers to offer paid leave to new parents C
  V161227x, #PRE: SUMMARY - Services to same sex couples L
  V161228x, #PRE: SUMMARY - Transgender policy L
  V161204x, #PRE: SUMMARY - Favor or oppose affirmative action in universities C
  
  #1-4
  V161229x, #PRE: SUMMARY - Laws to protect gays and lesbians against job discrim C
  V161233x, #PRE: SUMMARY - Favor or oppose death penalty L
  
  #1 to 3
  V161187,  #PRE: Should fed govt make it more difficult to buy a gun L/C
  #1-2
  V161221  #PRE: Is global warming happening or not C
))

sapply(ANES2016.Policy, class)

head(ANES2016.Policy)
#Drop Crime

#Parallel Analysis
parallel <- fa.parallel(ANES2016.Policy, fm = 'minres', fa = 'fa')

#Factor Analysis

#Factor
factor <- fa(ANES2016.Policy,nfactors = 1,rotate = "oblimin",fm="minres")
print(factor)

print(factor$loadings,cutoff = 0.3)
fa.diagram(factor)

cronbach(ANES2016.Policy)


######################## POLICY: Place on Same Scale #########################

for (i in Policy) {
  ANES2016.11[i] <- scale(ANES2016.11[i])
  
}
######################## POLICY: Place on Same Scale #########################

ANES2016.11$Policy <- rowMeans(ANES2016.11 [,c(#1-7
  "V161196x", #PRE: SUMMARY - Build wall with Mexico L
  "V161195x", #PRE: SUMMARY - Children brought illegally L
  "V161214x", #PRE: SUMMARY - Allow Syrian refugees C
  "V161225x", #PRE: SUMMARY - Govt action about rising temperatures C
  "V161226x", #PRE: SUMMARY - require employers to offer paid leave to new parents C
  "V161227x", #PRE: SUMMARY - Services to same sex couples L
  "V161228x", #PRE: SUMMARY - Transgender policy L
  "V161204x", #PRE: SUMMARY - Favor or oppose affirmative action in universities C
  
  #1-4
  "V161229x", #PRE: SUMMARY - Laws to protect gays and lesbians against job discrim C
  "V161233x", #PRE: SUMMARY - Favor or oppose death penalty L
  
  #1 to 3
  "V161187",  #PRE: Should fed govt make it more difficult to buy a gun L/C
  #1-2
  "V161221"  #PRE: Is global warming happening or not C
)])

################## Policy Dimension: Descriptive Stats ######################
hist(ANES2016.11$Policy)
bwplot(ANES2016.11$Policy)
summary(ANES2016.11$Policy)
sd(ANES2016.11$Policy, na.rm = T)


######################## OLS: TABLE ################

#**************Table********

#DV
table(ANES2016.11$PresVote)

#IV
table(ANES2016.11$Distrust)

#**********Controls
#Ordinal
table(ANES2016.11$PS.AttPol) 
table(ANES2016.11$PS.Camp) 

#Dicodimious
table(ANES2016.11$Union)
table(ANES2016.11$Gender)
table(ANES2016.11$PS.Vote2012)

#Continutios categorical
table(ANES2016.11$Education)
table(ANES2016.11$Income)

#Categorical
table(ANES2016.11$Race)

#Continious
table(ANES2016.11$Age) 

#Party ID
table(ANES2016.11$PartyID)

#Presidental Approval
summary(ANES2016.11$Pres.App)

#Ideology Proximity
summary(ANES2016.11$Proximity)


####################### GLM:1v1 ############################

Logit.Distrust.1v1 <- glm(PresVote ~ Distrust, data = ANES2016.11, family=binomial(link=logit))
summary(Logit.Distrust.1v1)

pR2(Logit.Distrust.1v1)

plot(
  effect("Distrust", 
         Logit.Distrust.1v1
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "Logit: Interaction Plot: Distrust and Voting"
)

Logit.Distrust.1v1.Odds <- exp(cbind(coef(Logit.Distrust.1v1), confint(Logit.Distrust.1v1))) 

#************* Ggplot Plot Graph *****************
# Plotting an Additive Regression with ggPlot2

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "glm", col = "red") +
    labs(title = paste(
      "Intercept =",signif(fit$coef[[1]],5 ),
      " Slope =",signif(fit$coef[[2]], 5),
      " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(glm(PresVote ~ Distrust, data = ANES2016.11, family=binomial(link=logit))) + theme_fivethirtyeight()

#Note the "names(fit$model)[2]". Change the number to specify the coefficient.


#************* Predicted Probabilities ***********
z.out <- zelig(PresVote ~ Distrust, model="logit",
               data = ANES2016.11
               )

zelig(PresVote ~ Distrust, model = "ls", data = ANES2016.11, cite = FALSE)

summary(z.out)


x.out1 <- setx(z.out, Distrust = 1:5)
#Model  #Set x
s.outD <- sim(z.out, x = x.out1)

summary(s.outD)
ci.plot(s.outD, ci= 95, leg = 0, col = "green", alpha = .3)

#Change in Probabiility
0.548 - 0.383


####################### OLS: Party Controls ################
OLS.Distrust <- lm(PresVote ~ Distrust + PartyID, data = ANES2016.11)
summary(OLS.Distrust)

vif(OLS.Distrust)
ncvTest(OLS.Distrust)

plot(
  effect("Distrust", 
         OLS.Distrust
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting")

####################### OLS: Party ID Interaction ################
OLS.Distrust <- lm(PresVote ~ Distrust * PartyID, data = ANES2016.11)
summary(OLS.Distrust)

vif(OLS.Distrust)
ncvTest(OLS.Distrust)

plot(
  effect("Distrust * PartyID", 
         OLS.Distrust
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting"
)

levels(ANES2016.9$PS.Camp)

################## *** OLS/LOGIT ADDIVITVE MODELS *** #################
################## OLS: Additive w/ Full Controls #################
OLS.Distrust <- lm(PresVote ~ Distrust 
                    + PartyID
                    + Union
                    + Gender
                    + Education
                    + Income
                    + Race
                    + Age
                    + Proximity,
                    data = ANES2016.11)
summary(OLS.Distrust)

#************************ Diagnostics *******************
#Multicolinarity
vif(OLS.Distrust)

#Heteroskadasity
ncvTest(OLS.Distrust)
spreadLevelPlot(OLS.Distrust)

# Test for Autocorrelated Errors
durbinWatsonTest(OLS.Distrust)

# Evaluate Nonlinearity
# component + residual plot 
crPlots(OLS.Distrust)
# Ceres plots 
ceresPlots(OLS.Distrust)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(OLS.Distrust) 
summary(gvmodel)



plot(
  effect("Distrust", 
         OLS.Distrust
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Additive Plot: Distrust and Voting")

################## *** LOGIT: Additive w/ Full Controls *** #################

Logit.Distrust.A <- glm(PresVote ~ Distrust 
                                      + PartyID
                                      + Union
                                      + Gender
                                      + Education
                                      + Income
                                      + Race
                                      + Age
                                      + Proximity, 
                              data = ANES2016.11, family=binomial(link=logit))
summary(Logit.Distrust.A)
pR2(Logit.Distrust.A)



vif(Logit.Distrust.A)
ncvTest(Logit.Distrust.A)

plot(
  effect("Distrust", 
         Logit.Distrust.A
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "Logit: Interaction Plot: Distrust and Voting")

Logit.Distrust.A.Odds <- exp(cbind(coef(Logit.Distrust.A), confint(Logit.Distrust.A))) 

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob (0.321102)

OddsToProb <- function(odds) {
  
prob <-  odds / (1 + odds)

return(prob)

}
OddsToProb(1.37864590)

#***************** Precent Change *********************
#******************************** The Model ****************************
z.out <- zelig(PresVote ~ Distrust 
                                 + PartyID
                                 + Union
                                 + Gender
                                 + Education
                                 + Income
                                 + Race
                                 + Age
                                 + Proximity,
                                 data = ANES2016.11,
                                 model="logit")

summary(z.out)


#***************** Mode Function *********************
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#********************* Democrat ********************

x.outD <- setx(z.out, Distrust = 1:5, 
                PartyID =  "Democrat", 
                Union =  "No", 
                Gender =  "Female", 
                Education =  12, 
                Income =  17, 
                Race =  "White", 
                Age =  54, 
                Proximity = 0)
#Model  #Set x
s.outD <- sim(z.out, x = x.outD)

summary(s.outD)
ci.plot(s.outD, ci= 95, leg = 0, col = "green", alpha = .3)

#Change in Probabiility
0.23 - 0.072

#********************* Independent  ********************

x.outI <- setx(z.out,Distrust = 1:5, 
                PartyID =  "Independent", 
                Union =  "No", 
                Gender =  "Female", 
                Education =  12, 
                Income =  17, 
                Race =  "White", 
                Age =  54, 
                Proximity = 0)
#Model  #Set x
s.outI <- sim(z.out, x = x.outI)

summary(s.outI)
ci.plot(s.outI, ci= 95, leg = 0)

#Change in Probability
0.727 - 0.426


#********************* Republican  ********************

x.outR <- setx(z.out,Distrust = 1:5, 
                PartyID =  "Republican", 
                Union =  "No", 
                Gender =  "Female", 
                Education =  12, 
                Income =  17, 
                Race =  "White", 
                Age =  54, 
                Proximity = 0)
#Model  #Set x
s.outR <- sim(z.out, x = x.outR)

summary(s.outR)
plot(s.outR, ci= 95)

#Change in Probability
0.887 - 0.696

#********************* Highest level of the predictor********************

x.out1j <- setx(z.out, Incval_pc09.recode = 7, 
                Congress.recode = 4, 
                Inc.party = "Republican",
                Party = "Incumbent Party")

#Model  #Set x
s.out1j <- sim(z.Logit.Distrust.Aout, x = x.out1j)

summary(s.out1j)
plot(s.out1j)

################## *** OLS/LOGIT INTERACTION MODELS *** ######################

################## OLS: Party ID Interaction w/ Full Controls ################
OLS.Distrust <- lm(PresVote ~ Distrust * PartyID
                    + Union
                    + Gender
                    + Education
                    + Income
                    + Age
                    + Race
                    + Proximity,
                    data = ANES2016.11)
summary(OLS.Distrust)
plot(OLS.Distrust)

vif(OLS.Distrust)
ncvTest(OLS.Distrust)

plot(
  effect("Distrust * PartyID", 
         OLS.Distrust
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting")

################### LOGIT: Party ID Interaction w/ Full Controls  #################

Logit.Distrust.I <- glm(PresVote ~ Distrust * PartyID
                                       + Union
                                       + Gender
                                       + Education
                                       + Income
                                       + Age
                                       + Race
                                       + Proximity,
                                       data = ANES2016.11, family=binomial(link=logit))
summary(Logit.Distrust.I)
plot(Logit.Distrust.I)

vif(Logit.Distrust.I)
ncvTest(Logit.Distrust.I)

plot(
  effect("Distrust * PartyID", 
         Logit.Distrust.I
  ), 
  xlab = "Distrust",
  ylab = "Probability of Voting for Trump",
  main = "OLS: Interaction Plot: Distrust and Voting")



####################### Likelyhood Ratio Test ######################
lrtest(Logit.Distrust.A, Logit.Distrust.I)
lrtest(Logit.Distrust.A, Logit.Distrust.R)



#Relevel
ANES2016.11$PartyID <- factor(ANES2016.11$PartyID,
                                    levels = c("Republican",
                                               "Independent",
                                               "Democrat" ))
 
                                               
                                          
                                                                                  
############################ ***Presidential Approval*** ###################
OLS.Distrust <- lm(PresVote ~  Distrust * PartyID + Pres.App + Proximity + Econ + Spending, data = ANES2016.11)
summary(OLS.Distrust)
vif(OLS.Distrust)
ncvTest(OLS.Distrust)
robust.se(OLS.Distrust)


Logit.Distrust.P <- glm(PresVote ~  Distrust + PartyID + Pres.App + Proximity + Econ + Policy, data = ANES2016.11, family=binomial(link=logit))
summary(Logit.Distrust.P)
pR2(Logit.Distrust.P)

Logit.Distrust.P.odds <- exp(cbind(coef(Logit.Distrust.P), confint(Logit.Distrust.P))) 


plot(
  effect("Distrust * PartyID", 
         OLS.Distrust
  ), 
  xlab = "Pres.App",
  ylab = "Distrust",
  main = "OLS: Interaction Plot: Distrust and Voting")

OLS.Distrust <- lm(Pres.App ~ Distrust, data = ANES2016.11)

OLS.Distrust <- lm(as.numeric(PartyID) ~ Distrust, data = ANES2016.11)
summary(OLS.Distrust)

################ *** LOGIT: COMBINDED MODEL *** ########################
Logit.Distrust.C <- glm(PresVote ~ Distrust 
                                            + PartyID
                                            + Union
                                            + Gender
                                            + Education
                                            + Income
                                            + Age
                                            + Race
                                            + Proximity 
                                            + Pres.App
                                            + Econ
                                            + Policy,
                                                  data = ANES2016.11, family=binomial(link=logit))
summary(Logit.Distrust.C)

pR2(Logit.Distrust.C)

Logit.Distrust.C.Odds <- exp(cbind(coef(Logit.Distrust.C), confint(Logit.Distrust.C))) 


################ *** BOOTSTRAP *** #######################

#*******************************Bootstrap Function*************************

boot.beta <- function(data, indices){
  
  #recreate the data set by pulling out Samples from the data set
  d <- data[indices,] # allows boot to select sample 
  
  #run a ols model, using our new data, "data"
  mod <- glm(PresVote ~ Distrust + PartyID + Pres.App + Proximity + Econ, data=d, family=binomial(link=logit)) 
  
  #then extract the our beta
  mod$coef 
}

#*****************************The Bootstrap**************************************
rm(bootstrap.OLS.Distrust)

#(our original data, our creatted function, # of times)
bootstrap.OLS.Distrust <- boot(data = ANES2016.11, 
                       statistic=boot.beta, 
                       1000
)
bootstrap.OLS.Distrust

hist(bootstrap.OLS.Distrust)

#Distust Interaction Model: Highly signifigant 
2*pt(abs(0.2322131)/0.13825274, (2468-138)- 6, lower = FALSE)

#Interaction: Distrust and  Indepednet Party ID - Signifigant
2*pt(abs(0.0846055960)/0.0425232831, (2468-180)- 16, lower = FALSE)



################## *** DIFFERENCE IN DIFFERENCE *** #####################

#************************* Trust in Government ********************
#Correlation
cor(ANES2016.11$Distrust, ANES2016.11$Pre.Trust.Gov, use ="complete")
scatterplot(ANES2016.11$Distrust, ANES2016.11$Pre.Trust.Gov, use = "complete")

#Measure the Cronbach Alpha between my distrust variable and Presiental Approval
Trust.Pre.Post <- with(ANES2016.11, data.frame(Distrust, Pre.Trust.Gov))
cronbach(Trust.Pre.Post)

#*********************** How many are Corrupt ******************

#Correlation
cor(ANES2016.11$Distrust, ANES2016.11$Pre.Corrupt, use ="complete")
scatterplot(ANES2016.11$Distrust, ANES2016.11$Pre.Corrupt, use = "complete")

#Measure the Cronbach Alpha between my distrust variable and Presiental Approval
Trust.Pre.Post <- with(ANES2016.11, data.frame(Distrust, Pre.Corrupt))
cronbach(Trust.Pre.Post)


################## *** INSTRAMENTAL VARIABLE ANALYSIS*** ###########

#Formula 
# Z-> X -> Y
#Instramental Varibale: POST: Feeling thermometer: CONGRESS


######################## INSTRAMENTAL VARIABLE: RECODE ##################

#************************** Congress **********************
#Rename 
ANES2016.11 <- rename(ANES2016.11,
                      Congress = V162104)

summary(ANES2016.11$Congress)

#Put NA for values > 990 and < 0
ANES2016.11$Congress[ANES2016.11$Congress == "998"] <- NA
ANES2016.11$Congress[ANES2016.11$Congress == "999"] <- NA
ANES2016.11$Congress[ANES2016.11$Congress == "-9"] <- NA

#Descriptive Stats
summary(ANES2016.11$Congress)
table(ANES2016.11$Congress)
hist(ANES2016.11$Congress)

#************************ Supreme Court ******************

#Rename 
ANES2016.11 <- rename(ANES2016.11,
                      SCOTUS = V162102)

summary(ANES2016.11$SCOTUS)
table(ANES2016.11$SCOTUS)


#Put NA for values > 990 and < 0
ANES2016.11$SCOTUS[ANES2016.11$SCOTUS == "998"] <- NA
ANES2016.11$SCOTUS[ANES2016.11$SCOTUS == "999"] <- NA
ANES2016.11$SCOTUS[ANES2016.11$SCOTUS == "-9"] <- NA

#Descriptive Stats
summary(ANES2016.11$SCOTUS)
table(ANES2016.11$SCOTUS)
hist(ANES2016.11$SCOTUS)

###################### Good IV or Naw? ###################

#Congress - Naw
cor(ANES2016.11$Distrust, ANES2016.11$Congress, use = "complete")
cor( ANES2016.11$Congress, ANES2016.11$PresVote, use = "complete")

#SCOTUS - 
cor(ANES2016.11$Distrust, ANES2016.11$SCOTUS, use = "complete")
cor(ANES2016.11$SCOTUS, ANES2016.11$PresVote, use = "complete")

#Trust in People
cor(ANES2016.11$Distrust, ANES2016.11$Pre.Trust.People, use = "complete")
cor(ANES2016.11$Pre.Trust.People, ANES2016.11$PresVote, use = "complete")


###################### IV Analysis ###################
#Model 1
ivmodel <- ivreg(PresVote ~  Pres.App + PartyID + Union
                                               + Gender
                                               + Education
                                               + Income
                                               + Age
                                               + Race
                                               + Proximity, 
                 
                 ~ Distrust + PartyID + Union
                                       + Gender
                                       + Education
                                       + Income
                                       + Age
                                       + Race
                                       + Proximity,
                 
                 x=TRUE, data=ANES2016.11)


summary(ivmodel)
anderson.rubin.ci(ivmodel)



#Model 2
ivmodel <- ivreg(PresVote ~  Distrust + PartyID + Pres.App + Proximity + Econ, 
              
              ~ Congress + PartyID + Pres.App + Proximity + Econ,
              
              x=TRUE, data=ANES2016.11)


summary(ivmodel)
anderson.rubin.ci(ivmodel)









###################### *** Ideology Analysis on Race *** ##########################
ANES2016.Latino <- ANES2016.11

ANES2016.Latino$Self.ID <- with(ANES2016.Latino, Self.ID - 4)
ANES2016.Latino$HC.ID <- with(ANES2016.Latino, HC.ID - 4)
ANES2016.Latino$Trump.ID <- with(ANES2016.Latino, Trump.ID - 4)

hist(ANES2016.Latino$Self.ID)

Ideology.Estimates <- data.frame()
Ideology.Estimates <- NULL

#Simple Means
Mean <- aggregate(Self.ID ~ Race, ANES2016.Latino, mean )

Ideology.Estimates$Race <- Mean$Race
Ideology.Estimates$Mean <- Mean$Self.ID

#Simple SD
sd.Race <- aggregate(Self.ID ~ Race, ANES2016.Latino, sd )

Ideology.Estimates$sd <- sd.Race$Self.ID


table(ANES2016.Traits$V161128)

mean(as.numeric(ANES2016.Traits$V161128), na.rm = T)
mean(as.numeric(ANES2016.Traits$V161129), na.rm = T)

table(ANES2016.Traits$V161128)

plot(as.numeric(Ideology.Estimates$Race), Ideology.Estimates$Mean.Self.ID)

View(Ideology.Estimates)

#Hispanic Vote
table(ANES2016.11$Race)
Latino.Vote <- subset(ANES2016.11, Race == "Hispanic")

CrossTable(Latino.Vote$Race, Latino.Vote$PresVote)
hist(Latino.Vote$Self.ID)

CrossTable(ANES2016.Traits.3b$V161310x, ANES2016.Traits.3b$V162034a)

table(ANES2016.Traits.3b$V161310x)

############################## TABLES ##########################
#1V1
stargazer(Logit.Distrust.1v1)
stargazer(Logit.Distrust.1v1.Odds)

#Demographics Model
stargazer(Logit.Distrust.A)
stargazer(Logit.Distrust.A.Odds)

#Poltical Variables
stargazer(Logit.Distrust.P)
stargazer(Logit.Distrust.P.odds)

#Combination Model
stargazer(Logit.Distrust.C)
stargazer(Logit.Distrust.C.Odds)

#Interaction Model
stargazer(Logit.Distrust.I)
pR2(Logit.Distrust.I)

########### Descriptie Stats: Distrust ###########

ggplot(ANES2016.11, aes(x=Distrust)) +
  geom_histogram(data=subset(ANES2016.11,Distrust <= quantile(ANES2016.11$Distrust, na.rm =T)[2]),bins=10, fill = "#000066", alpha = 0.4) +
  geom_histogram(data=subset(ANES2016.11,Distrust <= quantile(ANES2016.11$Distrust, na.rm =T)[3]),bins=10, fill = "#0033CC", alpha = 0.4) +
  geom_histogram(data=subset(ANES2016.11,Distrust <= quantile(ANES2016.11$Distrust, na.rm =T)[4]),bins=10, fill = "#0099FF", alpha = 0.4) +
  geom_histogram(data=subset(ANES2016.11,Distrust <= quantile(ANES2016.11$Distrust, na.rm =T)[5]),bins=10, fill = "#00FFFF", alpha = 0.4) +
  labs(x = "Distrust of Politicians",
       y = "Frequency",
       caption = "Shading Delineates Quartiles",
       title = "Histogram of Distrust") +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c(1,3,5), labels=c("Strongly Trust", 
                                               "Neither Trust nor Distrust",
                                               "Strongly Distrust"))

summary(ANES2016.11$Distrust)
sd(ANES2016.11$Distrust, na.rm = T)
