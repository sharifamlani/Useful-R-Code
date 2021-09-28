#Preamble
library(foreign)
library(car)
library(lattice)
library(RColorBrewer)
library(latticeExtra)
library(effects)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(rms)
library(haven)
library(readstata13)
library(apsrtable)
library(boot)
library(Hmisc)
library(lmtest)
library(sandwich)
library(stargazer)
library(sfsmisc)
library(reshape)
library(ggthemes)

#*************************Data for Final Project***********************************
options(scipen=3)


#Set working Directory
setwd("~/University of California, Davis/First Year/Winter Quarter/Methods - 212/Final Paper/Data/Working Directory for Final Project")

#Upload Data to R
House <- read.csv("GJ_ORIGINAL_1946-2016_House_Elex.csv")

nrow(House)

House <- House [House$year == 2010, ]

nrow(House)

Valence_Data <- read.csv("2010_District_UCDavis.csv")

#**************************Adding "0" to independent numbers******************
House$cd2 <- House$cd

# Replacing digits 1-9 to have a 0
House$cd2[House$cd2 == "1"] <- "01"
House$cd2[House$cd2 == "2"] <- "02"
House$cd2[House$cd2 == "3"] <- "03"
House$cd2[House$cd2 == "4"] <- "04"
House$cd2[House$cd2 == "5"] <- "05"
House$cd2[House$cd2 == "6"] <- "06"
House$cd2[House$cd2 == "7"] <- "07"
House$cd2[House$cd2 == "8"] <- "08"
House$cd2[House$cd2 == "9"] <- "09"

# Pasting the state name and congressional district number into one varibale
House$district <- paste(House$state,House$cd2)

# Gets rid of the white space between the state name and the digit
House$district <- gsub(" ", "", House$district, fixed = TRUE)

#Making the row names the same for Merge
rownames(House) <- House$district  

write.csv(House, file = "GJ_ORIGINAL_1946-2016_House_Elex.New.csv")

#******************************Combinded Data *****************************

House.Valence.Pure <- merge(x = Valence_Data, y = House, 
                            #if the variables are the same
                            by = "district")

#*****************************Combinded Data Management**************************
#Check Same Row Number (155 Expected)                       
nrow(Valence_Data)
nrow(House.Valence.Pure)

#Make sure columns were added
ncol(Valence_Data)
ncol(House)

ncol(House) + ncol(Valence_Data) # = Total NUmber of Expected Merged Columns


ncol(House.Valence.Pure)

#******************************************Remove Extranious variables*****************************

House.Valence <- with(House.Valence.Pure, 
                      data.frame(
                        #Leadership Valence - DEM
                        dinteg_pc10, dworkoth_pc10, dcomp_pc10, 
                        dgrasp_pc10, dsolve_pc10,   dqualif_pc10, 
                        dserv_pc10,
                        
                        #Campaign Valence - DEM
                        dprofcamp_pc10, dcamp_pc10,      dpublic_pc10,       
                        dnamerec_pc10,  dfundraise_pc10, dqualif_pc10,       
                        dserv_pc10,     dvis_pc10,
                        
                        #Leadership Valence - REP
                        rinteg_pc10, rworkoth_pc10, rcomp_pc10, 
                        rgrasp_pc10, rsolve_pc10,   rqualif_pc10, 
                        rserv_pc10,
                        
                        #Campaign Valence - REP
                        rprofcamp_pc10, rcamp_pc10,      rpublic_pc10,       
                        rnamerec_pc10,  rfundraise_pc10, rqualif_pc10,       
                        rserv_pc10,     rvis_pc10,
                        
                        #Control Variables
                        dpres, incran10,  chexp10,
                        
                        #Dependent Varibles
                        dexp, rexp,
                        
                        #Data Mangement Varables
                        district, cd, demvote10
                      ))

#***************************************More Data Management*******************************
View (House.Valence)

#confirm col names
colnames(House.Valence)

#Set row names
rownames(House.Valence) <- House.Valence$district

rownames(House.Valence)

#**********************************************Remove NA's*****************************************

#Before
nrow(House.Valence)

House.Valence <- na.omit(House.Valence)

#After
nrow(House.Valence)
#Perfect 150


#***********************Competitive Districts**********************************

House.Valence$compete <- NULL

which(House.Valence$demvote10 > 47  & House.Valence$demvote10 < 53)

House.Valence$compete [House.Valence$demvote10 > 47  & 
                         House.Valence$demvote10 < 53] <- 1

House.Valence$compete [House.Valence$demvote10 < 47  | 
                         House.Valence$demvote10 > 53] <- 0

table(House.Valence$compete)

House.Valence$compete <- as.factor(House.Valence$compete)

class(House.Valence$compete)

#*********************Key Independent Variable (Valenece)

#Combining the key valence measure (Democracts)
House.Valence$dpers_pc10 <- rowMeans(House.Valence [,c("dinteg_pc10",
                                                       "dworkoth_pc10", 
                                                       "dcomp_pc10", 
                                                       "dgrasp_pc10", 
                                                       "dsolve_pc10", 
                                                       "dqualif_pc10", 
                                                       "dserv_pc10"
)])


#Mean of Campaign Valence
House.Valence$dpers.camp_pc10 <- rowMeans(House.Valence [, c('dprofcamp_pc10',     
                                                             'dcamp_pc10',       
                                                             'dpublic_pc10',       
                                                             'dnamerec_pc10',     
                                                             'dfundraise_pc10',    
                                                             'dqualif_pc10',       
                                                             'dserv_pc10',        
                                                             'dvis_pc10'
)])


summary(House.Valence$dpers.camp_pc10)




#Mean leadership valence measure (Republicans)
House.Valence$rpers_pc10 <- rowMeans(House.Valence [,c("rinteg_pc10",
                                                       "rworkoth_pc10", 
                                                       "rcomp_pc10", 
                                                       "rgrasp_pc10", 
                                                       "rsolve_pc10", 
                                                       "rqualif_pc10", 
                                                       "rserv_pc10"
)])

#Mean of Campaign Valence (REPUBLICANS)
House.Valence$rpers.camp_pc10 <- rowMeans(House.Valence [, c('rprofcamp_pc10',     
                                                             'rcamp_pc10',       
                                                             'rpublic_pc10',       
                                                             'rnamerec_pc10',     
                                                             'rfundraise_pc10',    
                                                             'rqualif_pc10',       
                                                             'rserv_pc10',        
                                                             'rvis_pc10'
)])


summary(House.Valence$rpers.camp_pc10)

#MEAN Valence Difference

House.Valence$diffpers_pc10 <-  House.Valence$rpers_pc10 - House.Valence$dpers_pc10


#MEAN Valence Differnce per category

House.Valence$diffinteg_pc10 <-   House.Valence$rinteg_pc10 - House.Valence$dinteg_pc10
House.Valence$diffworkoth_pc10 <- House.Valence$rworkoth_pc10 - House.Valence$dworkoth_pc10
House.Valence$diffcomp_pc10 <-    House.Valence$rcomp_pc10 - House.Valence$dcomp_pc10
House.Valence$diffgrasp_pc10 <-   House.Valence$rgrasp_pc10 - House.Valence$dgrasp_pc10
House.Valence$diffsolve_pc10 <-   House.Valence$rsolve_pc10 - House.Valence$dsolve_pc10
House.Valence$diffqualif_pc10 <-  House.Valence$rqualif_pc10 - House.Valence$dqualif_pc10
House.Valence$diffserv_pc10 <-    House.Valence$rserv_pc10 - House.Valence$dserv_pc10


#Valence Characteristics
summary(House.Valence$dpers_pc10) #Democrats
summary(House.Valence$rpers_pc10) #Republicans
summary(House.Valence$diffpers_pc10) #Difference (Negative = Dem Advatahe)


#*************************************THE PLOT******************************* 
House.Valence$Win <- NULL

House.Valence$Win [House.Valence$demvote10 >= 50.01] <- "Democratic Victory"
House.Valence$Win [House.Valence$demvote10 < 50.01] <- "Republican Victory"

House.Valence$Win <- as.factor(House.Valence$win)

"Repubican Valence Advantage" <- (letters[2:1] == "a")
"Democratic Valence Advantage" <- (letters[2:1] == "b")

rects <- data.frame(xstart = seq(-2,0,2), xend = seq(0,2,2), col = letters[2:1])


ggplot()+
  geom_rect(data = rects, 
            aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), 
            alpha = 0.4) + guides(fill=FALSE) +
  geom_point(data = House.Valence, aes(x = diffpers_pc10, y = dexp, color = Win)) +
  scale_color_manual(values=c("blue", "red")) +
  stat_smooth(data = House.Valence, aes(x = diffpers_pc10, y = dexp), colour= "#0099FF", linetype="solid", size=1.25, method = "lm") +
  labs(x ="Valence Advantage",
       y = "Democratic Expenditures") +
  ggtitle("Valence Advantage & Democratic Expenditures", subtitle = "Party Color Denotes Valence Advantage") 
