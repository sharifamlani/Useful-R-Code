library(margins)
library(multiwayvcov)
library(ggplot2)
library(asciiSetupReader)
library(plyr)
library(reshape2)
library(descr)
library(readstata13)
library(gridBase)
library(gridExtra)
library(grid)

options(scipen=999)

setwd("/Users/carlosalgara/Desktop/carlos_school/PhD_UC Davis/Research/Submissions & Applications/House_Senate_Unified_Model/Data & Code")

load("county_level_senate_elections_dataset.Rdata")

sen$senate_raw_county_vote_totals_two_party_logged <- sen$senate_raw_county_vote_totals_two_party
sen$senate_raw_county_vote_totals_two_party_logged[sen$senate_raw_county_vote_totals_two_party_logged == 0] <- 1
sen$senate_raw_county_vote_totals_two_party_logged <- log(as.numeric(sen$senate_raw_county_vote_totals_two_party_logged))

sen$missing_county_data <- ifelse(is.na(sen$senate_democratic_raw_votes) & is.na(sen$senate_republican_raw_votes),1,0)
sen$missing_county_data_dem <- ifelse(is.na(sen$senate_democratic_raw_votes),1,0)
sen$missing_county_data_rep <- ifelse(is.na(sen$senate_republican_raw_votes),1,0)

sen$missing_county_data_presidential <- ifelse(is.na(sen$presidential_democratic_raw_votes) & is.na(sen$presidential_republican_raw_votes),1,0)
sen$missing_county_data_dem_presdential <- ifelse(is.na(sen$presidential_democratic_raw_votes),1,0)
sen$missing_county_data_rep_presidential <- ifelse(is.na(sen$presidential_republican_raw_votes),1,0)

sen$electoral_cycle <- ifelse(sen$election_year %in% seq(1913,2017,2), sen$election_year - 1,sen$election_year)
sen$census_region <- ifelse(sen$state %in% c("WA","OR","CA","NV","AZ","UT","CO","NM","WY","MT","ID"),"west",ifelse(sen$state %in% c("TX","OK","AR","LA","MS","AL","GA","FL","SC","NC","TN","KY","WV","VA","DC","MD","DE"),"south",ifelse(sen$state %in% c("ND","SD","NE","KS","MO","IA","MN","WI","IL","IN","OH","MI"),"midwest",ifelse(sen$state %in% c("PA","NJ","NY","CT","RI","MA","VT","NH","ME"),"northeast",ifelse(sen$state %in% c("AK","HI"),"pacific",NA)))))
sen$census_region <- factor(sen$census_region)

sen$senate_dem_two_party_vote_percent <- sen$senate_dem_two_party_vote_percent - 0.50
sen$presidential_dem_two_party_vote_percent <- sen$presidential_dem_two_party_vote_percent - 0.50

library(DataCombine)

sen <- sen[order(sen$state,sen$county_name,sen$seat_class,sen$election_year,sen$fips),]
sen <- data.frame(sen)
rownames(sen) <- NULL

sen <- slide(sen, Var = "senate_dem_two_party_vote_percent", GroupVar =c("state","county_name","fips"), slideBy = -1)
sen <- slide(sen, Var = "dem_seat", GroupVar =c("state","county_name","fips"), slideBy = -1)
sen <- slide(sen, Var = "dem_incumbency", GroupVar =c("state","county_name","fips"), slideBy = -1)
sen <- slide(sen, Var = "dem_qual_advantage_tri", GroupVar =c("state","county_name","fips"), slideBy = -1)

colnames(sen)[65:68]  <- c("lagged_seat_class_senate_dem_two_party_vote_percent","lagged_seat_class_dem_seat","lagged_seat_class_dem_incumbency","lagged_seat_class_dem_qual_advantage")

sen <- data.frame(sen)

# House

house <- read.dta13("/Users/carlosalgara/Desktop/carlos_school/PhD_UC Davis/Research/Submissions & Applications/House_Senate_Unified_Model/Data & Code/house_elex_1900_2018.dta")
house$district <- paste(house$state,house$district,sep="")
house$election_year <- house$year

house$decade <- ifelse(house$election_year %in% seq(1900,1909,1),-1,ifelse(house$election_year %in% seq(1910,1919,1),0,ifelse(house$election_year %in% seq(1920,1929,1), 1, ifelse(house$election_year %in% seq(1930,1939,1), 2, ifelse(house$election_year %in% seq(1940,1949,1), 3, ifelse(house$election_year %in% seq(1950,1959,1), 4, ifelse(house$election_year %in% seq(1960,1969,1), 5, ifelse(house$election_year %in% seq(1970,1979,1), 6, ifelse(house$election_year %in% seq(1980,1989,1), 7, ifelse(house$election_year %in% seq(1990,1999,1), 8, ifelse(house$election_year %in% seq(2000,2009,1), 9, ifelse(house$election_year %in% seq(2010,2018,1), 10, NA))))))))))))

house$decade <- factor(house$decade,labels=c("1900s","1910s","1920s","1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s"),levels=seq(-1,10,1))

source("/Users/carlosalgara/Desktop/carlos_school/PhD_UC Davis/Research/Submissions & Applications/House_Senate_Unified_Model/Analysis/two_way_fe_function.R")

# Analysis

setwd("/Users/carlosalgara/Desktop/carlos_school/PhD_UC Davis/Research/Submissions & Applications/House_Senate_Unified_Model/Results & Figures")

x <- subset(sen,sen$election_contest_type == "Coventional Two-Party Contested Election")
x <- subset(x,!is.na(x$fips))

x$electoral_cycle <- ifelse(x$election_year %in% seq(1915,2017,2),x$electoral_cycle+2,x$electoral_cycle )
x$mean_polarization_interval <- cut_width(x$mean_polarization,0.10)
x$mean_polarization_interval <- factor(x$mean_polarization_interval,levels=c("[0.45,0.55]","(0.55,0.65]","(0.65,0.75]","(0.75,0.85]"))
margins <- list()
for(i in unique(x$mean_polarization_interval)){
  y <- subset(x,x$mean_polarization_interval == i)
  y$election_year <- factor(y$election_year)
  y <- subset(y,y$fips != 9999)
  #y$dem_incumbency <- factor(y$dem_incumbency,levels=c(-1,0,1))
  #y$dem_qual_advantage_tri <- factor(y$dem_qual_advantage_tri,levels=c(-1,0,1))
  y$fips <- factor(y$fips)
  y <- subset(y,select=c(electoral_cycle,election_year,state,fips,county_name,dem_county_pres_voteshare,dem_incumbency,dem_qual_advantage_tri,dem_seat,senate_dem_two_party_vote_percent,lagged_seat_class_senate_dem_two_party_vote_percent,senate_lagged_dem_two_party_vote_percent,presidential_dem_two_party_vote_percent))
  y <- na.omit(y)
  m <- TwoWayFE(y = "senate_dem_two_party_vote_percent", Xs = c("presidential_dem_two_party_vote_percent", "dem_incumbency","dem_qual_advantage_tri","dem_seat","lagged_seat_class_senate_dem_two_party_vote_percent","senate_lagged_dem_two_party_vote_percent"),fe1 = "fips", fe2 = "election_year",dt = data.table(y),cluster = T)
  m <- data.frame(coefficients=m$coefficients,se=m$se,obs=nrow(y),df=m$df.residual,adjusted_r2=summary(m)[10])
  m$lower <- m$coefficients - qt(0.975,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$lower_90 <- m$coefficients - qt(0.95,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$upper <- m$coefficients + qt(0.975,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$upper_90 <- m$coefficients + qt(0.95,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$factor <- rownames(m)
  rownames(m) <- NULL
  m$mean_polarization_interval <- i
  m$number_counties <- length(unique(y$fips))
  m$number_election_years <- length(unique(y$election_year))
  margins[[i]] <- m
}
margins <- ldply(margins,data.frame)
margins$mean_polarization_interval <- factor(margins$mean_polarization_interval,levels=c("[0.45,0.55]","(0.55,0.65]","(0.65,0.75]","(0.75,0.85]"))

ggplot(subset(margins,margins$factor == "dem_incumbency"),aes(x=mean_polarization_interval,y=coefficients,ymin=lower,ymax=upper)) + geom_pointrange() + stat_smooth(method="loess")
ggplot(subset(margins,margins$factor == "dem_qual_advantage_tri"),aes(x=mean_polarization_interval,y=coefficients,ymin=lower,ymax=upper)) + geom_pointrange() + stat_smooth(method="loess")
ggplot(subset(margins,margins$factor == "presidential_dem_two_party_vote_percent"),aes(x=mean_polarization_interval,y=coefficients,ymin=lower,ymax=upper)) + geom_pointrange() + stat_smooth(method="loess")

senate_margins <- margins

# House Replication

x <- subset(house,house$unopposed_race == 0)
x <- subset(x,x$year >= 1900)

x$dem_vote <- x$dem_vote-0.5
x$dem_state_partisanship <- x$dem_state_partisanship-0.5
x$dvp <- (x$dvp/100) - 0.5
x$election_year <- x$year

x$mean_polarization_interval <- cut_width(x$mean_polarization,0.10)
x$mean_polarization_interval <- factor(x$mean_polarization_interval,levels=c("[0.45,0.55]","(0.55,0.65]","(0.65,0.75]","(0.75,0.85]","(0.85,0.95]"))

margins <- list()
for(i in unique(x$mean_polarization_interval)){
  y <- subset(x,x$mean_polarization_interval == i)
  y$election_year <- factor(y$year)
  #y$dem_incumbency <- factor(y$dem_incumbency,levels=c(-1,0,1))
  #y$dem_qual_advantage_tri <- factor(y$dem_qual_advantage_tri,levels=c(-1,0,1))
  y$district <- factor(y$district)
  y <- subset(y,select=c(election_year,state,dem_vote,dem_incumbency,dem_qual_advantage_tri,dem_seat,dem_state_partisanship,dvp,district))
  y <- na.omit(y)
  m <- TwoWayFE(y = "dem_vote", Xs = c("dem_state_partisanship", "dem_incumbency","dem_qual_advantage_tri","dem_seat","dvp"),fe1 = "district", fe2 = "election_year",dt = data.table(y),cluster = T)
  m <- data.frame(coefficients=m$coefficients,se=m$se,obs=nrow(y),df=m$df.residual,adjusted_r2=summary(m)[10])
  m$lower <- m$coefficients - qt(0.975,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$lower_90 <- m$coefficients - qt(0.95,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$upper <- m$coefficients + qt(0.975,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$upper_90 <- m$coefficients + qt(0.95,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$factor <- rownames(m)
  rownames(m) <- NULL
  m$mean_polarization_interval <- i
  m$number_counties <- length(unique(y$fips))
  m$number_election_years <- length(unique(y$election_year))
  margins[[i]] <- m
}
margins <- ldply(margins,data.frame)

margins$mean_polarization_interval <- factor(margins$mean_polarization_interval,levels=c("[0.45,0.55]","(0.55,0.65]","(0.65,0.75]","(0.75,0.85]","(0.85,0.95]"))

ggplot(subset(margins,margins$factor == "dem_incumbency"),aes(x=mean_polarization_interval,y=coefficients,ymin=lower,ymax=upper)) + geom_pointrange() + stat_smooth(method="loess")
ggplot(subset(margins,margins$factor == "dem_qual_advantage_tri"),aes(x=mean_polarization_interval,y=coefficients,ymin=lower,ymax=upper)) + geom_pointrange() + stat_smooth(method="loess")
ggplot(subset(margins,margins$factor == "dem_state_partisanship"),aes(x=mean_polarization_interval,y=coefficients,ymin=lower,ymax=upper)) + geom_pointrange() + stat_smooth(method="loess")

house_margins <- margins

# Baseline Models

x <- subset(house,house$unopposed_race == 0)
x <- subset(x,x$year >= 1900)

x$dem_vote <- x$dem_vote-0.5
x$dem_state_partisanship <- x$dem_state_partisanship-0.5
x$dvp <- (x$dvp/100) - 0.5
x$election_year <- x$year

y <- x
y$election_year <- factor(y$year)
#y$dem_incumbency <- factor(y$dem_incumbency,levels=c(-1,0,1))
#y$dem_qual_advantage_tri <- factor(y$dem_qual_advantage_tri,levels=c(-1,0,1))
y$district <- factor(y$district)
y <- subset(y,select=c(election_year,state,dem_vote,dem_incumbency,dem_qual_advantage_tri,dem_seat,dem_state_partisanship,dvp,district))
y <- na.omit(y)
m <- TwoWayFE(y = "dem_vote", Xs = c("dem_state_partisanship", "dem_incumbency","dem_qual_advantage_tri","dem_seat","dvp"),fe1 = "district", fe2 = "election_year",dt = data.table(y),cluster = T)
m <- data.frame(coefficients=m$coefficients,se=m$se,obs=nrow(y),df=m$df.residual,adjusted_r2=summary(m)[10])
m$lower <- m$coefficients - qt(0.975,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
m$lower_90 <- m$coefficients - qt(0.95,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
m$upper <- m$coefficients + qt(0.975,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
m$upper_90 <- m$coefficients + qt(0.95,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
m$factor <- rownames(m)
rownames(m) <- NULL
m$mean_polarization_interval <- "Pooled Model"
m$number_counties <- length(unique(y$fips))
m$number_election_years <- length(unique(y$election_year))
m$.id <- "Pooled Model"

house_margins <- rbind(house_margins,m)

# Senate

x <- subset(sen,sen$election_contest_type == "Coventional Two-Party Contested Election")
x <- subset(x,!is.na(x$fips))

x$electoral_cycle <- ifelse(x$election_year %in% seq(1915,2017,2),x$electoral_cycle+2,x$electoral_cycle )
y <- x
  y$election_year <- factor(y$election_year)
  y <- subset(y,y$fips != 9999)
  #y$dem_incumbency <- factor(y$dem_incumbency,levels=c(-1,0,1))
  #y$dem_qual_advantage_tri <- factor(y$dem_qual_advantage_tri,levels=c(-1,0,1))
  y$fips <- factor(y$fips)
  y <- subset(y,select=c(electoral_cycle,election_year,state,fips,county_name,dem_county_pres_voteshare,dem_incumbency,dem_qual_advantage_tri,dem_seat,senate_dem_two_party_vote_percent,lagged_seat_class_senate_dem_two_party_vote_percent,senate_lagged_dem_two_party_vote_percent,presidential_dem_two_party_vote_percent))
  y <- na.omit(y)
  m <- TwoWayFE(y = "senate_dem_two_party_vote_percent", Xs = c("presidential_dem_two_party_vote_percent", "dem_incumbency","dem_qual_advantage_tri","dem_seat","lagged_seat_class_senate_dem_two_party_vote_percent","senate_lagged_dem_two_party_vote_percent"),fe1 = "fips", fe2 = "election_year",dt = data.table(y),cluster = T)
  m <- data.frame(coefficients=m$coefficients,se=m$se,obs=nrow(y),df=m$df.residual,adjusted_r2=summary(m)[10])
  m$lower <- m$coefficients - qt(0.975,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$lower_90 <- m$coefficients - qt(0.95,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$upper <- m$coefficients + qt(0.975,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$upper_90 <- m$coefficients + qt(0.95,df=m$df) * m$se #https://www.econometrics-with-r.org/5-2-cifrc.html
  m$factor <- rownames(m)
  rownames(m) <- NULL
  m$mean_polarization_interval <- i
  m$number_counties <- length(unique(y$fips))
  m$number_election_years <- length(unique(y$election_year))
  m$mean_polarization_interval <- "Pooled Model"
  m$number_counties <- length(unique(y$fips))
  m$number_election_years <- length(unique(y$election_year))
  m$.id <- "Pooled Model"
  
senate_margins <- rbind(senate_margins,m)

senate_margins$chamber <- "U.S. Senate"
house_margins$chamber <- "U.S. House"

x <- rbind(senate_margins,house_margins)
x$mean_polarization_interval <- as.character(x$mean_polarization_interval)
x$mean_polarization_interval <- gsub(",","-",x$mean_polarization_interval)
x$mean_polarization_interval <- gsub("\\(","\\[",x$mean_polarization_interval)

x$mean_polarization_interval <- factor(x$mean_polarization_interval,levels=c("[0.45-0.55]","[0.55-0.65]","[0.65-0.75]","[0.75-0.85]","[0.85-0.95]","Pooled Model"))

plot <- ggplot(subset(x,x$factor == "dem_incumbency"),aes(x=mean_polarization_interval,y=coefficients,factor=chamber,group=chamber,color=chamber,shape=chamber,fill=chamber,ymin = lower, ymax = upper)) + geom_pointrange(lwd = 1/2, position = position_dodge(width=0.5)) + scale_fill_manual("",values=c("white","white")) + scale_shape_manual("",values=c(21,22)) + theme_bw() + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("",labels=c("Lowest \nPolarization \n[0.45-0.55]","Low \nPolarization \n[0.55-0.65]","Medium \nPolarization \n[0.65-0.75]","High \nPolarization \n[0.75-0.85]","Highest \nPolarization \n[0.85-0.95]","Baseline \nPooled \nModel")) + geom_linerange(aes(x= mean_polarization_interval, ymin = lower_90, ymax = upper_90), position = position_dodge(width=0.5), lwd  = 1) + scale_color_manual("",values=c("#F8766D","#00BFC4")) + theme(legend.position="bottom") + scale_y_continuous("Effect of Incumbency on Constituency Vote-Shares",breaks=seq(-0.03,0.10,0.010)) #+ geom_text(size=3.5,hjust =0,nudge_x = margins$nudge_x[margins$factor == "dem_incumbency"])
grid.newpage()
footnote <- "Two-way Fixed Effects Regression Models Presented."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.015, y = 0.25, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 9, col = "black"))) 
grid.draw(g)
ggsave("dem_incumbency_effect.png", g, width = 8, height = 5.5, units = "in")

x$factor <- ifelse(x$factor %in% c("dem_state_partisanship","presidential_dem_two_party_vote_percent"),"const_partisanship",x$factor)

plot <- ggplot(subset(x,x$factor == "const_partisanship"),aes(x=mean_polarization_interval,y=coefficients,factor=chamber,group=chamber,color=chamber,shape=chamber,fill=chamber,ymin = lower, ymax = upper)) + geom_pointrange(lwd = 1/2, position = position_dodge(width=0.5)) + scale_fill_manual("",values=c("white","white")) + scale_shape_manual("",values=c(21,22)) + theme_bw() + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("",labels=c("Lowest \nPolarization \n[0.45-0.55]","Low \nPolarization \n[0.55-0.65]","Medium \nPolarization \n[0.65-0.75]","High \nPolarization \n[0.75-0.85]","Highest \nPolarization \n[0.85-0.95]","Baseline \nPooled \nModel")) + geom_linerange(aes(x= mean_polarization_interval, ymin = lower_90, ymax = upper_90), position = position_dodge(width=0.5), lwd  = 1) + scale_color_manual("",values=c("#F8766D","#00BFC4")) + theme(legend.position="bottom") + scale_y_continuous("Effect of Constituency Partisanship on Constituency Vote-Shares") #+ geom_text(size=3.5,hjust =0,nudge_x = margins$nudge_x[margins$factor == "dem_incumbency"])
grid.newpage()
footnote <- "Two-way Fixed Effects Regression Models Presented."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.015, y = 0.25, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 9, col = "black"))) 
grid.draw(g)
ggsave("dem_partisanship_effect.png", g, width = 8, height = 5.5, units = "in")

plot <- ggplot(subset(x,x$factor == "dem_qual_advantage_tri"),aes(x=mean_polarization_interval,y=coefficients,factor=chamber,group=chamber,color=chamber,shape=chamber,fill=chamber,ymin = lower, ymax = upper)) + geom_pointrange(lwd = 1/2, position = position_dodge(width=0.5)) + scale_fill_manual("",values=c("white","white")) + scale_shape_manual("",values=c(21,22)) + theme_bw() + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("",labels=c("Lowest \nPolarization \n[0.45-0.55]","Low \nPolarization \n[0.55-0.65]","Medium \nPolarization \n[0.65-0.75]","High \nPolarization \n[0.75-0.85]","Highest \nPolarization \n[0.85-0.95]","Baseline \nPooled \nModel")) + geom_linerange(aes(x= mean_polarization_interval, ymin = lower_90, ymax = upper_90), position = position_dodge(width=0.5), lwd  = 1) + scale_color_manual("",values=c("#F8766D","#00BFC4")) + theme(legend.position="bottom") + scale_y_continuous("Effect of Candidate Quality on Constituency Vote-Shares") #+ geom_text(size=3.5,hjust =0,nudge_x = margins$nudge_x[margins$factor == "dem_incumbency"])
grid.newpage()
footnote <- "Two-way Fixed Effects Regression Models Presented."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.015, y = 0.25, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 9, col = "black"))) 
grid.draw(g)
ggsave("dem_quality_effect.png", g, width = 8, height = 5.5, units = "in")

# Polarization Over Time

library(Rvoteview)

ridges <- list()
for(i in seq(56,115,1)){
  x <- member_search("",chamber = c("House"), congress = i)
  x$chamber <- "U.S. House"
  y <- member_search("",chamber = c("Senate"), congress = i)
  y$chamber <- "U.S. Senate"
  congress <- rbind(x,y)
  congress <- data.frame(nominate.dim1=congress$nominate.dim1,party=congress$party_name,name=congress$bioname,party_code=congress$party_code,chamber=congress$chamber)
  congress$party <- as.character(congress$party)
  congress$party <- ifelse(congress$party_code %in% 328,"Democratic Party",congress$party)
  congress <- subset(congress,congress$party %in% c("Democratic Party","Republican Party"))
  #congress <- x
  congress$cong <- i
  ridges[[i]] <- congress
}

ridges <- ldply(ridges,data.frame)
ridges$party <- as.character(ridges$party)
ridges$counter <- (ridges$cong - 56) * 2
ridges$year <- 1900 + ridges$counter

ridges <- ddply(ridges,.(year,cong,party,chamber),summarize,nominate.dim1=mean(nominate.dim1,na.rm=T))
house_polarization <- subset(ridges,ridges$chamber == "U.S. House")
senate_polarization <- subset(ridges,ridges$chamber == "U.S. Senate")

senate_polarization$chamber <- NULL
house_polarization$chamber <- NULL

house_polarization <- reshape(house_polarization, idvar = c("year","cong"), timevar = "party", direction = "wide")
senate_polarization <- reshape(senate_polarization, idvar = c("year","cong"), timevar = "party", direction = "wide")

house_polarization$ideo_polarization <- abs(house_polarization[,3] - house_polarization[,4])
senate_polarization$ideo_polarization <- abs(senate_polarization[,3] - senate_polarization[,4])

senate_polarization$chamber <- "U.S. Senate"
house_polarization$chamber <- "U.S. House"

polarization <- rbind(senate_polarization,house_polarization)

plot <- ggplot(polarization,aes(x=year,y=ideo_polarization,group=chamber,color=chamber,linetype=chamber,shape=chamber)) + theme_bw() + geom_point() + scale_color_manual("",values=c("red","blue")) + scale_shape_manual("",values=c(21,23)) + stat_smooth(method = "loess", se=T, size = 0.75) + scale_linetype_manual("",values=c("solid","dashed")) + scale_x_continuous("",breaks=seq(1900,2018,6)) + scale_y_continuous("Mean Ideological Polarization",breaks=seq(0.45,0.90,0.05),limits=c(0.42,0.94)) + theme(legend.position=c(0.10,0.15)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + guides(color=guide_legend(override.aes=list(fill=NA)))
grid.newpage()
footnote <- "Polarization measured as absolute difference between first dimension DW-NOMINATE party means."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.015, y = 1, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 9, col = "black"))) 
grid.draw(g)
ggsave("polarization_overtime.png", g, width = 8, height = 5.5, units = "in")

# Correlations

x <- subset(sen,sen$election_contest_type == "Coventional Two-Party Contested Election")
x <- subset(x,!is.na(x$fips))

senate_corrs <- list()
for(i in seq(1914,2018,2)){
  y <- cor(sen$senate_dem_two_party_vote_percent[sen$election_year == i],sen$presidential_dem_two_party_vote_percent[sen$election_year == i],use="complete.obs")
  c <- cor.test(sen$senate_dem_two_party_vote_percent[sen$election_year == i], sen$presidential_dem_two_party_vote_percent[sen$election_year == i], method = "pearson", conf.level = 0.9)
  y<-data.frame(y)
  y$year <- i
  colnames(y)[1] <- "corr_senate_pres_county"
  y$lower <- c$conf.int[1]
  y$upper <- c$conf.int[2]
  senate_corrs[[i]] <- y
}
senate_corrs <- ldply(senate_corrs,data.frame)

ggplot(senate_corrs,aes(x=year,y=corr_senate_pres_county,ymin=lower,ymax=upper)) + geom_point(shape=1) + stat_smooth(method="loess")

x <- subset(house,house$unopposed_race == 0)
x <- subset(x,x$year >= 1900)
x$median_polarization_quartile <- factor(cut_number(x$mean_polarization,10),labels=seq(1,10,1))
x$electoral_cycle <- x$year

house_corrs <- list()
for(i in seq(1900,2018,2)){
  y <- cor(house$dem_vote[house$year == i],house$dem_state_partisanship[house$year == i],use="complete.obs")
  c <- cor.test(house$dem_vote[house$year == i], house$dem_state_partisanship[house$year == i], method = "pearson", conf.level = 0.9)
  y<-data.frame(y)
  y$year <- i
  colnames(y)[1] <- "corr_senate_pres_county"
  y$lower <- c$conf.int[1]
  y$upper <- c$conf.int[2]
  house_corrs[[i]] <- y
}
house_corrs <- ldply(house_corrs,data.frame)

# Aggregate Senate elecitons

senate_dataset <- read.dta13("/Users/carlosalgara/Desktop/carlos_school/PhD_UC Davis/Research/Submissions & Applications/House_Senate_Unified_Model/Data & Code/senate_elex_1914_2018.dta")
x <- senate_dataset
x <- subset(x,x$unopposed_race == 0)
x <- subset(x,x$major_thirdpartyrace == 0)

agg_senate_corrs <- list()
for(i in seq(1914,2018,2)){
  y <- cor(x$dem_vote[x$year == i],x$d_presvote_twoparty[x$year == i],use="complete.obs")
  c <- cor.test(x$dem_vote[x$year == i],x$d_presvote_twoparty[x$year == i], method = "pearson", conf.level = 0.9)
  y<-data.frame(y)
  y$year <- i
  colnames(y)[1] <- "corr_senate_pres_county"
  y$lower <- c$conf.int[1]
  y$upper <- c$conf.int[2]
  agg_senate_corrs[[i]] <- y
}
agg_senate_corrs <- ldply(agg_senate_corrs,data.frame)

# Correlations

house_corrs$model <- "U.S. House"
agg_senate_corrs$model <- "U.S. Senate Aggregate"
senate_corrs$model <- "U.S. Senate County"

correlations <- rbind(house_corrs,agg_senate_corrs,senate_corrs)

plot <- ggplot(correlations,aes(x=year,y=corr_senate_pres_county,group=model,factor=model,color=model,linetype=model,ymin=lower,ymax=upper,shape=model)) + geom_point() + geom_smooth(method="loess",show.legend = F) + theme_bw() + theme(legend.position="bottom") + scale_color_manual("",values=c("coral3","dodgerblue3","slategray")) + scale_shape_manual("",values=c(1,5,0)) + scale_linetype_manual("",values=c("solid","dashed","dotdash")) + annotate("label", x = 2004, y = 1, label = "U.S. House",color="coral3") + annotate("label", x = 1914, y = 1.05, label = "U.S. Senate County",color="slategray") + annotate("label", x = 2006, y = 0.25, label = "U.S. Senate State",color="dodgerblue3") + scale_x_continuous("",breaks=seq(1900,2018,10)) + scale_y_continuous("Pearson's Correlation Coefficient",breaks=seq(-.5,1,0.25),limits=c(-0.3,1.13)) + guides(color=guide_legend(override.aes=list(fill=NA))) #+ ggtitle("Correlation Between District Presidential & Congressional Preference, 1900-2018") + guides(color=guide_legend(override.aes=list(fill=NA)))
grid.newpage()
footnote <- "Data from Two-Party Contested U.S. House and U.S. Senate Races."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.015, y = 0.25, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 9, col = "black"))) 
grid.draw(g)
ggsave("correlations.png", g, width = 8, height = 5.5, units = "in")

# Partisan Continuity in Seat

y <- subset(senate_dataset,senate_dataset$unopposed_race == 0)
y <- subset(y,y$major_thirdpartyrace == 0)

y$partisan_continuity <- ifelse(y$d_presvote_twoparty >= 0.5 & y$dem_vote >= 0.5,1,ifelse(y$d_presvote_twoparty < 0.5 & y$dem_vote < 0.5,1,0))

house$partisan_continuity <- ifelse(house$dem_vote >= 0.5 & house$dem_state_partisanship >= 0.5,1,ifelse(house$dem_vote < 0.5 & house$dem_state_partisanship < 0.5,1,0))

x <- subset(sen,!is.na(sen$fips))
x <- subset(x,x$major_thirdpartyrace == 0)
x <- subset(x,x$unopposed_race == 0)

x$partisan_continuity <- ifelse(x$senate_dem_two_party_vote_percent >= 0 & x$presidential_dem_two_party_vote_percent >= 0,1,ifelse(x$senate_dem_two_party_vote_percent < 0 & x$presidential_dem_two_party_vote_percent < 0,1,0))

x <- ddply(x, .(election_year), summarize, partisan_continuity =mean(partisan_continuity, na.rm = T))
h <- ddply(house, .(year), summarize, partisan_continuity =mean(partisan_continuity, na.rm = T))
y <-  ddply(y, .(year), summarize, partisan_continuity =mean(partisan_continuity, na.rm = T))

x$model <- "U.S. Senate County"
h$model <- "U.S. House"
y$model <- "U.S. Senate State"

colnames(x)[1] <- "year"

continuity <- rbind(x,h,y)

plot <- ggplot(subset(continuity,continuity$year %in% seq(1900,2018,2)),aes(x=year,y=partisan_continuity,group=model,factor=model,color=model,linetype=model,shape=model)) + geom_point() + geom_smooth(method="loess",show.legend = F) + theme_bw() + theme(legend.position="bottom") + scale_color_manual("",values=c("coral3","dodgerblue3","slategray")) + scale_shape_manual("",values=c(1,5,0)) + scale_linetype_manual("",values=c("solid","dashed","dotdash")) + annotate("label", x = 2004, y = 1, label = "U.S. House",color="coral3") + annotate("label", x = 1914, y = 1.05, label = "U.S. Senate County",color="dodgerblue3") + annotate("label", x = 2006, y = 0.45, label = "U.S. Senate State",color="slategray") + scale_x_continuous("",breaks=seq(1900,2018,10)) + scale_y_continuous("Degree of Partisan Continuty") + ggtitle("Consistency between Congressional and Presidential Partisan Preference by Constituency, 1900-2018")
grid.newpage()
footnote <- "Data from Two-Party Contested U.S. House and U.S. Senate Races."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.015, y = 0.25, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 9, col = "black"))) 
grid.draw(g)
ggsave("partisan_continuity.png", g, width = 8, height = 5.5, units = "in")

x <- subset(sen,!is.na(sen$fips))
x <- subset(x,x$major_thirdpartyrace == 0)
x <- subset(x,x$unopposed_race == 0)

x$abs_diff_senate_pres_dem_vote <- abs(x$senate_dem_two_party_vote_percent - x$presidential_dem_two_party_vote_percent)
senate_county_diff <-  ddply(x, .(election_year), summarize, average_difference =mean(abs_diff_senate_pres_dem_vote, na.rm = T))

x <- subset(house,house$unopposed_race == 0)
x$abs_diff_senate_pres_dem_vote <- abs(x$dem_vote - x$dem_state_partisanship)
house_district_diff <-  ddply(x, .(year), summarize, average_difference =mean(abs_diff_senate_pres_dem_vote, na.rm = T))

x <- senate_dataset
x <- subset(x,x$unopposed_race == 0)
x <- subset(x,x$major_thirdpartyrace == 0)
x$abs_diff_senate_pres_dem_vote <- abs(x$dem_vote - x$d_presvote_twoparty)
senate_agg_diff <-  ddply(x, .(year), summarize, average_difference =mean(abs_diff_senate_pres_dem_vote, na.rm = T))

senate_county_diff$measure <- "U.S. Senate: County-Level"
senate_agg_diff$measure <- "U.S. Senate: State-Level"
house_district_diff$measure <- "U.S. House: District-Level"

colnames(senate_county_diff)[1] <- "year"
x <- rbind(senate_county_diff,senate_agg_diff,house_district_diff)

plot <- ggplot(subset(x,x$year %in% seq(1900,2018,2)),aes(x=year,y=average_difference,group=measure,factor=measure,color=measure,linetype=measure,shape=measure)) + geom_point() + geom_smooth(method="loess") + theme_bw() + theme(legend.position="none") + scale_color_manual("",values=c("coral3","dodgerblue3","slategray")) + scale_shape_manual("",values=c(1,5,0)) + scale_linetype_manual("",values=c("solid","dashed","dotdash")) + scale_x_continuous("",breaks=seq(1900,2018,20)) + scale_y_continuous("Absolute Difference in Congressional and Presidential Two-Party Vote-Share",breaks=seq(0,0.25,0.05)) + facet_wrap(~measure) #+ labs(title=" Difference in Congressional and Presidential Constituency Vote-Share, 1900-2018",caption="Data from Two-Party Contested U.S. House and U.S. Senate Races")
grid.newpage()
footnote <- "Data from Two-Party Contested U.S. House and U.S. Senate Races."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.015, y = 0.25, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 9, col = "black"))) 
grid.draw(g)
ggsave("difference_in_congresional_presidential_vote.png", g, width = 12, height = 6, units = "in")

# Adjusted R2 of the models

x <- subset(house,house$unopposed_race == 0)
x <- subset(x,x$year >= 1900)

x$dem_vote <- x$dem_vote-0.5
x$dem_state_partisanship <- x$dem_state_partisanship-0.5
x$dvp <- (x$dvp/100) - 0.5
x$election_year <- x$year

house_margins <- list()
house_r2 <- list()
for(i in seq(1900,2018,2)){
  y <- subset(x,x$year == i)
  print(summary(model <- lm(dem_vote~dem_state_partisanship + dem_incumbency + dem_qual_advantage_tri+ dem_seat + dvp,data=y)))
  mes <- data.frame(summary(margins(model, variables=c("dem_state_partisanship","dem_incumbency","dem_qual_advantage_tri"), change="dydx", vcov.=cluster.vcov(model, cluster=y$state))))
  mes$year <- i
  house_margins[[i]] <- mes  
  model <- data.frame(r2=summary(model)$r.squared,adjusted_r2=summary(model)$adj.r.squared,n=nobs(model),year=i,mean_polarization=unique(y$mean_polarization))
  house_r2[[i]] <- model
}

# Senate

x <- subset(sen,sen$election_contest_type == "Coventional Two-Party Contested Election")
x <- subset(x,!is.na(x$fips))

x$electoral_cycle <- ifelse(x$election_year %in% seq(1915,2017,2),x$electoral_cycle+2,x$electoral_cycle )

senate_margins <- list()
senate_r2 <- list()
for(i in seq(1914,2018,2)){
  y <- subset(x,x$election_year == i)
  y$election_year <- factor(y$election_year)
  y <- subset(y,y$fips != 9999)
  #y$dem_incumbency <- factor(y$dem_incumbency,levels=c(-1,0,1))
  #y$dem_qual_advantage_tri <- factor(y$dem_qual_advantage_tri,levels=c(-1,0,1))
  y$fips <- factor(y$fips)
  y$state <- factor(as.character(y$state))
  print(summary(model <- lm(senate_dem_two_party_vote_percent~presidential_dem_two_party_vote_percent + dem_incumbency + dem_qual_advantage_tri+ dem_seat + lagged_seat_class_senate_dem_two_party_vote_percent + senate_lagged_dem_two_party_vote_percent,data=y)))
  mes <- data.frame(summary(margins(model, variables=c("presidential_dem_two_party_vote_percent","dem_incumbency","dem_qual_advantage_tri"), change="dydx", vcov.=cluster.vcov(model, cluster=y$state))))
  mes$electoral_cycle <- i
  senate_margins[[i]] <- mes  
  model <- data.frame(r2=summary(model)$r.squared,adjusted_r2=summary(model)$adj.r.squared,n=nobs(model),year=i,mean_polarization=unique(y$mean_polarization))
  senate_r2[[i]] <- model
}

senate_r2 <- ldply(senate_r2,data.frame)
house_r2 <- ldply(house_r2,data.frame)