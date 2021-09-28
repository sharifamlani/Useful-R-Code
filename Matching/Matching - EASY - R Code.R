##################### Matching in R ##################

#MC.Resp.SameRace == Treatment Variable
#ContactRep.recode == Dependent Variable
#CMPS.Cong.Match = Data set
#PreTreat_cov == My Pre-Treatment Covariates to be matched


######################## *** Matching *** ######################
library(MatchIt)
library(dplyr)

#*********************Create Matching Data frame
CMPS.Cong.Match <- CMPS.Cong.1


#********* Difference-in-means: outcome variable
CMPS.Cong.Match %>%
  group_by(MC.Resp.SameRace) %>%
  summarise(n_Descriptive = n(),
            mean_contact = mean(as.numeric(ContactRep.recode)),
            std_error = sd(ContactRep.recode) / sqrt(n_Descriptive))

#************ T-TEST: Difference-in-means 
#T-Test
t.test(CMPS.Cong.Match$MC.Resp.SameRace ~ CMPS.Cong.Match$ContactRep.recode)


#*********** Difference-in-means: pre-treatment covariates


PreTreat_cov <- c(#Same Party
  "MC.Resp.SameParty", 
  
  #Same Ideology
  "MC.Resp.SameIdeo",
  
  #Education
  "Education",  
  
  #Income
  "Income", 
  
  #Voted
  "Voted.num",
  
  #Political Interest
  "VoteOften",
  "VoteImportance.Recode",
  "PolInterest.Recode",
  
  #Group Participation
  "GroupPart.recode",
  
  #Political Trust
  "PolHelp.recode",
  "PolChange.recode",
  "PolUnderstand.recode",
  
  #Particpation
  "WorkonCamp.num",
  "Donate.num")

CMPS.Cong.Match %>%
  group_by(MC.Resp.SameRace) %>%
  select(one_of(PreTreat_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))



#*********************** T-TEST: PRE-TREATMENT COVARIATES

lapply(PreTreat_cov, function(v) {
  t.test(CMPS.Cong.Match[, v] ~ CMPS.Cong.Match[, 'MC.Resp.SameRace'])
})

#************************** Propensity score estimation
#Propensity Score estimation: include any covariate that is related to both the treatment assignment and potential outcomes. 

#GLM Propensity Score Model
Matching.PS <- glm(MC.Resp.SameRace ~ MC.Resp.SameParty  +  MC.Resp.SameIdeo  + Education  +  
                     Income  +  Voted.num  + VoteOften  + VoteImportance.Recode  +
                     PolInterest.Recode  + GroupPart.recode  +  PolHelp.recode  +
                     PolChange.recode  +  PolUnderstand.recode  + WorkonCamp.num  +
                     Donate.num, 
                   family = binomial(), data = CMPS.Cong.Match)
summary(Matching.PS)

#GLM Propensity Score Predicited Probabiliy
prs_df <- data.frame(pr_score = predict(Matching.PS, type = "response"),
                     MC.Resp.SameRace = Matching.PS$model$MC.Resp.SameRace)
head(prs_df)

#************************** Examining the region of common support
library(ggplot2)
library(ggthemes)
dev.off()
labs <- paste("Descriptive Repersenation:", c("No", "Yes"))
prs_df %>%
  mutate(MC.Resp.SameRace = ifelse(MC.Resp.SameRace == 0, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~MC.Resp.SameRace) +
  xlab("Probability of having Descriptive Repersenation") +
  theme_bw()


#*********************** Executing a matching algorithm
library(MatchIt)

#MatchIt does not allow missing values
CMPS.Cong.Match.nomiss <- CMPS.Cong.Match %>%  
  select(ContactRep.recode, MC.Resp.SameRace, one_of(PreTreat_cov)) %>%
  na.omit()



mod_match <- matchit(MC.Resp.SameRace ~ MC.Resp.SameParty  +  MC.Resp.SameIdeo  + Education  +  
                       Income  +  Voted.num  + VoteOften  + VoteImportance.Recode  +
                       PolInterest.Recode  + GroupPart.recode  +  PolHelp.recode  +
                       PolChange.recode  +  PolUnderstand.recode  + WorkonCamp.num  +
                       Donate.num,
                     method = "nearest", data = CMPS.Cong.Match.nomiss)

