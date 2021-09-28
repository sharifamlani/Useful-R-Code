#****************** Mediation Analysis *************************
#https://towardsdatascience.com/doing-and-reporting-your-first-mediation-analysis-in-r-2fe423b92171

#M = Amount Percentage
#I = Discontinuity
#D = Absolute Differnce

#Step 1: Total Effect (y ~ x)
Results <- lm(Abs_distance ~ Discontinuity_New, data = subset(Dem_Data, Overall_Type == "All" & Donor_Location %in% c("Out of District"))); summary(Results)

#Step 2: The effect of the IV onto the mediator (m ~ x)
Results <- lm(Amount_Percentage ~ Discontinuity_New, data = subset(Dem_Data, Overall_Type == "All" & Donor_Location %in% c("Out of District"))); summary(Results)

#Step 3: The effect of the mediator on and the IV on the dependent variable (y ~ x + m)
Results <- lm(Abs_distance ~ Discontinuity_New +Amount_Percentage, data = subset(Dem_Data, Overall_Type == "All" & Donor_Location %in% c("Out of District"))); summary(Results)

