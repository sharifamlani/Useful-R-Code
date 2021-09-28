# Plotting Interactions With ggplot

########### Incumbent Party Manuel Plot ##############
ind1s <- with (%Data Frame%, 
               seq(
                  quantile(% X Variable %, probs = 0, na.rm = TRUE),
                  quantile(% X Variable %, probs = 1, na.rm = TRUE),
                  length = 20))

ind1s <- rep(ind1s,2)
ind2s <- c(rep(%Value 1 of Moderating Varable%,20),
           rep(%Value 2 of Moderating Varable%,20))

newdt <- with (%Data Frame%, 
                        data.frame(
                                    % Moderating Variable % = ind2s, 
                                    % X Variable % = ind1s #,
                                    
                                    %Control Variable %
                                   #Inc.party = 'Democratic',
                                   #Party = 'Strict Independent'
))

logitpred <- predict(% Model Name %, newdata = newdt,se.fit = TRUE, type = "response")
lprdt <- data.frame(fit = logitpred$fit, se = logitpred$se.fit)


## Combining Data Frames
lprdt2 <- as.data.frame(cbind(newdt,lprdt))
lprdt2

## Assiging Labels
table(lprdt2$ % Moderating Variable %)

lprdt2$labels <- ifelse(lprdt2$% Moderating Variable % == "1", 
                        "Republican", 
                        "Democrat")
table(lprdt2$labels)



## Plot the REsult
ggplot(data = lprdt2,aes(x= % X Variable %,
                         y=fit, 
                         fill=labels, 
                         linetype=labels, 
                         group=labels)) +
  geom_line(aes(x= % X Variable %,
                y=fit,
                linetype=labels), 
            size=.71) +
  geom_ribbon(aes(ymin=fit-1.96*se,ymax=fit+1.96*se,fill=labels),alpha=0.5)



##################### Example ####################

ind1s <- with (Valence, seq(quantile(Incval_pc09.recode, probs = 0, na.rm = TRUE),
                            quantile(Incval_pc09.recode, probs = 1, na.rm = TRUE),
                            length = 20))
ind1s <- rep(ind1s,2)
ind2s <- c(rep(1,20),rep(2,20))

newdt <- with (Valence, data.frame(Inc.party.n = ind2s, Incval_pc09.recode = ind1s #,
                                   #Inc.party = 'Democratic',
                                   #Party = 'Strict Independent'
))

logitpred <- predict(OLS.inc.int, newdata = newdt, se.fit = TRUE, type = "response")
lprdt <- data.frame(fit = logitpred$fit, se = logitpred$se.fit)


## Combining Data Frames
lprdt2 <- as.data.frame(cbind(newdt,lprdt))
lprdt2

## Assiging Labels
table(lprdt2$Incval_pc09.recode)

lprdt2$labels <- ifelse(lprdt2$Inc.party.n == "1", 
                        "Republican", 
                        "Democrat")
table(lprdt2$labels)



## Plot the REsult
ggplot(data = lprdt2,aes(x=Incval_pc09.recode,y=fit, fill=labels, linetype=labels, group=labels)) +
  geom_line(aes(x=Incval_pc09.recode,y=fit,linetype=labels), size=.71) +
  geom_ribbon(aes(ymin=fit-1.96*se,ymax=fit+1.96*se,fill=labels),alpha=0.5)





