# Plotting Interactions With ggplot

########### Incumbent Party Manuel Plot ##############
<- function(temp_F) {

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

predit.output <- predict(% Model Name %, newdata = newdt,se.fit = TRUE, type = "response")
predit.values <- data.frame(fit = predit.output$fit, se = predit.output$se.fit)


## Combining Data Frames
predit.values2 <- as.data.frame(cbind(newdt,predit.values))
predit.values2

## Assiging Labels
table(predit.values2$ % Moderating Variable %)

predit.values2$labels <- ifelse(predit.values2$% Moderating Variable % == "1", 
                        "Republican", 
                        "Democrat")
table(predit.values2$labels)



## Plot the REsult
ggplot(data = predit.values2,aes(x= % X Variable %,
                         y=fit, 
                         fill=labels, 
                         linetype=labels, 
                         group=labels)) +
  geom_line(aes(x= % X Variable %,
                y=fit,
                linetype=labels), 
            size=.71) +
  geom_ribbon(aes(ymin=fit-1.96*se,ymax=fit+1.96*se,fill=labels),alpha=0.5)

