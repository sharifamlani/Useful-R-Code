#Instramental Variables Analysis with IV package
library(AER)

#Formula
ivmodel=ivreg(Dependent Variable ~ Independent Variable + Control, 
              
              ~ Instramental Variable + Controls,
              
              x=TRUE, data=card.data)

#Example
ivmodel=ivreg(lwage ~ educ + 
                
                exper + expersq + black + south + smsa + reg661 + reg662 +
                reg663 + reg664 + reg665+ reg666 + reg667 + reg668 + smsa66, 
              
              ~ nearc4 + 
                
                exper + expersq + black + south + smsa + reg661+ reg662 + 
                reg663 + reg664 + reg665 + reg666 +reg667 + reg668 + smsa66,
              
              x=TRUE, data=card.data)