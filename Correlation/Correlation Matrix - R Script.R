
#################### **Correlation Matrix** ###############
library(Hmisc)


Error.Val.Ideo.Corr <- with(Valence.1, data.frame(Ideo.Can.zero.dist, 
                                                  Val.Can.zero.dist, 
                                                  Ideo.Valence.zero, 
                                                  Error))

rcorr(as.matrix(Error.Val.Ideo.Corr))