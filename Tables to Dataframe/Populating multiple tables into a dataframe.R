#Populating multiple tables into a dataframe

RespondentsYear <- list()

CDYear <- c("2006",
            "2007",
            "2008",
            "2009",
            "2010",
            "2011",
            "2012")

for(i in CDYear) {
  
  alpha <- subset(CCES.ALLPARTY, year == i)
  
  RespondentsYear[[i]] <- as.matrix(table(alpha$CD.pre))
  
}

RespondentsYear

RespondentsYear$`2012`