x <- seq(1, 50, by = 1)
y <- rnorm(mean = 0, sd = 1, n = 50)
z <- runif(n = 50, min = 0, max = 1000)

df <- data.frame(x, y, z)
head(df)

#loop accross columns = Double [[]]

for(i in colnames(df)){
  
  print(head(df[[i]]))
}
#USE THIS ONE^^


#Takes the columns themselves = single [[]]
for(i in colnames(df)){
  
  print(head(df[i]))
}