#How to create multiple data frames in a loop

for(i in unique(df$y)) {
  nam <- paste("df", i, sep = ".")
  assign(nam, df[df$y==i,])
}