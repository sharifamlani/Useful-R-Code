

#Working with text:
#How to separte a string and take certian element. In this case the second element.

sapply(strsplit(rownames(plot_df), " "), `[`, 2)

#Chnage the 2 to a 1 to get the first element


#Spliting on a period
sapply(strsplit(as.character(df_1$variable), "[.]"), `[`, 1)
