#https://stackoverflow.com/questions/12018499/how-to-put-labels-over-geom-bar-for-each-bar-in-r-with-ggplot2

#How to put labels over geom_bar for each bar in R with ggplot2
geom_text(aes(label=Number), position=position_dodge(width=0.9), vjust=-0.25)
