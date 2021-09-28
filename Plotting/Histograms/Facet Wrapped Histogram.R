#Facet Wrapped Histogram - Perfect for Differnces between parties

##REMEMBER: To use "Fill" or "Color", GGplot will not take numbered codes, 
          # you have to make them into WORDS

ggplot(Con88.plot, aes(x=SEC, fill=partynamei, color =partynamei)) +
  geom_histogram(position="identity", alpha = c(.7))+ 
  scale_fill_manual(name= "Party", values=c("blue", "red")) +
  scale_color_manual(name = "Party",values=c("blue", "red")) +
  facet_grid(partynamei ~ .) +
  theme_bw() +
  labs(x = "Structural Equivalence", 
       y = "Count")  +
  ggtitle("Histogram of Structural Equivalence in 88th Congress")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) 
