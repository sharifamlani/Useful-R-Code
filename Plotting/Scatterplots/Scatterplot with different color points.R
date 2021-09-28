#Scatterplot 
#Perfect for showing Party Differences 


ggplot(Con88.plot, aes(x=SEC, y=agreement)) +
  geom_point(aes(x=SEC, y=agreement, color= partynamei))+
  scale_color_manual("Party", values =c("Blue", "Red")) +
  geom_smooth(method = 'lm', level=0.999) +
  theme_bw() +
  labs(x = "Structural Equivalence", 
       y = "Network Agreement")  +
  ggtitle("Structural Equivalence and Network Agreement in 88th Congress")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))