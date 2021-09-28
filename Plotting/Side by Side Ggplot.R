#****************** Side By Side: Box Plot and CDF *****************

Plot7<- ggplot(Races_per_Donor, aes(y=Races)) +
  geom_boxplot() +
  guides(fill=FALSE) + 
  coord_flip() +
  theme_bw() +
  labs( y = "Number of Races",
        title = "Number of Races Contributed to by California Ballot Donors \n\nBoxplot") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(Races_per_Donor$Races), by = 5)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank());Plot7

Plot8 <- ggplot(Races_per_Donor, aes(x=Races) )+
  stat_ecdf()+
  theme_bw() +
  labs(x = "Number of Races", y = "Cumulative Precent of Donors",
       title = "Cumulative Density Plot ") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, max(Races_per_Donor$Races), by = 5));Plot8

library(gridExtra)
grid.arrange(Plot7, Plot8, ncol=1, nrow = 2)
ggsave(grid.arrange(Plot7, Plot8, ncol=1, nrow = 2), file = "CDF and Boxplot.png",
       width=9, height=6,  dpi = 300, units = "in")