#Ggridges

library(ggplot2)
library(ggridges)
library(gridExtra)
theme_set(theme_ridges())


ggplot(data = House_Final, aes(x = nominate.dim1, y =reorder(as.character(congress), congress),
                               fill = as.character(party_code)))+
  geom_density_ridges(alpha = 1, scale = 7)+
  scale_fill_manual(values = c("dodgerblue", "red"), name = 'Party',labels = c('Democrat','Republican')) +
  labs(title = "Ideological Polarization",
       subtitle = "House of Representives",
       y = 'Congress',
       x = "DW-Nominate (First Dimension)",
       caption = "Source: VoteView") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position="bottom", 
        legend.box = "vertical")


theme_set(theme_grey())