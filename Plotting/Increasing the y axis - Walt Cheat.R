


ggplot(subset(ValenceFinal_Rep), aes(x= factor(ExpertGender), y= Mean, fill =  CandidGender)) + 
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(Type_Facet ~ CandidParty_facet) +
  scale_fill_manual(name= "Candidates' Gender", labels = c("Male", "Female"), values=c("dodgerblue1","lightcoral")) +
  theme_minimal() + 
  ggtitle("Quality Scores By Republican Activists") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=12)) +
  scale_y_continuous(breaks = seq(2, 6, by = 2)) +
  coord_cartesian(ylim=c(2,6)) +
  ylab("Average Quality Scores") +
  xlab("Republican Activists' Gender") +
  labs(caption = "Source: 2010 UC Davis Election Study") 