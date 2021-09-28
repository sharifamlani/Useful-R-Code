

#Percentage on the Y-axis of a Histogram

ggplot(Money.Cosponsor.1,aes(x=Norm.Num.Com.Donors, y =(..count../sum(..count..))*100)) +
  geom_histogram(fill = "grey30", color= "grey30", alpha = 0.3, position="identity",  binwidth=density(Money.Cosponsor.1$Norm.Num.Com.Donors)$bw) +
  theme_bw()