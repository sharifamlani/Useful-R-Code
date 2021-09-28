######################## 3. Model  ###############################

fresh_submodel = ergmm(fresh_subnet ~ euclidean(d = 2) + #try with one and two dimensions, got interesting differences
                         rsociality + 
                         nodematch("party.x") + nodematch("state.x") +
                         edgecov(as.sociomatrix(fresh_subnet, attrname = "ideo.diff"),
                                 attrname = "ideo.diff") +
                         edgecov(as.sociomatrix(fresh_subnet, attrname = "Num.Com.Donors"), 
                                 attrname = "Num.Com.Donors"),  
                       response = "Num.Bill.CS",
                       family = "Poisson",
                       control = control.ergmm(sample.size = 5000, burnin = 50000, interval = 20)) #prob go even high

######################## 4. Look at results  ###############################
mcmc.diagnostics(fresh_submodel)

summary(fresh_submodel)

data.frame(name = fresh_subnet%v%"vertex.names", soc = fresh_submodel$mkl$sociality,
           state = fresh_subnet%v%"state.x") %>% arrange(soc)

####################### 5. Plot #############################

#************Base Plot
plot(fresh_submodel, vertex.col = "party.x", plot.means = F, plot.vars = F,
     vertex.cex = (fresh_submodel$mkl$sociality - min(fresh_submodel$mkl$sociality) + 1),
     #edge.col = 3 + as.numeric(as.factor(fresh_subnet%e%"Num.Com.Donors_factor")),
     labels = T, print.formula = F, main = "Latent Space Positions", xlab = "First Dimension", ylab = "Second Dimension")

#***********Plot with ggplot2
Output_df <- data.frame(cbind(fresh_submodel$mkl$Z, as.numeric(fresh_submodel$mkl$sociality), fresh_subnet%v%"vertex.names", fresh_subnet%v%"state.x", fresh_subnet%v%"party.x"))
head(Output_df)

colnames(Output_df) <- c("D1", "D2", "Soc", "Name", "State", "Party")

Output_df$D1 <- as.numeric(Output_df$D1)
Output_df$D2 <- as.numeric(Output_df$D2)
Output_df$Soc <- as.numeric(Output_df$Soc)
Output_df$Name <- factor(Output_df$Name)
Output_df$State <- factor(Output_df$State)
Output_df$Party <- factor(Output_df$Party)


library(ggplot2)
ggplot2::ggplot(data = Output_df, aes(x= D1, y = D2, color = Party, label=Name)) +
  geom_point(size = (Output_df$Soc +3)) +
  # geom_text(aes(label=Name),hjust=0, vjust=0) +
  scale_x_continuous(name = "First Dimension", limits = c(min(Output_df$D1) , (round(max(Output_df$D1)) +.1))) +
  scale_y_continuous(name = "Second Dimension") +
  scale_color_manual(name= "Party", labels = c("Democrat", "Republican"), values=c("dodgerblue","coral"), guide = guide_legend()) +
  theme(legend.position="bottom") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  ggtitle("Latent Space Position of Freshman MC (2002)") 