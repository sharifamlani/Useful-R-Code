library(ggplot2)
library(gridExtra)

P1 <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram()

P2 <- ggplot(mtcars, aes(x = wt)) +
  geom_histogram()

P3 <- ggplot(mtcars, aes(x = qsec)) +
  geom_histogram()


grid.arrange(P1, P2, P3, ncol = 2, nrow = 2)


#Answer:
grid.arrange(P1, P2, P3, ncol = 2, nrow = 2, layout_matrix= rbind(c(1,2), 3))
(P1 + P2) / P3

#See: https://stackoverflow.com/questions/65985705/defining-grid-arrange-so-the-third-plot-is-in-the-middle/65985832#65985832
#See: https://patchwork.data-imaginist.com/ 