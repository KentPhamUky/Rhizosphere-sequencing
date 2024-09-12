#lefse plot
library(tidyverse)
library(readxl)


lefse <- read_excel(path="Lefsefigure.xlsx")
lefse$Year = as.factor(lefse$Year)
lefse$Taxonomy <- factor(lefse$Taxonomy, levels = lefse$Taxonomy)

plot_colors <- c(
  "#CBD588", "#5F7FC7", "orange","#DA5724", "#508578"
)
  



plot = ggplot(lefse, aes(x=Taxonomy, y=LDA, fill = Year)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = plot_colors) +
  ggtitle("LEFSE Analysis by Year")
plot
