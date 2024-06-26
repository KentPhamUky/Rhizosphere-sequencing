library(tidyverse)
library(readxl)
metadata = read_csv(file="Metadata2021.csv")
AlphaDiversity = read_csv(file="alphadiversity.csv")
Mixed = inner_join(metadata, AlphaDiversity, by=c('group'))

alphaplot = ggplot(Mixed, aes(x = Treatment, y = invsimpson, color = Treatment))   +
  geom_violin(size = 2) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "Treatment", colour = "Treatment", y = "Inverse Simpson")+ 
  ggtitle("Spring 2021 Wheat Rhizosphere Alpha Diversity") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme_classic()
alphaplot

richnessplot = ggplot(Mixed, aes(x = Treatment, y = sobs, color = Treatment))   +
  geom_violin(size = 2) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "Treatment", colour = "Treatment", y = "Richness")+ 
  ggtitle("Spring 2021 Wheat Rhizosphere Richness") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme_classic()
richnessplot
