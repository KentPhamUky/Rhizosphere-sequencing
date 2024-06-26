library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)
library(RColorBrewer)
library(grid)

####Complete read in####
#reads in the metadata
metadata <- read_excel(path="Metadata2023.xlsx")

#read in the PCoA axes
PCoA <- read_tsv(file="Y3final.opti_mcc.0.03.subsample.braycurtis.0.03.lt.pcoa.axes",col_types = cols(group=col_character()))
#Combine data
Wheat2023 <- inner_join(metadata, PCoA, by=c('group'))
####end####

####Hull option####
hull = Wheat2023 %>%
  group_by(Site,Treatment) %>%
  slice(chull(axis1, axis2))
####end####

####2023####
PCoA2023plot = ggplot(Wheat2023, aes(x = axis1, y = axis2, shape = Site, color = Cashcrop, linetype = Site, fill=Cashcrop))   +
  geom_point(size = 2 ) +
  geom_polygon(data=hull, alpha=0.25)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "PC1 (28.8%)", y = "PC2 (11.6%)")+ 
  #stat_ellipse(lwd=1)+
  ggtitle("Spring 2023 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  #annotate(geom="text", x= .43, y=.5, label="Stress = 0.151 \n Rsq = 0.911 \n 95% confidence",
          # color="black", size = 6) +
  theme_classic()
PCoA2023plot
ggsave("PCoA 2023.tiff", height = 8, width = 10, units = "in")

####end####
