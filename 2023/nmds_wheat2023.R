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

#read in the nmds axes
nmds <- read_tsv(file="demi.thetayc.axes",col_types = cols(group=col_character()))
#Combine data
metadata_nmds <- inner_join(metadata, nmds, by=c('group'))
as.factor(metadata_nmds$Treatment)
####end####

####2023####
NMDS2023plot = ggplot(Wheat2023, aes(x = axis1, y = axis2, shape = Site, color = Cashcrop, linetype = Site))   +
  geom_point(size = 2 ) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "NMDS1", colour = "Treatment", y = "NMDS2", shape = "Site")+ 
  stat_ellipse(lwd=1)+
  ggtitle("Spring 2023 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  annotate(geom="text", x= .43, y=.5, label="Stress = 0.151 \n Rsq = 0.911 \n 95% confidence",
           color="black", size = 6) +
  theme_classic()
NMDS2023plot
ggsave("NMDS 2023.tiff", height = 8, width = 10, units = "in")

####end####
