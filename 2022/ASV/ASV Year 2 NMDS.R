library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)
library(RColorBrewer)

#install.packages("rgl")
library(rgl)        

setwd("C:/Users/kentp/Documents/GitHub/Rhizosphere-sequencing/Wheat 2021")                  #sets working directory to target folder
setwd("D:/GitHub/Rhizosphere-sequencing/2022/ASV")

metadata <- read_excel(path="wheat metadata.xlsx")                                          #reads in the metadata

nmds <- read_tsv(file="final.asv.ASV.subsample.thetayc.ASV.lt.ave.nmds.axes",               #read in the nmds axes
                 col_types = cols(group=col_character()))


metadata_nmds <- inner_join(metadata, nmds, by=c('group'))                                  #Combine metadata with nmds and set treatment to factor
as.factor(metadata_nmds$Treatment)


#hull_make <- metadata_nmds %>%                                                             #Used for making hulls, don't need with statistical ellipses
  #group_by(combined) %>%
  #slice(chull(axis1, axis2, axis3))

hull_make2 <- metadata_nmds %>%
  group_by(Treatment, combined) %>%
  slice(chull(axis1, axis2))

hull_make2$Treatment = as.factor(hull_make2$Treatment)

plot2 = ggplot(metadata_nmds, aes(x = axis1, y = axis2, color = as.factor(Treatment), shape = Site))   +
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
  geom_polygon(data = hull_make2, alpha = 0.2, aes(fill = Treatment, colour = Treatment ), show.legend = FALSE)  +
  #scale_color_brewer(palette = "Set3") + scale_fill_brewer(palette = "Set3") +
  ggtitle("2020 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  annotate(geom="text", x= -.30, y=-.35, label="Stress = 0.145 \n Rsq = 0.911",
           color="red", size = 6) 

plot2
ggsave("NMDS better.tiff", height = 8, width = 10, units = "in")





plot3d(x=metadata_nmds$axis1, y=metadata_nmds$axis2, z=metadata_nmds$axis3)









