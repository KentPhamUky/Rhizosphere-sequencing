library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)
library(RColorBrewer)
        
#sets working directory to target folder
setwd("C:/Users/kentp/Documents/GitHub/Rhizosphere-sequencing/2023") 
setwd("C:/Users/kdph224/Documents/Github/Rhizosphere-sequencing/2023")
setwd("D:/GitHub/Rhizosphere-sequencing/2023")
#reads ub the metadata
metadata <- read_excel(path="wheat metadata2.xlsx")
#renames items in metadata file
#metadata <- rename_all(.tbl=metadata, .funs=tolower)

#read in the nmds axes
nmds <- read_tsv(file="demi.thetayc.axes",
                 col_types = cols(group=col_character()))

metadata_nmds <- inner_join(metadata, nmds, by=c('group'))
as.factor(metadata_nmds$Treatment)


plot2 = ggplot(metadata_nmds, aes(x = axis1, y = axis2, shape = Site, color = as.factor(Treatment)))   +
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
  ggtitle("Spring 2023 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  annotate(geom="text", x= -.28, y=-.31, label="Stress = 0.151 \n Rsq = 0.911",
           color="black", size = 6)

plot2
 ggsave("NMDS 2023.tiff", height = 8, width = 10, units = "in")











 hull_make2 <- metadata_nmds2 %>%
   group_by(combined) %>%
   slice(chull(axis1, axis2))
 
 hull_make2$Treatment = as.factor(hull_make2$Treatment)
 
 plot2 = ggplot(metadata_nmds2, aes(x = axis1, y = axis2, shape = Site, color = as.factor(Treatment), linetype = Site))   +
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
  # geom_polygon(data = hull_make2, alpha = 0.2, aes(fill = Treatment, colour = Treatment ), show.legend = FALSE)  +
   #scale_color_brewer(palette = "Set3") + scale_fill_brewer(palette = "Set3") +
   ggtitle("Spring 2023 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
   annotate(geom="text", x= .43, y=.5, label="Stress = 0.151 \n Rsq = 0.911 \n 95% confidence",
            color="black", size = 6)
 
 plot2
 ggsave("NMDS 2023 stats.tiff", height = 8, width = 10, units = "in")
 
