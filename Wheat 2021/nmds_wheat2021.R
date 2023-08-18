library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)
library(RColorBrewer)
        
#sets working directory to target folder
setwd("C:/Users/kentp/Documents/GitHub/Rhizosphere-sequencing/Wheat 2021") 
setwd("D:/GitHub/Rhizosphere-sequencing/Wheat 2021")
#reads ub the metadata
metadata <- read_excel(path="wheat metadata.xlsx")

#renames items in metadata file
#metadata <- rename_all(.tbl=metadata, .funs=tolower)

#read in the nmds axes
nmds <- read_tsv(file="Mix.opti_mcc.0.03.subsample.thetayc.0.03.lt.ave.nmds.axes",
                 col_types = cols(group=col_character()))

#if your metadata has "sample" as column title, this will be by=c('sample'='group') 
metadata_nmds <- inner_join(metadata, nmds, by=c('group'))
as.factor(metadata_nmds$Treatment)
#choose colors in color brewer, must be same number of colors as your catagory
#color2 <- c("grey", "green",  "blue", "red", "yellow", "cyan")

#if you want convex hulls

#create a function to find hulls
#find_hull <- function(df) df[chull(df$axis1, df$axis2), ]


hull_make <- metadata_nmds %>%
  group_by(combined) %>%
  slice(chull(axis1, axis2))

#if you don't need to filter

#nmds_hulls <- ddply(metadata_nmds, "treatment", find_hull)

#ggplot, no filter

plot = ggplot(metadata_nmds, aes(x = axis1, y = axis2))   +
  geom_point(size = 2, aes(  colour = as.factor(combined), shape = Site) ) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "NMDS1", colour = "Treatment", y = "NMDS2", shape = "Site")+ 
 geom_polygon(data = hull_make, alpha = 0.2, aes(fill = factor(combined)), show.legend = FALSE)  +
  #scale_color_brewer(palette = "Set3") + scale_fill_brewer(palette = "Set3") +
  ggtitle("2020 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  annotate(geom="text", x= -.30, y=-.35, label="Stress = 0.145",
           color="red", size = 6) 
  
  #scale_colour_manual(values = c("#009E73", "#E69F00"))  

plot


ggsave("NMDS.tiff", height = 8, width = 10, units = "in")







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





plot2 = ggplot(metadata_nmds, aes(x = axis1, y = axis2, shape = Site, color = as.factor(Treatment), linetype = Site))   +
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
  annotate(geom="text", x= .43, y=.4, label="Stress = 0.145 \n Rsq = 0.911 \n 95% confidence",
           color="black", size = 6)

plot2
ggsave("NMDS 2021 stats.tiff", height = 8, width = 10, units = "in")









