library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)
library(RColorBrewer)
library(grid)
        
####Complete read in####
#reads in the metadata
metadata <- read_excel(path="wheat metadata.xlsx")

#read in the nmds axes
nmds <- read_tsv(file="demi.thetayc.axes",
                 col_types = cols(group=col_character()))
#Combine data
metadata_nmds <- inner_join(metadata, nmds, by=c('group'))
as.factor(metadata_nmds$Treatment)


Wheat2021 = subset(meatdata_nmds, Year == "2021")
Wheat2022 = subset(meatdata_nmds, Year == "2022")
Wheat2023 = subset(meatdata_nmds, Year == "2023")
Wheat2024 = subset(meatdata_nmds, Year == "2024")
####end####

####Individual read in####
metadata <- read_excel(path="Year1metadata.xlsx")
nmds <- read_tsv(file="year1.thetayc.axes", col_types = cols(group=col_character()))
Wheat2021 = inner_join(metadata, nmds, by=c('group'))

metadata <- read_excel(path="Year2metadata.xlsx")
nmds <- read_tsv(file="year2.thetayc.axes", col_types = cols(group=col_character()))
Wheat2022 = inner_join(metadata, nmds, by=c('group'))

metadata <- read_excel(path="Year3metadata.xlsx")
nmds <- read_tsv(file="year3.thetayc.axes", col_types = cols(group=col_character()))
Wheat2023 = inner_join(metadata, nmds, by=c('group'))

metadata <- read_excel(path="Year4metadata.xlsx")
nmds <- read_tsv(file="year4.thetayc.axes", col_types = cols(group=col_character()))
Wheat2024 = inner_join(metadata, nmds, by=c('group'))
####end####

####2021####
NMDS2021plot = ggplot(Wheat2021, aes(x = axis1, y = axis2, shape = Site, color = as.factor(Treatment), linetype = Site))   +
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
  ggtitle("Spring 2021 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  annotate(geom="text", x= .43, y=.5, label="Stress = 0.151 \n Rsq = 0.911 \n 95% confidence",
            color="black", size = 6) +
  theme_classic()
NMDS2021plot
ggsave("NMDS 2021 stats.tiff", height = 8, width = 10, units = "in")
 
####end####

####2022####
NMDS2022plot = ggplot(Wheat2022, aes(x = axis1, y = axis2, shape = Site, color = as.factor(Treatment), linetype = Site))   +
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
  ggtitle("Spring 2022 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  annotate(geom="text", x= .43, y=.5, label="Stress = 0.151 \n Rsq = 0.911 \n 95% confidence",
           color="black", size = 6) +
  theme_classic()
NMDS2022plot
ggsave("NMDS 2022 stats.tiff", height = 8, width = 10, units = "in")

####end####

####2023####
NMDS2023plot = ggplot(Wheat2023, aes(x = axis1, y = axis2, shape = Site, color = as.factor(Treatment), linetype = Site))   +
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
ggsave("NMDS 2023 stats.tiff", height = 8, width = 10, units = "in")

####end####

####2024####
NMDS2024plot = ggplot(Wheat2024, aes(x = axis1, y = axis2, shape = Site, color = as.factor(Treatment), linetype = Site))   +
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
  ggtitle("Spring 2024 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  annotate(geom="text", x= .43, y=.5, label="Stress = 0.151 \n Rsq = 0.911 \n 95% confidence",
           color="black", size = 6) +
  theme_classic()
NMDS2024plot
ggsave("NMDS2024.tiff", height = 8, width = 10, units = "in")

####end####

####Combine graphs####
ggarrange(NMDS2021plot, NMDS2022plot, NMDS2023plot, NMDS2024plot, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right")
####end####