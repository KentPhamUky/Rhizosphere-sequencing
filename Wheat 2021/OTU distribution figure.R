library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)
library(RColorBrewer)

#sets working directory to target folder
setwd("C:/Users/kentp/Documents/Github/Rhizosphere-sequencing/Wheat 2021") 

#reads ub the metadata
data <- read_excel(path="OTU.xlsx")
Distribution = ggplot(data, aes(fill=OTU,y=Count, x=Treatment)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.7)+
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "Treatment", y = "Proportion (%)") +
  ggtitle("2020 OTU Community Distribution") + theme(plot.title = element_text(size=20, hjust = 0.5))

Distribution

ggsave("Distribution OTU.tiff", height = 8, width = 10, units = "in")
