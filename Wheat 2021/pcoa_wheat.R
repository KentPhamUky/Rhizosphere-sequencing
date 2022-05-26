library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)

setwd("C:/Mothur/Wheat 2021")

#you may need to change the file path
metadata <- read_excel(path="wheat metadata.xlsx")

metadata <- rename_all(.tbl=metadata, .funs=tolower)

pcoa <- read_tsv(file="Mix.opti_mcc.0.03.subsample.thetayc.0.03.lt.ave.pcoa.axes",
                 col_types = cols(group=col_character()))

#if your metadata has "sample" as column title, this will be by=c('sample'='group') 
metadata_pcoa <- inner_join(metadata, pcoa, by=c('group'))
as.factor(metadata_pcoa$treatment)
#choose colors in color brewer, must be same number of colors as your catagory
color2 <- c("grey", "green",  "blue", "red", "yellow", "cyan")

#if you want convex hulls

#create a function to find hulls
#find_hull <- function(df) df[chull(df$axis1, df$axis2), ]

hull_make <- metadata_pcoa %>%
  group_by(treatment) %>%
  slice(chull(axis1, axis2))

#if you don't need to filter

#nmds_hulls <- ddply(metadata_nmds, "treatment", find_hull)

#ggplot, no filter

plot = ggplot(metadata_pcoa, aes(x = axis1, y = axis2))   +
  geom_point(size = 4, aes( shape = as.factor(treatment), colour = as.factor(treatment))) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "pcoa1", colour = "treatment", y = "pcoa2", shape = "treatment")+ 
 geom_polygon(data = hull_make, alpha = 0.2, aes(fill = factor(treatment)), show.legend = FALSE) 
  #scale_colour_manual(values = c("#009E73", "#E69F00"))  

plot


ggsave("pcoa.tiff", height = 8, width = 10, units = "in")
















