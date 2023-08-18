library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)
library(RColorBrewer)
        
#sets working directory to target folder
setwd("C:/Users/kentp/Documents/GitHub/Rhizosphere-sequencing/2022/OTU") 
setwd("D:/GitHub/Rhizosphere-sequencing/2022/OTU")
#reads ub the metadata
metadata <- read_excel(path="wheat metadata.xlsx")
metadata2 = read_excel(path= "wheat metadata2.xlsx")
#renames items in metadata file
#metadata <- rename_all(.tbl=metadata, .funs=tolower)

#read in the nmds axes
nmds <- read_tsv(file="toro.trim.contigs.good.unique.good.filter.unique.precluster.denovo.vsearch.pick.opti_mcc.0.03.subsample.thetayc.0.03.lt.ave.nmds.axes",
                 col_types = cols(group=col_character()))

#if your metadata has "sample" as column title, this will be by=c('sample'='group') 
metadata_nmds <- inner_join(metadata2, nmds, by=c('group'))
as.factor(metadata_nmds$Treatment)



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
   ggtitle("Spring 2022 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
   annotate(geom="text", x= .43, y=.4, label="Stress = 0.171 \n Rsq = 0.891 \n 95% confidence",
            color="black", size = 6)
 
 plot2
 ggsave("NMDS 2022 stats.tiff", height = 8, width = 10, units = "in")
 






