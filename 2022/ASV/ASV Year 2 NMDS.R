library(tidyverse)
library(readxl)
library(broom)
library(purrr)
library(plyr)
library(RColorBrewer)
library(plotly)
#install.packages("rgl")
library(rgl)        
#install.packages("corpcor")
library(corpcor)
setwd("C:/Users/kentp/Documents/GitHub/Rhizosphere-sequencing/Wheat 2021")                  #sets working directory to target folder
setwd("D:/GitHub/Rhizosphere-sequencing/2022/ASV")

metadata <- read_excel(path="wheat metadata.xlsx")                                          #reads in the metadata

nmds <- read_tsv(file="final.asv.ASV.subsample.thetayc.ASV.lt.ave.nmds.axes",               #read in the nmds axes
                 col_types = cols(group=col_character()))


metadata_nmds <- inner_join(metadata, nmds, by=c('group'))                                  #Combine metadata with nmds and set treatment to factor
as.factor(metadata_nmds$Treatment)
metadata_nmds$color = case_when(metadata_nmds$Treatment == "1" ~ "blue",
                                metadata_nmds$Treatment == "2" ~ "red",
                                metadata_nmds$Treatment == "3" ~ "green",
                                metadata_nmds$Treatment == "4" ~ "pink",
                                metadata_nmds$Treatment == "5" ~ "purple",
                                metadata_nmds$Treatment == "6" ~ "cyan",
                                TRUE ~ NA_character_)

#hull_make <- metadata_nmds %>%                                                             #Used for making hulls, don't need with statistical ellipses
  #group_by(combined) %>%
  #slice(chull(axis1, axis2, axis3))


#plot2 = ggplot(metadata_nmds, aes(x = axis1, y = axis2, color = as.factor(Treatment), shape = Site))   +
 # geom_point(size = 2 ) +
  #theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
       # axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
       # legend.text = element_text(size = 20, face ="bold", colour ="black"), 
       # legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        #axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        #legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        #panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        #legend.key=element_blank()) +  
  #labs(x = "NMDS1", colour = "Treatment", y = "NMDS2", shape = "Site")+ 
  #geom_polygon(data = hull_make2, alpha = 0.2, aes(fill = Treatment, colour = Treatment ), show.legend = FALSE)  +
  #scale_color_brewer(palette = "Set3") + scale_fill_brewer(palette = "Set3") +
  #ggtitle("2020 Wheat Rhizosphere Microbial Community") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  #annotate(geom="text", x= -.30, y=-.35, label="Stress = 0.145 \n Rsq = 0.911",
   #        color="red", size = 6) 

#plot2
#ggsave("NMDS better.tiff", height = 8, width = 10, units = "in")



#This section splits the data set into year, site, treatment

mn_2020 = subset(metadata_nmds, Year==1)
mn_2021 = subset(metadata_nmds, Year==2)


mn_2020_UK = subset(mn_2020, Site =="UK")
mn_2020_KS = subset(mn_2020, Site =="KS")

mn_2020_1 = subset(mn_2020, Treatment ==1)
  ellips_2020_1 = ellipse3d(cov(cbind(mn_2020_1$axis1, mn_2020_1$axis2, mn_2020_1$axis3)),
                          centre=c(mean(mn_2020_1$axis1), mean(mn_2020_1$axis2), mean(mn_2020_1$axis3), level = 0.95))
mn_2020_2 = subset(mn_2020, Treatment ==2)
mn_2020_3 = subset(mn_2020, Treatment ==3)
mn_2020_4 = subset(mn_2020, Treatment ==4)
mn_2020_5 = subset(mn_2020, Treatment ==5)
mn_2020_6 = subset(mn_2020, Treatment ==6)


mn_2020_UK_1 = subset(mn_2020_UK, Treatment == 1)
mn_2020_UK_2 = subset(mn_2020_UK, Treatment == 2)
mn_2020_UK_3 = subset(mn_2020_UK, Treatment == 3)
mn_2020_UK_4 = subset(mn_2020_UK, Treatment == 4)
mn_2020_UK_5 = subset(mn_2020_UK, Treatment == 5)
mn_2020_UK_6 = subset(mn_2020_UK, Treatment == 6)

mn_2020_KS_1 = subset(mn_2020_KS, Treatment == 1)
mn_2020_KS_2 = subset(mn_2020_KS, Treatment == 2)
mn_2020_KS_3 = subset(mn_2020_KS, Treatment == 3)
mn_2020_KS_4 = subset(mn_2020_KS, Treatment == 4)
mn_2020_KS_5 = subset(mn_2020_KS, Treatment == 5)
mn_2020_KS_6 = subset(mn_2020_KS, Treatment == 6)





#Using rgl to 3d map
plot3d(x=mn_2020_UK_1$axis1, y=mn_2020_UK_1$axis2, z=mn_2020_UK_1$axis3, size = 0.15,
       col = mn_2020_UK_1$color)
plot3d(ellips_2020_UK_1, col = "blue", alpha = 0.2, add = TRUE, type = "shade", box = FALSE)



plot3d(x=metadata_nmds_2020_UK$axis1, y=metadata_nmds_2020_UK$axis2, z=metadata_nmds_2020_UK$axis3,
       xlab = "NMDS Axis 1", ylab = "NMDS Axis 2", zlab = "NMDS Axis 3",
       col=metadata_nmds_2020$color, type ="s", size =1)




#Using plotly to map

pal = c("blue", "red", "green", "pink", "purple", "cyan")
pal = setNames(pal, c("1", "2", "3", "4", "5", "6"))

fig = plot_ly(mn_2020, x= ~axis1, y = ~axis2, z= ~axis3, color = ~Treatment, colors = pal,
              mode = 'markers', symbol = ~Site, symbols = c('x', 'o'), marker = list(size=5))
fig




groups = unique(mn_2020$Treatment)
groups = sort(groups, decreasing = FALSE)
#levs = levels(groups) 
for(i in 1:length(groups)){
  group = groups[i]
  selected = groups == group
  xx = mn_2020$axis1[selected]; yy = mn_2020$axis2[selected]; zz = mn_2020$axis3[selected] 
  co = cov(cbind(xx,yy,zz))
  S = make.positive.definite(co)
  ellipse = ellipse3d(co)
  ellips = ellipse3d(S, centre = c(mean(xx),mean(yy), mean(zz)), level =0.95)
  fig = add_trace(fig, x=ellipse$vb[1,], y= ellipse$vb[2,], z = ellipse$vb[3,],
                  type = 'scatter3d', size = 1,
                  opacity = 0.2,
                  #color = pal[i],
                  showlegend = FALSE)
}
fig

#Currently creates a cloud plot. Figure out how to turn into actual shape
