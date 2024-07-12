library(tidyverse)
library(readxl)
metadata = read_csv(file="Metadata.csv")
AlphaDiversity = read_csv(file="alphatotal.summary.csv")
Mixed = inner_join(metadata, AlphaDiversity, by=c('group'))
Mixed$Year = as.factor(Mixed$Year)

alphaplot = ggplot(Mixed, aes(x = Treatment, y = invsimpson, fill= Year)) +
  geom_boxplot(width=0.7) +
  facet_wrap(~Site) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "Treatment", colour = "Treatment", y = "Inverse Simpson")+ 
  ggtitle("Wheat Rhizosphere Alpha Diversity") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme_classic()
alphaplot

richnessplot = ggplot(Mixed, aes(x = Treatment, y = sobs, fill=Year)) +
  geom_boxplot(width=0.7) +
  facet_wrap(~Site) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +  
  labs(x = "Treatment", colour = "Treatment", y = "Richness")+ 
  ggtitle("Wheat Rhizosphere Richness") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme_classic()
richnessplot

Conventional = subset(Mixed, Treatment == "Conventional")
Fiber = subset(Mixed, Treatment == "Hemp Fiber")
Grain = subset(Mixed, Treatment == "Hemp Grain")

aovConvdiv = aov(invsimpson ~ Year, data = Conventional)
summary(aovConvdiv)
aovConvrich = aov(sobs ~ Year, data = Conventional)
summary(aovConvrich)

aovFibdiv = aov(invsimpson ~ Year, data = Fiber)
summary(aovFibdiv)
aovFibrich = aov(sobs ~ Year, data = Fiber)
summary(aovFibrich)

aovGraindiv = aov(invsimpson ~ Year, data = Grain)
summary(aovGraindiv)
aovGrainrich = aov(sobs ~ Year, data = Grain)
summary(aovGrainrich)


Year1 = subset(Mixed, Year == "1")
Year4 = subset(Mixed, Year == "4")

aovY1div = aov(invsimpson ~ Treatment, data=Year1)
summary(aovY1div)
aovY1rich = aov(sobs~Treatment, data=Year1)
summary(aovY1rich)
aovY4div = aov(invsimpson ~ Treatment, data=Year4)
summary(aovY4div)
aovY4rich = aov(sobs~Treatment, data=Year4)
summary(aovY4rich)

