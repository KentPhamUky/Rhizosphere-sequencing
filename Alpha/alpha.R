library(tidyverse)
library(readxl)
metadata = read_csv(file="Metadata.csv")
AlphaDiversity = read_csv(file="alphatotal.summary.csv")
Mixed = inner_join(metadata, AlphaDiversity, by=c('group'))
Mixed$Year = as.factor(Mixed$Year)
UK = subset(Mixed, Site =="UK")

alphaplot = ggplot(Mixed, aes(x = Treatment, y = invsimpson, fill= Year)) +
  facet_grid(vars(),vars(Site)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x = "Treatment", colour = "Treatment", y = "Inverse Simpson")+ 
  ggtitle("Wheat Rhizosphere Inverse Simpson") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme_classic()
alphaplot

richnessplot = ggplot(Mixed, aes(x = Treatment, y = sobs, fill=Year)) +
  facet_grid(vars(),vars(Site)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 20, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) +    
  labs(x = "Treatment", colour = "Treatment", y = "# of OTUs")+ 
  ggtitle("Wheat Rhizosphere Richness") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme_classic() #Overwrites theme above.
richnessplot

UKtest = subset(Mixed, Site =="UK")

Conventional = subset(UKtest, Treatment == "Conventional")
Fiber = subset(UKtest, Treatment == "Hemp Fiber")
Grain = subset(UKtest, Treatment == "Hemp Grain")

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


Year1 = subset(UKtest, Year == "1")
Year4 = subset(UKtest, Year == "4")

aovY1div = aov(invsimpson ~ Rotation, data=Year1)
summary(aovY1div)
aovY1rich = aov(sobs~Rotation, data=Year1)
summary(aovY1rich)
aovY4div = aov(invsimpson ~ Rotation, data=Year4)
summary(aovY4div)
aovY4rich = aov(sobs~Rotation, data=Year4)
summary(aovY4rich)


KStest = subset(Mixed, Site =="KS")

Conventional = subset(KStest, Treatment == "Conventional")
Fiber = subset(KStest, Treatment == "Hemp Fiber")
Grain = subset(KStest, Treatment == "Hemp Grain")

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

