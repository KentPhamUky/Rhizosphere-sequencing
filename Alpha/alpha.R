library(tidyverse)
library(readxl)
metadata = read_csv(file="Metadata.csv")
AlphaDiversity = read_csv(file="alphatotal.summary.csv")
Mixed = inner_join(metadata, AlphaDiversity, by=c('group'))
Mixed$Year = as.factor(Mixed$Year)
UK = subset(Mixed, Site =="UK")

Inversesimpplot = ggplot(Mixed, aes(x = Treatment, y = invsimpson, fill= Year)) +
  facet_grid(vars(),vars(Site)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x = "Treatment", colour = "Treatment", y = "Inverse Simpson")+ 
  ggtitle("Wheat Rhizosphere Inverse Simpson") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme_classic()
Inversesimpplot

Shannonplot = ggplot(Mixed, aes(x = Treatment, y = shannon, fill= Year)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x = "Treatment", colour = "Treatment", y = "Shannon Diversity")+ 
  ggtitle("Wheat Rhizosphere Shannon Diversity") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  #geom_line(data=tibble(x=c(2,4), y=c(6.95,6.95)), aes(x=x, y=y), inherit.aes=FALSE) +
  #geom_text(data=tibble(x=3, y=7), aes(x=x, y=y, label="*"), inherit.aes=FALSE) +
  facet_grid(vars(),vars(Site)) +
  theme_classic()
Shannonplot

richnessplot = ggplot(Mixed, aes(x = Treatment, y = sobs, fill=Year)) +
  facet_grid(vars(),vars(Site)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x = "Treatment", colour = "Treatment", y = "# of OTUs")+ 
  ggtitle("Wheat Rhizosphere Richness") + theme(plot.title = element_text(size=20, hjust = 0.5)) +
  theme_classic() #Overwrites theme above.
richnessplot

UKtest = subset(Mixed, Site =="UK")

Conventional = subset(KStest, Treatment == "Conventional")
Fiber = subset(KStest, Treatment == "Hemp Fiber")
Grain = subset(KStest, Treatment == "Hemp Grain")

aovConvdiv = aov(invsimpson ~ Year, data = Conventional)
summary(aovConvdiv)
aovConvrich = aov(sobs ~ Year, data = Conventional)
summary(aovConvrich)
aovConvshannon = aov(shannon ~ Year, data = Conventional)
summary(aovConvshannon)

aovFibdiv = aov(invsimpson ~ Year, data = Fiber)
summary(aovFibdiv)
aovFibrich = aov(sobs ~ Year, data = Fiber)
summary(aovFibrich)
aovFibshannon = aov(shannon ~ Year, data = Fiber)
summary(aovFibshannon)

aovGraindiv = aov(invsimpson ~ Year, data = Grain)
summary(aovGraindiv)
aovGrainrich = aov(sobs ~ Year, data = Grain)
summary(aovGrainrich)
aovGrainshannon = aov(shannon ~ Year, data = Grain)
summary(aovGrainshannon)


Year0 = subset(KStest, Year == "0")
Year3 = subset(KStest, Year == "3")

aovY0div = aov(invsimpson ~ Treatment, data=Year0)
summary(aovY0div)
aovY0rich = aov(sobs~Treatment, data=Year0)
summary(aovY0rich)
aovY0shannon = aov(shannon ~ Treatment, data =Year0)
summary(aovY0shannon)

aovY3div = aov(invsimpson ~ Treatment, data=Year3)
summary(aovY3div)
aovY3rich = aov(sobs~Treatment, data=Year3)
summary(aovY3rich)
aovY3shannon = aov(sobs~Treatment, data=Year3)
summary(aovY3shannon)


KStest = subset(Mixed, Site =="KS")

Treatmentsobs = aov(sobs ~ Treatment, data = UKtest)
summary(Treatmentsobs)
Treatmentinv = aov(invsimpson ~ Treatment, data = UKtest)
summary(Treatmentinv)
Treatmentshannon = aov(shannon ~ Treatment, data = UKtest)
summary(Treatmentshannon)

Yearsobs = aov(sobs ~ Year, data = UKtest)
summary(Yearsobs)
Yearinv = aov(invsimpson ~ Year, data = UKtest)
summary(Yearinv)
Yearshannon = aov(shannon ~ Year, data = UKtest)
summary(Yearshannon)
