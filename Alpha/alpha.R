library(tidyverse)
library(readxl)
library(agricolae)
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
KStest = subset(Mixed, Site =="KS")


UKinvsimp = aov(invsimpson ~ Treatment * Year, data=UKtest)
summary(UKinvsimp)
UKshannon = aov(shannon ~ Treatment * Year, data=UKtest)
summary(UKshannon)
UKrich = aov(sobs ~ Treatment * Year, data=UKtest)
summary(UKrich)

KSinvsimp = aov(invsimpson ~ Treatment * Year, data=KStest)
summary(KSinvsimp)
KSshannon = aov(shannon ~ Treatment * Year, data=KStest)
summary(KSshannon)
KSrich = aov(sobs ~ Treatment * Year, data=KStest)
summary(KSrich)


lsd=LSD.test(UKrich, "Year")
print(lsd)



