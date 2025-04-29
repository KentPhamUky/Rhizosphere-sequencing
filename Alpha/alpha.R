library(tidyverse)
library(readxl)
library(agricolae)
library(nlme)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
library(ggpubr)
metadata = read_csv(file="Metadata.csv")
AlphaDiversity = read_csv(file="alphatotal.summary.csv")
Mixed = inner_join(metadata, AlphaDiversity, by=c('group'))
Mixed$Year = as.factor(Mixed$Year)
UKtest = subset(Mixed, Site =="UK")
KStest = subset(Mixed, Site =="KS")

InversesimpplotKS = ggplot(KStest, aes(x = Treatment, y = invsimpson, fill= Year)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x = NULL, colour = "Treatment", y = "Inverse Simpson")+ 
  annotate("text", x = .825, y = 400, label = "ABCD") +
  annotate("text", x = 1.175, y = 430, label = "E") +
  annotate("text", x = 1.825, y = 405, label = "AC") +
  annotate("text", x = 2.175, y = 420, label = "BDE") +
  annotate("text", x = 2.825, y = 380, label = "AB") +
  annotate("text", x = 3.175, y = 400, label = "CDE") +
  ylim(100,600)+
  theme_classic()
InversesimpplotKS

InversesimpplotUK = ggplot(UKtest, aes(x = Treatment, y = invsimpson, fill= Year)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x = NULL, colour = "Treatment", y = "Inverse Simpson")+ 
  annotate("text", x = .825, y = 520, label = "A") +
  annotate("text", x = 1.175, y = 520, label = "A") +
  annotate("text", x = 1.825, y = 510, label = "A") +
  annotate("text", x = 2.175, y = 480, label = "A") +
  annotate("text", x = 2.825, y = 530, label = "A") +
  annotate("text", x = 3.175, y = 480, label = "A") +
  ylim(100,600)+
  theme_classic()
InversesimpplotUK


ShannonplotKS = ggplot(KStest, aes(x = Treatment, y = shannon, fill= Year)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x=NULL, y = "Shannon Diversity")+ 
  annotate("text", x = .825, y = 7.25, label = "ABCE") +
  annotate("text", x = 1.175, y = 7.18, label = "DF") +
  annotate("text", x = 1.825, y = 7.08, label = "ACD") +
  annotate("text", x = 2.175, y = 7.18, label = "BEF") +
  annotate("text", x = 2.825, y = 7.20, label = "AB") +
  annotate("text", x = 3.175, y = 7.1, label = "CDEF") +
  ylim(6, 7.5)+
  theme_classic()
ShannonplotKS

ShannonplotUK = ggplot(UKtest, aes(x = Treatment, y = shannon, fill= Year)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x=NULL, y = "Shannon Diversity")+ 
  annotate("text", x = .825, y = 7.3, label = "A") +
  annotate("text", x = 1.175, y = 7.3, label = "A") +
  annotate("text", x = 1.825, y = 7.3, label = "A") +
  annotate("text", x = 2.175, y = 7.3, label = "A") +
  annotate("text", x = 2.825, y = 7.3, label = "A") +
  annotate("text", x = 3.175, y = 7.3, label = "A") +
  ylim(6, 7.5)+
  theme_classic()
ShannonplotUK


richnessplotKS = ggplot(KStest, aes(x = Treatment, y = sobs, fill=Year)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x = NULL, colour = "Treatment", y = "# of OTUs")+ 
  annotate("text", x = .825, y = 5500, label = "A") +
  annotate("text", x = 1.175, y = 5500, label = "A") +
  annotate("text", x = 1.825, y = 5500, label = "A") +
  annotate("text", x = 2.175, y = 5200, label = "A") +
  annotate("text", x = 2.825, y = 5700, label = "A") +
  annotate("text", x = 3.175, y = 5000, label = "A") +
  ylim(2400,6000)+
  theme_classic() #Overwrites theme above.
richnessplotKS

richnessplotUK = ggplot(UKtest, aes(x = Treatment, y = sobs, fill=Year)) +
  geom_boxplot(width=0.7) +
  scale_fill_manual(values = c("#508578", "#AD6F3B")) +
  labs(x = NULL, colour = "Treatment", y = "# of OTUs")+ 
  annotate("text", x = .825, y = 4000, label = "CD") +
  annotate("text", x = 1.175, y = 3450, label = "AB") +
  annotate("text", x = 1.825, y = 4250, label = "BCD") +
  annotate("text", x = 2.175, y = 3300, label = "A") +
  annotate("text", x = 2.825, y = 4100, label = "D") +
  annotate("text", x = 3.175, y = 3400, label = "ABC") +
  ylim(2400,6000)+
  theme_classic() #Overwrites theme above.
richnessplotUK

ann1_table <- ggtexttable(
  data.frame(label = "KS Rhizosphere Diversity Indices"), 
  rows = NULL,
  theme = ttheme("blank", base_style = "blank", 
                 base_size = 12, 
                 colnames.style = colnames_style(color = NA),
                 tbody.style = tbody_style(hjust = 0))
)
ann2_table <- ggtexttable(
  data.frame(label = "UK Rhizosphere Diversity Indices"), 
  rows = NULL,
  theme = ttheme("blank", base_style = "blank", 
                 base_size = 12, 
                 colnames.style = colnames_style(color = NA),
                 tbody.style = tbody_style(hjust = 0))
)

combineyears = ggarrange(InversesimpplotKS,InversesimpplotUK+rremove("ylab"),
                         ShannonplotKS,ShannonplotUK+rremove("ylab"),
                         richnessplotKS,richnessplotUK+ rremove("ylab"),
                         common.legend= TRUE,
                         legend="right",
                         ncol = 2, nrow = 3,heights = c(1,1,1))

combineyears <- annotate_figure(combineyears,
                                top = text_grob("KS Rhizosphere Diversity Indices                             UK Rhizosphere Diversity Indices", 
                                                face = "bold")
)
combineyears

lsd=LSD.test(UKrich, "Year")
print(lsd)

mod <- lm(shannon ~ Treatment+Year,
           data = UKtest,
           na.action = na.omit
)
anova(mod)

#this is doing the pairwise comparisons within each measurement
emm<-emmeans(mod, ~Treatment+Year)
emm<-emmeans(mod, ~Treatment)
emm <-emmeans(mod, ~Year)

#this is getting the letters. 
multcomp::cld(emm, Letters=LETTERS)



