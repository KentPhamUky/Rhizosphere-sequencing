#lefse plot
library(tidyverse)
library(readxl)

setwd("D:/GitHub/Rhizosphere-sequencing/KS")
lefse <- read_excel(path="Lefsefigure.xlsx")

lefse$Taxonomy <- factor(lefse$Taxonomy, levels = lefse$Taxonomy)


#scale_fill_manual(values = c("#508578", "#AD6F3B")) +


plotKS = ggplot(lefse, aes(x=fct_reorder(OTU, Year), y=LDA, fill = Year)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(name = "Change from\nYear 0 to Year 3",values = c("#508578", "#AD6F3B")) +
  ggtitle("LEfSe of KS Rhizosphere Communities")+ labs(x="OTU")+
  theme_classic() +
  annotate("text", x = 1, 0.1, hjust =0, label = "c_Spartobacteria")+
  annotate("text", x = 2, 0.1, hjust =0, label = "o_Gp6")+
  annotate("text", x = 3, 0.1, hjust =0, label = "o_Gp6")+
  annotate("text", x = 4, 0.1, hjust =0, label = "c_Spartobacteria")+
  annotate("text", x = 5, 0.1, hjust =0, label = "o_Gp4")+
  annotate("text", x = 6, 0.1, hjust =0, label = "o_Gp6")+
  annotate("text", x = 7, 0.1, hjust =0, label = "c_Spartobacteria")+
  annotate("text", x = 8, 0.1, hjust =0, label = "g_Nocardioides")+
  annotate("text", x = 9, 0.1, hjust =0, label = "Unclassified")+
  annotate("text", x = 10, 0.1, hjust =0, label = "o_Gp6")+
  annotate("text", x = 11, -0.1, hjust =1, label = "g_Bradyrhizobium")+
  annotate("text", x = 12, -0.1, hjust =1, label = "g_Gaiella")+
  annotate("text", x = 13, -0.1, hjust =1, label = "g_Flavobacterium")+
  annotate("text", x = 14, -0.1, hjust =1, label = "g_Pseudonocardia")+
  annotate("text", x = 15, -0.1, hjust =1, label = "o_Hyphomicrobiales")+
  annotate("text", x = 16, -0.1, hjust =1, label = "f_Bradyrhizobiaceae")+
  annotate("text", x = 17, -0.1, hjust =1, label = "g_Gaiella")+
  annotate("text", x = 18, -0.1, hjust =1, label = "g_Gaiella")+
  annotate("text", x = 19, -0.1, hjust =1, label = "Unclassified")+
  annotate("text", x = 20, -0.1, hjust =1, label = "g_Solirubrobacter")
  
plotKS

