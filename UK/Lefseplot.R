#lefse plot
library(tidyverse)
library(readxl)


lefse <- read_excel(path="Lefsefigure.xlsx")

lefse$Taxonomy <- factor(lefse$Taxonomy, levels = lefse$Taxonomy)


#scale_fill_manual(values = c("#508578", "#AD6F3B")) +


plot = ggplot(lefse, aes(x=fct_reorder(OTU, Year), y=LDA, fill = Year)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(name = "Change from\nYear 0 to Year 3",values = c("#508578", "#AD6F3B")) +
  ggtitle("LEFSE Analysis of UK Rhizosphere Communities")+ labs(x="OTU")+
  theme_classic() +
  annotate("text", x = 1, 0.1, hjust =0, label = "c_Spartobacteria")+
  annotate("text", x = 2, 0.1, hjust =0, label = "g_Pseudarthrobacter")+
  annotate("text", x = 3, 0.1, hjust =0, label = "c_Acidobacteria_Gp6")+
  annotate("text", x = 4, 0.1, hjust =0, label = "f_Comamonadaceae")+
  annotate("text", x = 5, 0.1, hjust =0, label = "c_Acidobacteria_Gp16")+
  annotate("text", x = 6, 0.1, hjust =0, label = "f_Comamonadaceae")+
  annotate("text", x = 7, 0.1, hjust =0, label = "c_Acidobacteria_Gp4")+
  annotate("text", x = 8, 0.1, hjust =0, label = "g_Massilia")+
  annotate("text", x = 9, 0.1, hjust =0, label = "Unclassified")+
  annotate("text", x = 10, 0.1, hjust =0, label = "o_Nitrosomonadales")+
  annotate("text", x = 11, -0.1, hjust =1, label = "g_Bradyrhizobium")+
  annotate("text", x = 12, -0.1, hjust =1, label = "g_Mycobacterium")+
  annotate("text", x = 13, -0.1, hjust =1, label = "g_Flavobacterium")+
  annotate("text", x = 14, -0.1, hjust =1, label = "g_Solirubrobacter")+
  annotate("text", x = 15, -0.1, hjust =1, label = "p_Verrucomicrobiota")+
  annotate("text", x = 16, -0.1, hjust =1, label = "c_Acidobacteria_Gp6")+
  annotate("text", x = 17, -0.1, hjust =1, label = "g_Pseudonocardia")+
  annotate("text", x = 18, -0.1, hjust =1, label = "g_Gaiella")+
  annotate("text", x = 19, -0.1, hjust =1, label = "g_Marmoricola")+
  annotate("text", x = 20, -0.1, hjust =1, label = "f_Bacillaceae")
  
plot

