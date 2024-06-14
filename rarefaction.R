##Kent Modified rarefaction
library(ggplot2)
library(reshape2)
library(dplyr)
# read in rarefaction file
data <- read.table("final.opti_mcc.groups.rarefaction", header=T)

#Remove lci and hci columns
Cleaned = data %>% 
  select(!contains("lci")) %>%
  select(!contains("hci")) %>%
  melt(id.vars="numsampled")

