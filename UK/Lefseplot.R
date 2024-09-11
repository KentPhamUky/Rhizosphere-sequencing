#lefse plot
library(tidyverse)
library(readxl)


metadata <- read_excel(path="Lefseplotyear.xlsx")

plot = ggplot(data, aes(x=name, y=value, colour = Year)) + 
  geom_bar(stat = "identity") +
  coord_flip()