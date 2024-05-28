if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("phyloseq")

library(ggplot2)
library(vegan)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(phyloseq)

#Plotting theme
theme_set(theme_bw())

sharedfile = "X.shared"
taxfile = "X.taxonomy"
mapfile = "mothurmetadata.csv"

mothur_data <- import_mothur(mothur_shared_file = sharedfile,
                             mothur_constaxonomy_file = taxfile)
# Import sample metadata
map <- read.csv(mapfile)

map <- sample_data(map)

# Assign rownames to be Sample ID's
rownames(map) <- map$SampleID

moth_merge <- merge_phyloseq(mothur_data, map)
colnames(tax_table(moth_merge)) <- c("Kingdom", "Phylum", "Class", 
                                     "Order", "Family", "Genus")

moth_sub <- moth_merge %>%
  subset_samples(Type == "sample") %>%
  prune_taxa(taxa_sums(.) > 0, .)



Clean <- moth_sub %>%
  subset_taxa(
    Kingdom == "Bacteria" &
      Family  != "mitochondria" &
      Class   != "Chloroplast"
  )


# Make a data frame with a column for the read counts of each sample
sample_sum_df <- data.frame(sum = sample_sums(Clean))
ggplot(sample_sum_df, aes(x = sum)) + 
  geom_histogram(color = "black", fill = "indianred", binwidth = 2500) +
  ggtitle("Distribution of sample sequencing depth") + 
  xlab("Read counts") +
  theme(axis.title.y = element_blank())
smin <- min(sample_sums(Clean))
smean <- mean(sample_sums(Clean))
smax <- max(sample_sums(Clean))



Clean_phylum <- Clean %>%
  tax_glom(taxrank = "Phylum") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa
  arrange(Phylum)                                      # Sort data frame alphabetically by phylum

# Plot
phylum_colors <- c(
  "#CBD588", "#5F7FC7", "orange","#DA5724", "#508578", "#CD9BCD",
  "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861"
)
ggplot(Clean_phylum, aes(x = Plot, y = Abundance, fill = Phylum)) + 
  facet_grid(Station~.) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = phylum_colors) +
  #scale_x_discrete(
   # breaks = c("7/8", "8/4", "9/2", "10/6"),
   #labels = c("Jul", "Aug", "Sep", "Oct"), 
   # drop = FALSE
  #) +
  # Remove x axis title
  theme(axis.title.x = element_blank()) + 
  #
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Phyla > 2%) \n") +
  ggtitle("Phylum Composition of Spring 2024 \n Bacterial Communities by Plot") 



####Ordination with arrows####

bray<- phyloseq::distance(physeq = Clean, method = "bray")


# CAP ordinate
cap_ord <- ordinate(
  physeq = Clean, 
  method = "CAP",
  distance = bray,
  formula = ~ soilpH + P.kg.ha + K.kg.ha + Ca.kg.ha + Mg.kg.ha + Zn.kg.ha
)

# CAP plot
cap_plot <- plot_ordination(
  physeq = Clean, 
  ordination = cap_ord, 
  color = "CashCrop", 
  axes = c(1,2)
) + 
  aes(shape = Site) + 
  geom_point(aes(colour = CashCrop), alpha = 0.4, size = 4) + 
  geom_point(colour = "grey90", size = 1.5) + 
  scale_color_manual(values = c("#a65628", "red", "#ffae19", "#4daf4a", 
                                "#1919ff", "darkorchid3", "magenta")
  )


# Now add the environmental variables as arrows
arrowmat <- vegan::scores(cap_ord, display = "bp")

# Add labels, make a data.frame
arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)

# Define the arrow aesthetic mapping
arrow_map <- aes(xend = CAP1, 
                 yend = CAP2, 
                 x = 0, 
                 y = 0, 
                 shape = NULL, 
                 color = NULL, 
                 label = labels)

label_map <- aes(x = 1.3 * CAP1, 
                 y = 1.3 * CAP2, 
                 shape = NULL, 
                 color = NULL, 
                 label = labels)

arrowhead = arrow(length = unit(0.02, "npc"))

# Make a new graphic
cap_plot + 
  geom_segment(
    mapping = arrow_map, 
    size = .5, 
    data = arrowdf, 
    color = "gray", 
    arrow = arrowhead
  ) + 
  geom_text(
    mapping = label_map, 
    size = 4,  
    data = arrowdf, 
    show.legend = FALSE
  )