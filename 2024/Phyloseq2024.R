library(devtools)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("phyloseq")
BiocManager::install("microbiome")
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(ggplot2)
library(vegan)
library(plyr)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(phyloseq)
library(microbiome)
library(pairwiseAdonis)

####Loading in####
#Plotting theme
theme_set(theme_bw())

sharedfile = "Y4.final.opti_mcc.0.03.subsample.shared"
taxfile = "Y4.final.opti_mcc.0.03.cons.taxonomy"
mapfile = "Metadata2024.csv"

mothur_data <- import_mothur(mothur_shared_file = sharedfile,
                             mothur_constaxonomy_file = taxfile)
# Import sample metadata
map <- read.csv(mapfile)
map <- subset(map, Site== "KS")
map <- sample_data(map)

# Assign rownames to be Sample ID's
rownames(map) <- map$group

moth_merge <- merge_phyloseq(mothur_data, map)
colnames(tax_table(moth_merge)) <- c("Kingdom", "Phylum", "Class", 
                                     "Order", "Family", "Genus")

#moth_sub <- moth_merge %>%
#subset_samples(Type == "Cashcrop") %>%
# prune_taxa(taxa_sums(.) > 0, .)



Clean <- moth_merge %>%
  subset_taxa(
    Kingdom == "Bacteria" &
      Family  != "mitochondria" &
      Class   != "Chloroplast"
  )
####Metadata####
#Not needed due to us subsampling-
# Make a data frame with a column for the read counts of each sample
sample_sum_df <- data.frame(sum = sample_sums(Clean))
ggplot(sample_sum_df, aes(x = sum)) + 
  geom_histogram(color = "black", fill = "indianred", binwidth = 100) +
  ggtitle("Distribution of sample sequencing depth") + 
  xlab("Read counts") +
  theme(axis.title.y = element_blank())
smin <- min(sample_sums(Clean))
smean <- mean(sample_sums(Clean))
smax <- max(sample_sums(Clean))

####RankedAbundance####
topN <- 100
most_abundant_taxa = sort(taxa_sums(Clean), TRUE)[1:topN]

#extract most_abundant_taxa from mouse_data
Top_100_OTUs <- prune_taxa(names(most_abundant_taxa), Clean)

# create a dataframe with the counts per otu
otu_sums <- data.frame(taxa_sums(Top_100_OTUs))

# use the dataframe to plot the top 100 OTUs in a Rank abundance curve.
ggplot(otu_sums,aes(x=reorder(row.names(otu_sums), -taxa_sums.Top_100_OTUs.), y=taxa_sums.Top_100_OTUs.)) + 
  geom_bar(stat="identity",colour="black",fill="darkturquoise")  +
  xlab("OTU Rank") + ylab("Number of Sequences per OTU") +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + theme_classic() +
  ggtitle("Rank Abundance Curve of the Top 100 OTUs") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

####Phylum Graph####

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
ggplot(Clean_phylum, aes(x = reorder(Sample,Cashcrop), y = Abundance, fill = Phylum)) + 
  #facet_grid(Station~.) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = phylum_colors) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  #
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Phyla > 2%) \n") +
  ggtitle("Phylum Composition of Spring 2024 \n Bacterial Communities by Plot") 






####Genus Graph####

Clean_genus <- Clean %>%
  tax_glom(taxrank = "Genus") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa
  arrange(Genus)                                      # Sort data frame alphabetically by genera

# Plot
genus_colors <- c(
  "#CBD588", "#5F7FC7", "orange","#DA5724", "#508578", "#CD9BCD",
  "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861"
)
ggplot(Clean_genus, aes(x = reorder(Sample,Cashcrop), y = Abundance, fill = Genus)) + 
  geom_bar(stat = "identity") +
  #scale_fill_manual(values = genus_colors) +
  theme(axis.title.x = element_blank()) + 
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  ylab("Relative Abundance (Genera > 2%) \n") +
  ggtitle("Genus Level Composition of Spring 2024 \n Bacterial Communities by Plot") 

####Ordination no arrows ####

# Ordinate
Clean_pcoa <- ordinate(
  physeq = Clean, 
  method = "PCoA", 
  distance = "bray"
)

# Plot 
plot_ordination(
  physeq = Clean,
  ordination = Clean_pcoa,
  color = "Fulltreat",
  shape = "Site",
  title = "PCoA of 2024 by Treatment"
) + 
  scale_color_manual(values = c("#a65628", "red", "#ffae19",
                                "#4daf4a", "#1919ff", "darkorchid3", "magenta")
  ) +
  geom_point(aes(color = Fulltreat), alpha = 0.7, size = 4) +
  geom_point(colour = "grey90", size = 1.5) 

####Permanova####
Clean_bray <- phyloseq::distance(Clean, method = "bray")

# make a data frame from the sample_data
sampledf <- data.frame(sample_data(moth_merge))

# Adonis test
adonis2(Clean_bray ~ Fulltreat, data = sampledf)

pairwise.adonis2(Clean_bray ~ Fulltreat, data = sampledf)



####Ordination with arrows####

bray<- phyloseq::distance(physeq = Clean, method = "bray")



# CAP ordinate
cap_ord <- ordinate(
  physeq = Clean, 
  method = "CAP",
  distance = bray,
  formula = ~ soilpH + P.kg.ha. + K.kg.ha. + Ca.kg.ha. + Mg.kg.ha. + Zn.kg.ha.
)


# CAP plot
cap_plot <- plot_ordination(
  physeq = Clean, 
  ordination = cap_ord, 
  color = "Cashcrop", 
  axes = c(1,2)
) + 
  aes(shape = Site) + 
  geom_point(aes(colour = Cashcrop), alpha = 0.4, size = 4) + 
  geom_point(colour = "grey90", size = 1.5) + 
  scale_color_manual(values = c("#a65628", "red", "#ffae19", "#4daf4a", 
                                "#1919ff", "darkorchid3", "magenta")
  )

cap_plot
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
