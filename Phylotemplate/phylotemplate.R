#Phyloseq template
#Kent Pham 2/14/2024

####Script preparation####
if (!require("BiocManager", quietly = TRUE))                    #Checks to see if BiocManager is already downloaded. Tool is used to install bioconducter packages
  install.packages("BiocManager")                               #If not yet downloaded, downloads.

BiocManager::install("phyloseq")                                #Installs more updated version
BiocManager::install("microbiome")                              #Installs more updated version
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") #Github retrieval of package

library(ggplot2)        #Used for plotting. Can replace some packages with tidyverse
library(vegan)          #Data analysis
library(plyr)           #Data manipulation
library(dplyr)          #Data manipulation
library(scales)         #Overrides ggplot2 scaling infrastructure. Used to modify plots
library(grid)           #Allows manipulation of graphical objects
library(reshape2)       #melt function allows for data aggregation
library(phyloseq)       #Primary analysis tool
library(microbiome)     #Used to aggregate and merge data files. Also used for exporting phyloseq data for import into other analyses packages. 
library(pairwiseAdonis) #Used for pairwise PERMANOVA

theme_set(theme_bw()) #Sets default plotting theme


####Loading in data from mothur####
sharedfile = "FileName.shared"     #Change FileName to name of .shared file
taxfile = "FileName.cons.taxonomy" #Change FileName to name of .cons.taxonomy
mapfile = "MetadataTemplate.csv"   #Change FileName to name of metadata file

mothur_data = import_mothur(mothur_shared_file = sharedfile,
                             mothur_constaxonomy_file = taxfile) #Imports data files and merges into mothur_data file
map = read.csv(mapfile)                 #Reads in metadata
map = sample_data(map)                  #Converts metadata into sample data filetype
rownames(map) <- map$group              #Assign rownames to be Sample ID's

moth_merge <- merge_phyloseq(mothur_data, map) #Merges mothur data with metadata
colnames(tax_table(moth_merge)) <- c("Kingdom", "Phylum", "Class", 
                                     "Order", "Family", "Genus") #Creates taxonomy table within mothur data

Clean <- moth_merge %>%          #Secondary cleaning stage to ensure bacterial only reads
  subset_taxa(
    Kingdom == "Bacteria" &
      Family  != "mitochondria" &
      Class   != "Chloroplast"
  )


####Phylum Graph####
Clean_phylum <- Clean %>%
  tax_glom(taxrank = "Phylum") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to relative abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.01) %>%                         # Filter out low abundance taxa. Here is where you can change appearance threshold
  arrange(Phylum)                                      # Sort data frame alphabetically by phylum

# Plot Phylum taxonomy
phylum_colors <- c(   #Generates custom color palette
  "#CBD588", "#5F7FC7", "orange","#DA5724", "#508578", "#CD9BCD",
  "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861"
)

ggplot(Clean_phylum, aes(x = Group, y = Abundance, fill = Phylum)) + #X axis are samples, Y axis is relative abundance, colors are unique phyla
  facet_grid(vars(Treatment1),vars(Treatment2)) + #Used for grouping samples by various treatments
  geom_bar(stat = "identity", width = .85) +      #Generates bar graph
  scale_fill_manual(values = phylum_colors) +     #Replaces colors with custom palette
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + #Changes x axis to fit labels
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) + #Modifies legend
  ylab("Relative Abundance (Phyla > 1%) \n") +    #Changes Y axis label
  ggtitle("Title")                                #Changes figure title

####Genus Graph####
Clean_genus <- Clean %>%
  tax_glom(taxrank = "Genus") %>%                     # agglomerate at genus level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa. Here is where you can change appearance threshold
  filter(!grepl("unclassified", Genus)) %>%            # Removes unclassified groups
  arrange(Genus)                                      # Sort data frame alphabetically by genera

# Plot
genus_colors <- c(  #Generates custom color palette
  "#CBD588", "#5F7FC7", "orange","#DA5724", "#508578", "#CD9BCD",
  "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861"
)
ggplot(Clean_genus, aes(x = Group, y = Abundance, fill = Genus)) + #X axis are samples, Y axis is relative abundance, colors are unique genera
  facet_grid(vars(Treatment1),vars(Treatment2)) + #Used for grouping samples by various treatments
  geom_bar(stat = "identity", width = .85) +    #Generates bar graph
  scale_fill_manual(values = genus_colors) +    #Replaces colors with custom palette
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + #Changes x axis to fit labels
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) + #Modifies legend
  ylab("Relative Abundance (Genera > 2%) \n") + #Changes Y axis label 
  ggtitle("Title")                              #Changes figure title


####Ordination no arrows ####
# Ordinate
Clean_pcoa <- ordinate(   #Ordination function
  physeq = Clean,         #Data file used to create ordination
  method = "PCoA",        #Choose method of dimensional reduction
  distance = "bray"       #Choose distance/dissimilarity calculator
)

# Plot 
plot_ordination(           #Generates a scatter plot
  physeq = Clean,          #Data file used to create ordination
  ordination = Clean_pcoa, #Takes distance/dissimilarity matrix from previous step
  color = "Treatment1",    #Change Treatment1 to treatment label
  shape = "Treatment2",    #Additional way of separating data
  title = "Title"          #Changes title
) + 
  scale_color_manual(values = c( #Custom color palette
    "#5F7FC7", "orange", "#508578", 
    "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
    "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861"
  )
  ) +
  geom_point(alpha = 0.7, size = 4) +     #Used to modify shape and infill of points
  scale_shape_manual(values = c(15:18)) + #Used for choosing custom shapes
  stat_ellipse(aes(group=Treatment1)) +   #Generates statistical ellipses at 95% confidence interval
  geom_point(size = 1.5)                  #Creates smaller shape inside original shape for more contrast
  
####Permanova####
Clean_bray <- phyloseq::distance(Clean, method = "bray")       #Generates distance matrix
sampledf <- data.frame(sample_data(moth_merge))                #Make a data frame from the sample_data. Creates new random sample data every time it is run
adonis2(Clean_bray ~ Treatment1 + Treatment2, data = sampledf) #Adonis test used to perform a permanova. Can add more treatments to test for interactions
pairwise.adonis2(Clean_bray ~ Treatment1, data = sampledf)     #Pairwise test to get letters in case of significant differences
####Ordination with arrows####

bray<- phyloseq::distance(physeq = Clean, method = "bray") #Generates new bray-curtis dissimilarity matrix
# CAP ordinate
cap_ord <- ordinate(                            #Generates correlation arrows as well as PCoA
  physeq = Clean,                               #Data file used to create ordination
  method = "CAP",                               #Constrained Analysis of Principal Coordinates
  distance = bray,                              #Uses premade matrix
  formula = ~ metadata1 + metadata2 + metadata3 #Replace with numerical metadata ie yield, nutrient levels, etc.
)
# CAP plot
cap_plot <- plot_ordination( #Used to generate the initial PCoA plot
  physeq = Clean,            #Dataset you are analyzing
  ordination = cap_ord,      #Uses the ordination generated in previous step
  color = "Treatment1",      #Selects first variable for color
  axes = c(1,2)              #Takes the first two axes which should be most significant. Theoretically you could use other ones but not advised
) + 
  aes(shape = Treatment2) +                                     #Shape if needed. Can comment out if only one variable
  geom_point(aes(colour = Treatment1), alpha = 0.4, size = 4) + #Creates initial point
  geom_point(colour = Treatment1, size = 1.5) +                 #Adds an inner point for clarity
  scale_color_manual(values = c(                                #Custom color palette
    "#5F7FC7", "orange", "#508578", 
    "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
    "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861"
  ))

cap_plot #Displays the initial PCoA

# This adds the arrows
arrowmat <- vegan::scores(cap_ord, display = "bp")           #Returns matrix of scores from ordination
arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat) # Add labels, makes a data frame from scores
arrow_map <- aes(xend = CAP1,                                # Defines the arrow aesthetics as well as origin
                 yend = CAP2, 
                 x = 0, 
                 y = 0, 
                 shape = NULL, 
                 color = NULL, 
                 label = labels)

label_map <- aes(x = 1.3 * CAP1, #Creates an aesthetic list of the names of your metadata
                 y = 1.3 * CAP2, #Offsets it from the arrow. Adjust the number to move closer or farther
                 shape = NULL, 
                 color = NULL, 
                 label = labels)

arrowhead = arrow(length = unit(0.02, "npc")) #Defines the arrowhead used

# Combine the graphics
cap_plot +                    #Takes original PCoA and adds the arrows
  geom_segment(
    mapping = arrow_map,      #Defines what you are mapping
    size = .5,                #Defines size of arrows
    data = arrowdf,           #Arrow dataframe
    color = "gray",           #Chooses color for arrows
    arrow = arrowhead         #Selects previously defined arrow
  ) + 
  geom_text(
    mapping = label_map,      #Defines what youare mapping
    size = 4,                 #Size of the text
    data = arrowdf,           #Text dataframe
    show.legend = FALSE       #Removes legend
  )
