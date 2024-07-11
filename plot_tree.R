# Install necessary packages if not already installed
# install.packages(c("ape", "tidyverse"))
#
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
#
# BiocManager::install("ggtree")
# BiocManager::install("ggtreeExtra")
# BiocManager::install("phyloseq")

# Load packages
library(ape)
library(ggtree)
library(tidyverse)
library(ggtreeExtra)
library(phyloseq)
library(dplyr)
library(ggplot2)

# Read the phylogenetic tree from a Newick file (replace 'path_to_tree_file' with your actual file path)
# tree <- read.tree("path_to_tree_file")
tree <- read.tree("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Trees/Trees/2_global_family_level.tre")
# For demonstration, we can use an example tree from ape
# data(woodmouse)
# tree <- nj(dist.dna(woodmouse))

# Plot the tree using ggtree in circular format
ggtree(tree, layout="circular") +
  geom_tiplab(aes(label=label), size=2, align=TRUE, linetype='dashed', linesize=0.5)

ggtree(tree, layout="circular") +
  geom_tiplab(aes(label=label), size=2, align=TRUE, linetype='dashed', linesize=0.5) +
  xlim(0, 100)
#
# # Define a base size for the plot
# base_size <- 12
#
# # Plot the tree using ggtree in circular format
# ggtree(tree, layout="circular") +
#   geom_tiplab(aes(label=label), size=base_size / 3, align=TRUE, linetype='dashed', linesize=0.5) +
#   xlim(0, 1.5 * base_size)

ggtree(tree, layout = "circular") +
  geom_tippoint() +
  geom_tiplab2(offset = 7, size=2) +
  xlim(-40, NA)

tree$tip.label




###### Find the CR species in the dataset ##################################################################
# library("ggplot2")
# library("ggtree")
# library("ggtreeExtra")

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

# load iucn data
iucn_banked_recalitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))

# load tree data
# trfile <- system.file("extdata", "tree.nwk", package="ggtreeExtra")
tr <- tree#read.tree(trfile)
numtip <- length(tr$tip.label)

# Get number of species per family that are CR and that are banked
fam_count = iucn_banked_recalitrance[,c("family", "banked", "accessions")] %>%
  group_by(family) %>%
  summarize(
    CR_species = n(),
    banked_species = length(which(banked)),
    total_accessisions = sum(accessions[!is.na(accessions)])
  )

test = data.frame(tr$tip.label) %>% left_join(fam_count,
                                  by = c("tr.tip.label" = "family"))

# data
dat2 <- data.frame(ID=tr$tip.label,
                   # Location=c(rep("HK", 50), rep("TW", 36), rep("SX", 30), rep("GD", 48),
                   #            rep("HN", 20), rep("AH", 20), rep("FJ", 26)),
                   Length=abs(rnorm(n=numtip, mean=0.6)),
                   # Group=c(rep("Yes", 200), rep("No", 30)),
                   Banked = test$banked_species,
                   CR = test$CR_species)
                   # Abundance=abs(rnorm(n=numtip, mean=10, sd=0.00000001)))
dat2[is.na(dat2)] = 0


p <- ggtree(tr, layout="circular", size=0.1) +
  geom_treescale(x=6, y=0, fontsize=1.2, linesize=0.3)

p + geom_fruit(data=dat2,
               geom=geom_bar,
               mapping=aes(y=ID, x=CR, fill=Banked),
               pwidth=0.7,
               stat="identity",
               orientation="y") +
  geom_tiplab2(offset = 7, size=2) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG")) +
  # scale_fill_manual(values=c("#D15FEE","#EE6A50","#FFC0CB",
  #                            "#8E8E38","#9ACD32","#006400",
  #                            "#8B4513"),
  # guide=guide_legend(keywidth=0.5, keyheight=0.5, order=6)) +
  theme(legend.position.inside=c(0.95, 0.5),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        legend.spacing.y = unit(0.02, "cm"))



