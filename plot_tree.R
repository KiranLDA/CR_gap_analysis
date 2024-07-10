# Install necessary packages if not already installed
# install.packages(c("ape", "ggtree", "tidyverse"))

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

# Load packages
library(ape)
library(ggtree)
library(tidyverse)

# Read the phylogenetic tree from a Newick file (replace 'path_to_tree_file' with your actual file path)
# tree <- read.tree("path_to_tree_file")

# For demonstration, we can use an example tree from ape
data(woodmouse)
tree <- nj(dist.dna(woodmouse))

# Plot the tree using ggtree in circular format
ggtree(tree, layout="circular") +
  geom_tiplab(aes(label=label), size=2, align=TRUE, linetype='dashed', linesize=0.5)
