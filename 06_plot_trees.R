# # Install necessary packages if not already installed
# install.packages(c("ape", "tidyverse"))
#
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
#
# BiocManager::install("ggtree")
#
# BiocManager::install("phyloseq")
#
# install.packages("installr")
# library("installr")
# uninstall.packages("cli")
# uninstall.packages("utf8")
# uninstall.packages("vctrs")
# install.packages("cli")
# install.packages("utf8")
# install.packages("vctrs")
# BiocManager::install("ggtreeExtra")

# Load packages
library(ape)
library(ggtree)
library(tidyverse)
library(ggtreeExtra)
library(phyloseq)
library(dplyr)
library(ggplot2)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"
plotpath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code"
# load iucn data
iucn_banked_recalitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))



# Read the phylogenetic tree from Zuntini
tree <- read.tree("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Trees/Trees/2_global_family_level.tre")

# load tree data
tr <- tree
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


colorz  = ifelse(test$banked_species > 0, "darkolivegreen",
                 ifelse(test$CR_species >0, "#FFA500","grey"))
colorz[is.na(colorz)] = "grey"



dat = rbind(data.frame(id = test$tr.tip.label,
                       group = "Unbanked CR species in family",
                       value = ((test$CR_species - test$banked_species)/
                                  (test$CR_species)),
                       colour = colorz),
            data.frame(id = test$tr.tip.label,
                       group = "Banked CR species in family",
                       value = test$banked_species/(test$CR_species),
                       colour = colorz),
            data.frame(id = test$tr.tip.label,
                       group = "No CR species in family",
                       value = ifelse(is.na(1 - (((test$CR_species - test$banked_species)/
                                                    (test$CR_species))+
                                                   test$banked_species/
                                                   (test$CR_species))),1,0),
                       colour = colorz))

proportions = dat$value[dat$group == "Banked CR species in family"]

# % families with CR
length(which(!is.na(proportions)))/length(proportions) *100

# % families with banked CR
length(which(proportions > 0))/length(proportions) *100

# % CR that have some banked
length(which(proportions > 0))/length(which(!is.na(proportions))) *100

# % CR that have 50% banked
length(which(proportions > 0.50))/length(which(!is.na(proportions))) *100

# % CR that have 99% banked
length(which(proportions > 0.99))/length(which(!is.na(proportions))) *100

# Names of fanmilies with CR that are 99% banked
dat$id[which(dat$group == "Banked CR species in family" & dat$value >0.99)]



# Plot the phylogenetic tree
p <- ggtree(tr, layout = "circular") +
  xlim(-10, 70) +
  geom_fruit(data = dat,
             geom = geom_bar,
             mapping = aes(y = id, x = value, fill = group),
             pwidth = 0.5,
             stat = "identity",
             orientation = "y",
             offset = 0.05) +
  scale_fill_manual(values = c("darkolivegreen", "grey", "#FFA500"),
                    name = "")

# Extract tip labels from the tree data
tip_data <- p$data %>% filter(isTip) %>% left_join(dat, by = c("label" = "id"))

p <- p %<+% tip_data +
  aes(color = colour) +
  geom_tiplab(aes(label=label), offset=24, size=2) +
  scale_color_identity() +
  # scale_colour_manual(values = c("darkolivegreen", "grey", "#FFA500", "black"))
  scale_colour_manual(values = c( "#FFA500","darkolivegreen","grey",  "black"),
                      labels = c("Unbanked CR", "Banked CR", "Not CR", "NA")) +
  guides(colour = "none")

print(p)

ggsave(paste0(plotpath, "/phylo_banked_proportion_textcol.pdf"),
       width = 20,
       height = 20,
       units = "cm")

ggsave(paste0(plotpath, "/phylo_banked_proportion_textcol.png"),
       width = 25,
       height = 25,
       units = "cm")

##################
