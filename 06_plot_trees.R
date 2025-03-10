# # Install necessary packages if not already installed
# install.packages(c("ape", "tidyverse"))
#
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
#
#
# #
# install.packages("installr")
# library("installr")
# uninstall.packages("cli")
# uninstall.packages("utf8")
# uninstall.packages("vctrs")
# install.packages("cli")
# install.packages("utf8")
# install.packages("vctrs")
# BiocManager::install("GenomeInfoDbData")
# BiocManager::install("ggtree")#, force = T)
# BiocManager::install("ggtreeExtra")
# BiocManager::install("phyloseq")#, force=T)

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

# What families have nothing banked?
fam_count$family[which(fam_count$banked_species == 0)]

# What CR families have nothing banked?
fam_count$family[which(fam_count$banked_species == 0 & fam_count$CR_species > 0)]

# What CR families have nothing banked?
fam_count$family[which(fam_count$CR_species > 0)]

# what proportion of families with CR species are banked
length(fam_count$family[which(fam_count$banked_species == 0 & fam_count$CR_species > 0)])/length(fam_count$family[which(fam_count$CR_species > 0)])
# 0.6260504

# what families have lost of CR species and no collections
fam_count$family[which(fam_count$banked_species == 0 &
                         fam_count$CR_species > 50)]
# "Annonaceae"       "Araliaceae"       "Bromeliaceae"     "Dipterocarpaceae"
# "Fagaceae"         "Lauraceae"        "Zingiberaceae"


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



##########################################
### GET STATS
##########################################

proportions = dat$value[dat$group == "Banked CR species in family"]

# % families with CR
length(which(!is.na(proportions)))/length(proportions) *100
# 47.24771

# % families with banked CR
length(which(proportions > 0))/length(proportions) *100
# 19.26606

# % CR that have some banked
length(which(proportions > 0))/length(which(!is.na(proportions))) *100

# % CR that have 50% banked
length(which(proportions > 0.50))/length(which(!is.na(proportions))) *100
# 40.7767

# % CR that have 99% banked
length(which(proportions > 0.99))/length(which(!is.na(proportions))) *100
# 4.854369

# Names of fanmilies with CR that are 99% banked
dat$id[which(dat$group == "Banked CR species in family" & dat$value >0.99)]
# [1] "Moringaceae"     "Cistaceae"       "Onagraceae"      "Kewaceae"
# [5] "Frankeniaceae"   "Byblidaceae"     "Calceolariaceae" "Stylidiaceae"
# [9] "Pennantiaceae"   "Nyssaceae"

# Stats for big families
test$CR_species[test$tr.tip.label == "Rubiaceae"] #342
test$CR_species[test$tr.tip.label == "Myrtaceae"] #297
test$CR_species[test$tr.tip.label == "Lauraceae"] #290
test$CR_species[test$tr.tip.label == "Fabaceae"] #286
test$CR_species[test$tr.tip.label == "Orchidaceae"] #268
test$CR_species[test$tr.tip.label == "Asteraceae"] #247

#############################################
# Plot the phylogenetic tree
#############################################

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


######################################################################

p <- ggtree(tr, layout = "circular") +
  xlim(-10, 90) +
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
tip_data <- tip_data %>% left_join(test[,c("tr.tip.label","CR_species")],
                                   by = c("label" = "tr.tip.label"))
tip_data$CR_species[is.na(tip_data$CR_species)] = 0
tip_data$label2 <- paste0("(n = ",tip_data$CR_species,")")

p <- p %<+% tip_data +
  aes(color = colour) +
  geom_tiplab(aes(label = label2), offset=24, size=1) +
  scale_color_identity() +
  geom_tiplab(aes(label = label), offset=30, size=1.5) +
  scale_color_identity() +
  scale_colour_manual(values = c( "#FFA500","darkolivegreen","grey",  "black"),
                      labels = c("Unbanked CR", "Banked CR", "Not CR", "NA")) +
  guides(colour = "none",
         fill = "none")

print(p)

ggsave(paste0(plotpath, "/phylo_banked_proportion_textcol_n.pdf"),
       width = 20,
       height = 20,
       units = "cm")

ggsave(paste0(plotpath, "/phylo_banked_proportion_textcol_n.png"),
       width = 25,
       height = 25,
       units = "cm")


############################################
# Raw data tree
############################################

dat2 = rbind(data.frame(id = test$tr.tip.label,
                        group = "Unbanked CR species in family",
                        value = (test$CR_species - test$banked_species),
                        colour = colorz),
             data.frame(id = test$tr.tip.label,
                        group = "Banked CR species in family",
                        value = test$banked_species,
                        colour = colorz),
             data.frame(id = test$tr.tip.label,
                        group = "No CR species in family",
                        value = 0,
                        colour = colorz))

dat2$value[is.na(dat2$value)] = 0


# Plot the phylogenetic tree
p <- ggtree(tr, layout = "circular") +
  xlim(-10, 80) +
  geom_fruit(data = dat2,
             geom = geom_bar,
             mapping = aes(y = id, x = value, fill = group),
             pwidth = 0.7,
             stat = "identity",
             orientation = "y",
             offset =  0.35) +
  scale_fill_manual(values = c("darkolivegreen", "grey", "#FFA500"),
                    name = "")

# Extract tip labels from the tree data
tip_data <- p$data %>% filter(isTip) %>% left_join(dat2, by = c("label" = "id"))

p <- p %<+% tip_data +
  aes(color = colour) +
  geom_tiplab(aes(label=label), offset=13, size=2, hjust = 1) +
  scale_color_identity() +
  scale_colour_manual(values = c( "#FFA500","darkolivegreen","grey",  "black"),
                      labels = c("Unbanked CR", "Banked CR", "Not CR", "NA")) +
  guides(colour = "none")

print(p)


ggsave(paste0(plotpath, "/phylo_banked_raw.pdf"),
       width = 40,
       height = 40,
       units = "cm")

ggsave(paste0(plotpath, "/phylo_banked_raw.png"),
       width = 40,
       height = 40,
       units = "cm")




