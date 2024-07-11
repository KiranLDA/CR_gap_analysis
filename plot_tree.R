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

# Plot the tree using ggtree in circular format
# ggtree(tree, layout="circular") +
#   geom_tiplab(aes(label=label), size=2, align=TRUE, linetype='dashed', linesize=0.5)
#
# ggtree(tree, layout="circular") +
#   geom_tiplab(aes(label=label), size=2, align=TRUE, linetype='dashed', linesize=0.5) +
#   xlim(0, 100)
#
#
# ggtree(tree, layout = "circular") +
#   geom_tippoint() +
#   geom_tiplab2(offset = 7, size=2) +
#   xlim(-40, NA)




###### Find the CR species in the dataset ##################################################################

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

# data
dat2 <- data.frame(ID=tr$tip.label,
                   # Location=c(rep("HK", 50), rep("TW", 36), rep("SX", 30), rep("GD", 48),
                   #            rep("HN", 20), rep("AH", 20), rep("FJ", 26)),
                   Length=abs(rnorm(n=numtip, mean=0.6)),
                   # Group=c(rep("Yes", 200), rep("No", 30)),
                   Banked = test$banked_species,
                   CR = test$CR_species)
                   # Abundance=abs(rnorm(n=numtip, mean=10, sd=0.00000001)))
# dat2[is.na(dat2)] = 0


# p <- ggtree(tr, layout="circular", size=0.1) +
#   geom_treescale(x=6, y=0, fontsize=1.2, linesize=0.3)

p <- ggtree(tree, layout = "circular") +
  # geom_tippoint() +
  # geom_tiplab2(offset = 7, size=2) +
  xlim(-40, NA)

p + geom_fruit(data=dat2,
               geom=geom_bar,
               mapping=aes(y=ID, x=CR, fill=Banked),
               pwidth=0.7,
               stat="identity",
               orientation="y") +
  # geom_tiplab2(data=dat2,
  #              mapping= aes(y=ID, x=CR),
  #              hjust = x, size=2) +
  # geom_tiplab2( size=0.2, offset = 2) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  # scale_fill_manual(values=c("#D15FEE","#EE6A50","#FFC0CB",
  #                            "#8E8E38","#9ACD32","#006400",
  #                            "#8B4513"),
  # guide=guide_legend(keywidth=0.5, keyheight=0.5, order=6)) +
  theme(legend.position.inside=c(0.95, 0.5),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        legend.spacing.y = unit(0.02, "cm"))


# format the histogram data in R
data = rbind(data.frame(id = test$tr.tip.label,
                        group = "CR",
                        value = (test$CR_species-test$banked_species)),
             data.frame(id = test$tr.tip.label,
                        group = "Banked",
                        value = test$banked_species))


# Get angles
number_of_bar = nrow(test)
index = data.frame(tr$tip.label)
index$node = 1:nrow(index)
test = test %>% left_join(index, by = c("tr.tip.label"="tr.tip.label"))

angle = 0 - 360 * (test$node-0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
test$angle <- ifelse((angle < -90 & angle > -270), angle + 180, angle)
test$hjust <- ifelse(((angle < -90) & (angle > -270)), 1, 0)
test$height <- rowSums(test[,c("CR_species","banked_species")],
                       na.rm=T)

# plot
p <- ggtree(tree, layout = "circular") +
  xlim(0, 50) +
  geom_fruit(data=data,
             geom=geom_bar,
             mapping=aes(y=id, x=value, fill=group),
             pwidth=1.2,
             stat="identity",
             orientation="y") +
  scale_fill_manual(values = c("#0000FF","#FFA500")) +
  theme(legend.position.inside=c(0.95, 0.5),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        legend.spacing.y = unit(0.02, "cm")) +
  geom_cladelab(node = which(!is.na(test$CR_species)),
                label = test$tr.tip.label[which(!is.na(test$CR_species))],#test$tr.tip.label[which(!is.na(test$CR_species))],
                angle = test$angle[which(!is.na(test$CR_species))],
                hjust = test$hjust[which(!is.na(test$CR_species))],
                x = test$node[which(!is.na(test$CR_species))],
                offset = 8,
                # offset = test$height[which(!is.na(test$CR_species))]/8,
                fontsize = 2)
p

ggsave(paste0(plotpath, "/phylo_banked.pdf"),
       width = 20,
       height = 20,
       units = "cm")



p <- ggtree(tree, layout = "circular") +
  xlim(0, 80) +
  geom_fruit(data=data,
             geom=geom_bar,
             mapping=aes(y=id, x=value, fill=group),
             pwidth=5,
             stat="identity",
             orientation="y",
             offset = 0.3) +
  scale_fill_manual(values = c("#0000FF","#FFA500")) +
  theme(legend.position.inside=c(0.95, 0.5),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        legend.spacing.y = unit(0.02, "cm")) +
  geom_tiplab2(label = test$tr.tip.label,
               offset = 10, size=2, hjust = 1)
  # geom_cladelab(node = which(!is.na(test$CR_species)),
  #               label = test$tr.tip.label[which(!is.na(test$CR_species))],#test$tr.tip.label[which(!is.na(test$CR_species))],
  #               angle = test$angle[which(!is.na(test$CR_species))],
  #               hjust = test$hjust[which(!is.na(test$CR_species))],
  #               x = test$node[which(!is.na(test$CR_species))],
  #               offset = 2,
  #               # offset = test$height[which(!is.na(test$CR_species))]/8,
  #               fontsize = 2)
p

ggsave(paste0(plotpath, "/phylo_banked_outer.pdf"),
       width = 40,
       height = 40,
       units = "cm")

  # geom_tiplab2(data = filtered_data,
  #              mapping = aes(y=id, x=value, label=id), # mapping=aes(y=node, x=label, label=label),
  #              size=2, node = id)
  # geom_tiplab2(aes(subset = !is.na(data$value)),
  #             offset = 7, size=2)


  # geom_text(data=filtered_data,
  #           aes(y=node, x=loc, label=label),
  #           vjust=-0.5, size=2) +  # Adjust 'vjust' and 'size' as needed
 #+
  # geom_tiplab2(y = lab_data$label,
  #              h_just = lab_data$loc,
  #              label = lab_data$label, size=1) +
  # geom_tiplab2(data = data,
  #              hjust = data$value, size=2) +
  # geom_tiplab2(size=0.2, offset) +
  # geom_tiplab2(y = lab_data$label,
  #              h_just = lab_data$loc,
  #              label = lab_data$label, size=1) +
  # geom_cladelab(node=which(!is.na(test$CR_species)),
  #               label=test$tr.tip.label[which(!is.na(test$CR_species))]
  #               ) +
  # geom_tiplab2(test$CR_species[], size=0.2, offset = 2) +



# geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
