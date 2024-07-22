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
#
# dat3 = data.frame(id = test$tr.tip.label,
#                        height = test$CR_species)
# dat3[is.na(dat3)]=0
# # Plot the phylogenetic tree
# p <- ggtree(tr, layout = "circular") +
#   xlim(-10, 170) +
#   geom_fruit(data = dat,
#              geom = geom_bar,
#              mapping = aes(y = id, x = value, fill = group),
#              pwidth = 0.5,
#              stat = "identity",
#              orientation = "y",
#              offset = 0.05) +
#   scale_fill_manual(values = c("darkolivegreen", "grey", "#FFA500"),
#                     name = "")
#
# # Extract tip labels from the tree data
# tip_data <- p$data %>% filter(isTip) %>% left_join(dat, by = c("label" = "id"))
#
# p <- p %<+% tip_data +
#   aes(color = colour) +
#   geom_tiplab(aes(label=label), offset=24, size=2) +
#   scale_color_identity() +
#   # scale_colour_manual(values = c("darkolivegreen", "grey", "#FFA500", "black"))
#   scale_colour_manual(values = c( "#FFA500","darkolivegreen","grey",  "black"),
#                       labels = c("Unbanked CR", "Banked CR", "Not CR", "NA")) +
#   guides(colour = "none")
#
# p <- p + geom_fruit(data=dat3,
#                geom=geom_bar,
#                mapping=aes(y=id, x=height),
#                pwidth=4,
#                stat="identity",
#                orientation="y",
#                offset = 0.8,
#                color= "black")
#
#
#
# print(p)
#
# ggsave(paste0(plotpath, "/phylo_banked_proportion_number.pdf"),
#        width = 40,
#        height = 40,
#        units = "cm")
#
# ggsave(paste0(plotpath, "/phylo_banked_proportion_number.png"),
#        width = 40,
#        height = 40,
#        units = "cm")
#




# # data
# dat2 <- data.frame(ID=tr$tip.label,
#                    # Location=c(rep("HK", 50), rep("TW", 36), rep("SX", 30), rep("GD", 48),
#                    #            rep("HN", 20), rep("AH", 20), rep("FJ", 26)),
#                    Length=abs(rnorm(n=numtip, mean=0.6)),
#                    # Group=c(rep("Yes", 200), rep("No", 30)),
#                    Banked = test$banked_species,
#                    CR = test$CR_species)
#                    # Abundance=abs(rnorm(n=numtip, mean=10, sd=0.00000001)))
# # dat2[is.na(dat2)] = 0
#
#
# # p <- ggtree(tr, layout="circular", size=0.1) +
# #   geom_treescale(x=6, y=0, fontsize=1.2, linesize=0.3)
#
# p <- ggtree(tree, layout = "circular") +
#   # geom_tippoint() +
#   # geom_tiplab2(offset = 7, size=2) +
#   xlim(-40, NA)
#
# p + geom_fruit(data=dat2,
#                geom=geom_bar,
#                mapping=aes(y=ID, x=CR, fill=Banked),
#                pwidth=0.7,
#                stat="identity",
#                orientation="y") +
#   # geom_tiplab2(data=dat2,
#   #              mapping= aes(y=ID, x=CR),
#   #              hjust = x, size=2) +
#   # geom_tiplab2( size=0.2, offset = 2) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(9, "YlOrRd")) +
#   # scale_fill_manual(values=c("#D15FEE","#EE6A50","#FFC0CB",
#   #                            "#8E8E38","#9ACD32","#006400",
#   #                            "#8B4513"),
#   # guide=guide_legend(keywidth=0.5, keyheight=0.5, order=6)) +
#   theme(legend.position.inside=c(0.95, 0.5),
#         legend.title=element_text(size=7),
#         legend.text=element_text(size=6),
#         legend.spacing.y = unit(0.02, "cm"))
#
#
# # format the histogram data in R
# data = rbind(data.frame(id = test$tr.tip.label,
#                         group = "CR",
#                         value = (test$CR_species-test$banked_species)),
#              data.frame(id = test$tr.tip.label,
#                         group = "Banked",
#                         value = test$banked_species))
#
#
# # Get angles
# number_of_bar = nrow(test)
# index = data.frame(tr$tip.label)
# index$node = 1:nrow(index)
# test = test %>% left_join(index, by = c("tr.tip.label"="tr.tip.label"))
#
# angle = 0 - 360 * (test$node-0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# test$angle <- ifelse((angle < -90 & angle > -270), angle + 180, angle)
# test$hjust <- ifelse(((angle < -90) & (angle > -270)), 1, 0)
# test$height <- rowSums(test[,c("CR_species","banked_species")],
#                        na.rm=T)
#
# # plot
# p <- ggtree(tree, layout = "circular") +
#   xlim(0, 50) +
#   geom_fruit(data=data,
#              geom=geom_bar,
#              mapping=aes(y=id, x=value, fill=group),
#              pwidth=1.2,
#              stat="identity",
#              orientation="y") +
#   scale_fill_manual(values = c("#0000FF","#FFA500")) +
#   theme(legend.position.inside=c(0.95, 0.5),
#         legend.title=element_text(size=7),
#         legend.text=element_text(size=6),
#         legend.spacing.y = unit(0.02, "cm")) +
#   geom_cladelab(node = which(!is.na(test$CR_species)),
#                 label = test$tr.tip.label[which(!is.na(test$CR_species))],#test$tr.tip.label[which(!is.na(test$CR_species))],
#                 angle = test$angle[which(!is.na(test$CR_species))],
#                 hjust = test$hjust[which(!is.na(test$CR_species))],
#                 x = test$node[which(!is.na(test$CR_species))],
#                 offset = 8,
#                 # offset = test$height[which(!is.na(test$CR_species))]/8,
#                 fontsize = 2)
# p
#
# ggsave(paste0(plotpath, "/phylo_banked.pdf"),
#        width = 20,
#        height = 20,
#        units = "cm")
#
#
#


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
  xlim(-10, 70) +
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
  # geom_tiplab2(label = test$tr.tip.label,
  #              offset = 10,
  #              size=2,
  #              hjust = 1)
  # geom_tiplab(aes(label=label), offset=24, size=2) +
  scale_color_identity() +
  # scale_colour_manual(values = c("darkolivegreen", "grey", "#FFA500", "black"))
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


########################################################

p <- ggtree(tree, layout = "circular") +
  xlim(0, 80) +
  geom_fruit(data=dat2,
             geom=geom_bar,
             mapping=aes(y=id, x=value, fill=group),
             pwidth=5,
             # align=TRUE,
             stat="identity",
             orientation="y",
             offset = 0.3) +
  scale_fill_manual(values = c("darkolivegreen", "grey", "#FFA500"),
                    name = "") +
  theme(legend.position.inside=c(0.95, 0.5),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        legend.spacing.y = unit(0.02, "cm")) +
  geom_tiplab2(label = test$tr.tip.label,
               offset = 10,
               size=2,
               hjust = 1)
  p


ggsave(paste0(plotpath, "/phylo_banked_outer.pdf"),
       width = 40,
       height = 40,
       units = "cm")

ggsave(paste0(plotpath, "/phylo_banked_outer.png"),
       width = 40,
       height = 40,
       units = "cm")
#
#
# ########################################################################
# # format the histogram data in R
# dat = rbind(data.frame(id = test$tr.tip.label,
#                        group = "Unbanked CR",
#                        value = (test$CR_species-test$banked_species),
#                        label = test$tr.tip.label),
#             data.frame(id = test$tr.tip.label,
#                        group = "Banked CR",
#                        value = test$banked_species,
#                        label = test$tr.tip.label))
#
# p <- ggtree(tree, layout = "circular") +
#   xlim(-40, 80) +
#   geom_fruit(data=dat,
#              geom=geom_bar,
#              mapping=aes(y=id, x=value, fill=group),
#              pwidth=5,
#              stat="identity",
#              orientation="y",
#              offset = 0.4) +
#   scale_fill_manual(values = c("#0000FF","#FFA500")) +
#   theme(legend.position.inside=c(0.95, 0.5),
#         legend.title=element_text(size=7),
#         legend.text=element_text(size=6),
#         legend.spacing.y = unit(0.02, "cm")) +
#   geom_tiplab2(aes(label=label),
#                offset = 15,
#                size=2,
#                hjust = 1,
#                geom = "text",
#                bg.colour = "white",
#                linetype = "dotted"
#                )
# p
#
# ggsave(paste0(plotpath, "/phylo_banked_inside.pdf"),
#        width = 40,
#        height = 40,
#        units = "cm")
#
#
#
#
# ###### PROPORTION ############################
# test2 = test
# test2[is.na(test2)] = 0
# test2$colour  = ifelse(test2$banked_species > 0, "Banked CR",
#                       ifelse(test2$CR_species >0, "Unbanked CR", "No CR"))
# test2$colour[is.na(test2$colour)] = "No CR"
#
# dat = rbind(data.frame(id = test2$tr.tip.label,
#                        group = "Unbanked CR",
#                        value = ((test2$CR_species-test2$banked_species)/(test2$CR_species)),
#                        colour = test2$colour),
#             data.frame(id = test2$tr.tip.label,
#                        group = "Banked CR",
#                        value = test2$banked_species/(test2$CR_species),
#                        colour = test2$colour),
#             data.frame(id = test2$tr.tip.label,
#                        group = "No CR",
#                        value = ifelse(is.na(1 - (((test2$CR_species-test2$banked_species)/
#                                        (test2$CR_species))+
#                                       test2$banked_species/
#                                       (test2$CR_species))),1,0),
#                        colour = test2$colour))
#
# # plot
# p <- ggtree(tr, layout = "circular") +
#   xlim(-10, 100) +
#   geom_fruit(data=dat,
#              geom=geom_bar,
#              mapping=aes(y=id, x=value, fill=group),#, color = colour
#              pwidth=0.5,
#              stat="identity",
#              orientation="y",
#              offset = 0.05) +
#   scale_fill_manual(values = c("darkolivegreen", "grey","#FFA500")) + #"#FFA500"
#   geom_tiplab(geom = "text",
#               label = id,
#               offset = 24,
#               size=2.4,
#               hjust = 0
#   ) +
#   theme(legend.position.inside=c(0.95, 0.5),
#         legend.title=element_text(size=7),
#         legend.text=element_text(size=6),
#         legend.spacing.y = unit(0.02, "cm")) +
#     scale_color_manual(values= c("darkolivegreen", "grey","#FFA500"))
# p
# ggsave(paste0(plotpath, "/phylo_banked_proportion.pdf"),
#        width = 40,
#        height = 40,
#        units = "cm")
#


