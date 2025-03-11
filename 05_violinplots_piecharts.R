# library("rnaturalearth")
# library("rnaturalearthdata")
library(geodata)
library(dplyr)
library(stringdist)
library(sf)
library(biscale)
library(ggplot2)
library(cowplot)
library(vioplot)
library(viridis)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

##########################################################################################################
#####  VIOLIN PLOTS    ###################################################################################
##########################################################################################################


indexes = read.csv(paste0(basepath,"iucn_brahms_indexes_targets.csv"))
# indexes$total_index = ((indexes$information_index + indexes$viability_index +
#                           indexes$genetic_index)/3)
indexes$cultivation_index = ifelse(indexes$CultivatedAll == FALSE, NA, indexes$cultivation_index)
indexes$exsitu_index = ifelse(indexes$CultivatedAll == TRUE, NA, indexes$exsitu_index)


#########################
##  VIOLIN PLOT: MSBP  ##
#########################

stacked_MSBP <- stack(indexes[, c("information_index", "viability_index", "cultivation_index",
                                  "exsitu_index", "total_index")])
stacked_MSBP = stacked_MSBP[!is.na(stacked_MSBP$values),]
stacked_MSBP$ind<- factor(stacked_MSBP$ind, levels=c("information_index", "viability_index", "cultivation_index",
                                                     "exsitu_index", "total_index"))


par(mar = c(5,5,3,3))
vioplot(stacked_MSBP$values ~ stacked_MSBP$ind,
        col="darkmagenta",
        ylab="Index", xlab="", xaxt="n")
axis(side=1, at=1:5,labels=c("Information", "Viability", "Cultivation", "Field Site", "Total"))
legend("bottomleft", legend = c("MSB-P"), fill = c("darkmagenta"), cex = 1)


############################
##  VIOLIN PLOT: MSB & P  ##
############################

# MSB
stacked_MSB <- stack(indexes[indexes$SEEDBANK == "MSB", c("information_index", "viability_index", "cultivation_index",
                                                          "exsitu_index", "total_index")])
stacked_MSB = stacked_MSB[!is.na(stacked_MSB$values),]
stacked_MSB$ind<- factor(stacked_MSB$ind, levels=c("information_index", "viability_index", "cultivation_index",
                                                   "exsitu_index", "total_index"))

# Partners
stacked_P <- stack(indexes[indexes$SEEDBANK != "MSB", c("information_index", "viability_index", "cultivation_index",
                                                        "exsitu_index", "total_index")])
stacked_P = stacked_P[!is.na(stacked_P$values),]
stacked_P$ind<- factor(stacked_P$ind, levels=c("information_index", "viability_index", "cultivation_index",
                                               "exsitu_index", "total_index"))


# Plot
vioplot(stacked_MSB$values ~ stacked_MSB$ind, side="left",
        col="darkorange",
        ylab="Index", xlab="", xaxt="n")

vioplot(stacked_P$values ~ stacked_P$ind,side="right",
        col="cornflowerblue",
        ylab="Index", xlab="",xaxt="n",
        add=T)
axis(side=1, at=1:5,labels=c("Information", "Viability", "Cultivation", "Field Site", "Total"))
legend("bottomleft", legend = c("MSB", "Partners"), fill = c("darkorange", "cornflowerblue"), cex = 1)



##########################
## GGPLOT VIOLIN      ####
##########################

stacked_MSBP <- stack(indexes[, c("total_index", "genetic_index", "viability_index","information_index")])
stacked_MSBP = stacked_MSBP[!is.na(stacked_MSBP$values),]
stacked_MSBP$ind<- factor(stacked_MSBP$ind,
                          levels= c("total_index", "genetic_index", "viability_index","information_index"),
                          labels=c("Overall index", "Genetic Index","Viability Index", "Information Index"))


# Basic horizontal violin plot
vio <- ggplot(stacked_MSBP, aes(x = factor(ind), y = values,
                                fill = factor(ind), color = factor(ind))) +
  geom_violin(width = 1, scale = "width", #draw_quantiles = c(0.25, 0.5, 0.75),
              alpha = 0.8) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  coord_flip() +
  ylab("Index Value") +
  theme_minimal() +
  theme(
    legend.position="none"
  ) +
  # coord_flip() +
  xlab("")

vio
ggsave(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "index.pdf"),
       width = 20, height = 12, units = "cm")

par(mar = c(5,5,3,3))
png(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "index.png"),
    width = 6, height = 3.5, units = "in", res = 300)
vio
dev.off()




##########################################################################################################
#####       PIE CHART       ##############################################################################
##########################################################################################################

# Proportion CR and predicted CR banked, orthodox, intermediate, recalcitrant
spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
spp_banked_recalcitrant$category[which(is.na(spp_banked_recalcitrant$probability.of.recalcitrance))] = "unknown"
spp_banked_recalcitrant$predictions = ifelse(spp_banked_recalcitrant$redlistCriteria == "prediction", "prediction", "IUCN")
spp_banked_recalcitrant$category2 = spp_banked_recalcitrant$category
spp_banked_recalcitrant$category2[which(spp_banked_recalcitrant$category2 =="recalcitrant")] = "exceptional"
spp_banked_recalcitrant$category2 = ifelse(spp_banked_recalcitrant$banked == T, "banked", spp_banked_recalcitrant$category2)
spp_banked_recalcitrant$labels = paste0( spp_banked_recalcitrant$category2, " (", spp_banked_recalcitrant$predictions,")")

single = spp_banked_recalcitrant[which(duplicated(spp_banked_recalcitrant$taxon_name) == F),]
# bank = spp_banked_recalcitrant[spp_banked_recalcitrant$banked == T,]

# Some are replicated but they are species that have bee split
length(unique(spp_banked_recalcitrant$taxon_name)) # 5758
length(spp_banked_recalcitrant$taxon_name) # 5780, was 5773

length(unique(single$taxon_name)) # 5758
length(single$taxon_name) # 5758

pie_data = single %>% count(labels)
pie_data = pie_data[c(5,6,7,8,3,4,1,2),]
sum(pie_data$n)
par(mar = c(0,0,1,10))
pdf(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "piechart_iucn_colourblind.pdf"),
    width = 8, height = 5)
pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n), cex=.75,
    col=c("#FFC107", "goldenrod3",
          "#fe6100", "darkorange4",
          "#D81B60", "#610A1A",
          "#1E88E5", "#0F4777"))
# "#785ef0",
# "#004D40", "#00231D",
#     "darkolivegreen4","darkolivegreen4",
#     "darkolivegreen3","darkolivegreen4",
#     "darkgoldenrod1","darkgoldenrod3",
#     "chocolate1","chocolate3",
#     "brown3","brown4",
# "black", "grey"))
dev.off()


png(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "piechart_iucn_colourblind.png"),
    width = 8, height = 5, units = "in", res = 300)
pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n), cex=.75,
    col=c("#FFC107", "goldenrod3",
          "#fe6100", "darkorange4",
          "#D81B60", "#610A1A",
          "#1E88E5", "#0F4777"))
# pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n), cex=.75,
#     col=c("darkolivegreen3","darkolivegreen4",
#           "darkgoldenrod1","darkgoldenrod3",
#           "chocolate1","chocolate3",
#           "brown3","brown4",
#           "black", "grey"))
dev.off()
par(mar = c(5,5,3,3))

##########################################################################################################
#####       PIE CHART CERTAIN       ######################################################################
##########################################################################################################

# Proportion CR and predicted CR banked, orthodox, intermediate, recalcitrant
spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
spp_banked_recalcitrant$category_certain[which(is.na(spp_banked_recalcitrant$category_certain))] = "unknown"
spp_banked_recalcitrant$predictions = ifelse(spp_banked_recalcitrant$redlistCriteria == "prediction", "prediction", "IUCN")
spp_banked_recalcitrant$category2 = spp_banked_recalcitrant$category_certain
spp_banked_recalcitrant$category2[which(spp_banked_recalcitrant$category2 =="recalcitrant")] = "exceptional"
spp_banked_recalcitrant$category2 = ifelse(spp_banked_recalcitrant$banked == T, "banked", spp_banked_recalcitrant$category2)
spp_banked_recalcitrant$labels = paste0( spp_banked_recalcitrant$category2, " (", spp_banked_recalcitrant$predictions,")")

single = spp_banked_recalcitrant[which(duplicated(spp_banked_recalcitrant$taxon_name) == F),]
# bank = spp_banked_recalcitrant[spp_banked_recalcitrant$banked == T,]

# Some are replicated but they are species that have bee split
length(unique(spp_banked_recalcitrant$taxon_name)) # 5758
length(spp_banked_recalcitrant$taxon_name) # 5780, was 5773

length(unique(single$taxon_name)) # 5758
length(single$taxon_name) # 5758

pie_data = single %>% count(labels)
pie_data = pie_data[c(1,2,7,8,5,6,3,4,9,10),] # c(5,6,7,8,3,4,1,2),]
sum(pie_data$n)
par(mar = c(0,0,1,10))
pdf(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "piechart_iucn_colourblind_certain.pdf"),
    width = 8, height = 5)
pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n), cex=.75,
    col=c("#FFC107", "goldenrod3",
          "#fe6100", "darkorange4",
          "#D81B60", "#610A1A",
          "#1E88E5", "#0F4777",
          "grey40","grey20"))
# "#785ef0",
# "#004D40", "#00231D",
#     "darkolivegreen4","darkolivegreen4",
#     "darkolivegreen3","darkolivegreen4",
#     "darkgoldenrod1","darkgoldenrod3",
#     "chocolate1","chocolate3",
#     "brown3","brown4",
# "black", "grey"))
dev.off()


png(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "piechart_iucn_colourblind_certain.png"),
    width = 8, height = 5, units = "in", res = 300)
pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n), cex=.75,
    col=c("#FFC107", "goldenrod3",
          "#fe6100", "darkorange4",
          "#D81B60", "#610A1A",
          "#1E88E5", "#0F4777",
          "grey40","grey20"))
# pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n), cex=.75,
#     col=c("darkolivegreen3","darkolivegreen4",
#           "darkgoldenrod1","darkgoldenrod3",
#           "chocolate1","chocolate3",
#           "brown3","brown4",
#           "black", "grey"))
dev.off()
par(mar = c(5,5,3,3))

##########################################################################################################
#####       PIE CHART       ##############################################################################
##########################################################################################################

# Proportion CR and predicted CR banked, orthodox, intermediate, recalcitrant
spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
spp_banked_recalcitrant$category_uncertain[which(is.na(spp_banked_recalcitrant$category_uncertain))] = "unknown"
spp_banked_recalcitrant$predictions = ifelse(spp_banked_recalcitrant$redlistCriteria == "prediction", "prediction", "IUCN")
spp_banked_recalcitrant$category2 = spp_banked_recalcitrant$category_uncertain
spp_banked_recalcitrant$category2[which(spp_banked_recalcitrant$category2 =="recalcitrant")] = "exceptional"
spp_banked_recalcitrant$category2 = ifelse(spp_banked_recalcitrant$banked == T, "banked", spp_banked_recalcitrant$category2)
spp_banked_recalcitrant$labels = paste0( spp_banked_recalcitrant$category2, " (", spp_banked_recalcitrant$predictions,")")

single = spp_banked_recalcitrant[which(duplicated(spp_banked_recalcitrant$taxon_name) == F),]
# bank = spp_banked_recalcitrant[spp_banked_recalcitrant$banked == T,]

# Some are replicated but they are species that have bee split
length(unique(spp_banked_recalcitrant$taxon_name)) # 5758
length(spp_banked_recalcitrant$taxon_name) # 5780, was 5773

length(unique(single$taxon_name)) # 5758
length(single$taxon_name) # 5758

pie_data = single %>% count(labels)
pie_data = pie_data[c(1,2,7,8,5,6,3,4,9,10),] # c(5,6,7,8,3,4,1,2),]
sum(pie_data$n)
par(mar = c(0,0,1,10))
pdf(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "piechart_iucn_colourblind_uncertain.pdf"),
    width = 8, height = 5)
pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n), cex=.75,
    col=c("#FFC107", "goldenrod3",
          "#fe6100", "darkorange4",
          "#D81B60", "#610A1A",
          "#1E88E5", "#0F4777",
          "grey40","grey20"))
dev.off()


png(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "piechart_iucn_colourblind_uncertain.png"),
    width = 8, height = 5, units = "in", res = 300)
pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n), cex=.75,
    col=c("#FFC107", "goldenrod3",
          "#fe6100", "darkorange4",
          "#D81B60", "#610A1A",
          "#1E88E5", "#0F4777",
          "grey40","grey20"))
dev.off()
par(mar = c(5,5,3,3))





# #################################################
# #       bankable vs banked
# #################################################
#
# spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
# spp_banked_recalcitrant$category[which(is.na(spp_banked_recalcitrant$category))] = "unknown"
# spp_banked_recalcitrant$predictions = ifelse(spp_banked_recalcitrant$redlistCriteria == "prediction", "prediction", "IUCN")
#
# bankable = spp_banked_recalcitrant[spp_banked_recalcitrant$category %in% c("orthodox","banked","unknown"), ]
# bankable$labels = paste(bankable$category, bankable$predictions)
#
# # bank = spp_banked_recalcitrant[spp_banked_recalcitrant$banked == T,]
# # predicted = spp_banked_recalcitrant[spp_banked_recalcitrant$redlistCriteria == "predicted",]
#
# # Some are replicated but they are species that have bee split
# # length(unique(spp_banked_recalcitrant$taxon_name)) # 5707
# # length(spp_banked_recalcitrant$taxon_name) # 5717
#
#
# pie_data = bankable %>% count(labels)
# par(mar = c(0,0,1,4))
# pdf(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "piechart_bankable.pdf"),
#     width = 8, height = 5)
# pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n) , cex=.75,
#     col=c("darkolivegreen3","darkolivegreen4",
#           "darkgoldenrod1","darkgoldenrod3", "grey"))
# dev.off()
# png(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "piechart_bankable.png"),
#     width = 8, height = 5, units = "in", res = 300)
# pie(pie_data$n, paste(pie_data$labels,"n =",pie_data$n) , cex=.75,
#     col=c("darkolivegreen3","darkolivegreen4",
#           "darkgoldenrod1","darkgoldenrod3", "grey"))
# dev.off()
# par(mar = c(5,5,3,3))



