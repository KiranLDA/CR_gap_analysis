spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))


# the current problem is that the predictions are replicated.

length(unique(spp_banked_recalcitrant$taxon_name)) # 6136
length(spp_banked_recalcitrant$taxon_name) # 5705


# find duplicated names that don't have any accepted name
dupl = spp_banked_recalcitrant$taxon_name %in% unique(spp_banked_recalcitrant$taxon_name[ duplicated(spp_banked_recalcitrant$taxon_name)])
dupl_nam = unique(spp_banked_recalcitrant$taxon_name[dupl])

duplicated = spp_banked_recalcitrant[spp_banked_recalcitrant$taxon_name %in% dupl_nam,]
duplicated = duplicated[order(duplicated$taxon_name),]
