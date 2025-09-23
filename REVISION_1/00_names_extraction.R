# get spp list for Naomi:
iucn_predictions <- read.csv(paste0(basepath,"revision_1/iucn_predictions_wcvp_matched.csv"))
iucn <- read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"))

# colnames(iucn)
# View(iucn[,c("scientificName", "wcvp_name", "taxon_name", "wcvp_status")])
wcvp$plant_name_id <- as.character(wcvp$plant_name_id)
iucn_predictions$wcvp_accepted_id <- as.character(iucn_predictions$wcvp_accepted_id)

iucn_predictions <- iucn_predictions[which(iucn_predictions$category == "CR"),]
iucn_predictions <- iucn_predictions %>% left_join(wcvp[,c("plant_name_id", "taxon_authors")],
                                                   by=c("wcvp_accepted_id" = "plant_name_id"))

wcvp$plant_name_id <- as.character(wcvp$plant_name_id)
iucn <- iucn %>% left_join(wcvp[,c("plant_name_id", "taxon_authors")],
                           by=c("wcvp_accepted_id" = "plant_name_id"))


all_names <- rbind(iucn[,c("taxon_name","taxon_authors")],
                   iucn_predictions[,c("taxon_name","taxon_authors")])


all_names <- all_names[which(!duplicated(all_names)),]
write.csv(all_names, paste0(basepath,"revision_1/IUCN_seedinfo_list.csv"), row.names = F)
