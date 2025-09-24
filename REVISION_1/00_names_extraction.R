library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"
wcvp <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

# get spp list for Naomi:
iucn_predictions <- read.csv(paste0(basepath,"revision_1/iucn_predictions_wcvp_matched.csv"))
iucn <- read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"))

# colnames(iucn)
# View(iucn[,c("scientificName", "wcvp_name", "taxon_name", "wcvp_status")])
wcvp$plant_name_id <- as.character(wcvp$plant_name_id)
iucn_predictions$wcvp_accepted_id <- as.character(iucn_predictions$wcvp_accepted_id)

iucn_predictions <- iucn_predictions[which(iucn_predictions$category == "CR"),]
iucn_predictions <- iucn_predictions %>% left_join(wcvp[,c("plant_name_id", "ipni_id" )],
                                                   by=c("wcvp_accepted_id" = "plant_name_id"))

wcvp$plant_name_id <- as.character(wcvp$plant_name_id)
iucn <- iucn %>% left_join(wcvp[,c("plant_name_id", "ipni_id" )],
                           by=c("wcvp_accepted_id" = "plant_name_id"))


all_names <- rbind(iucn[,c("taxon_name", "taxon_authors","ipni_id" )],
                   iucn_predictions[,c("taxon_name","taxon_authors", "ipni_id" )])


all_names <- all_names[which(!duplicated(all_names)),]
write.csv(all_names, paste0(basepath,"revision_1/CR_list.csv"), row.names = F)
