library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################
wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

#load data from previous session
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))

brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))

# make sure only consider predicted that aren't already CR
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched_full_name.csv"))
brahms_unique_wcvp_matched = brahms_unique_wcvp_matched[which(!is.na(brahms_unique_wcvp_matched$species)),]

# brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched.csv"))
# brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched.csv"))
# exceptional_wcvp_matched = read.csv(paste0(basepath,"exceptional_wcvp_matched.csv"))
exceptional <- read.csv(paste0(basepath, "pence_appendix1.csv"))
exceptional_wcvp_matched = read.csv(paste0(basepath,"exceptional_unique_wcvp_matched.csv"))

iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "iucn_predictions_wcvp_matched.csv"))

iucn_CR_predictions = iucn_predictions[which(iucn_predictions$category == "CR"),]
# keep only the CR ones
iucn_CR_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]
# # keep only the ones that aren't already in IUCN
# iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in% iucn_wcvp_matched$taxon_name)),]
iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(ifelse(iucn_CR_predictions_wcvp_matched$taxon_name %in%
                                                                                   iucn_wcvp_matched$taxon_name, FALSE,TRUE)),]
length(unique(iucn_CR_predictions_wcvp_matched$taxon_name))



# # load brahms data
# brahms <- read.csv(paste0(basepath,"2024-03-21_164953044-BRAHMSOnlineData.csv"))
#
# # remove duplicates
# brahms <- brahms[duplicated(brahms$AccessionNumber)==FALSE,] # removes 441 duplicates
#
# # extract species
# brahms$species <- gsub("^(\\S+ \\S+).*", "\\1", brahms$Taxon) #gsub("^(\\w+ \\w+).*", "\\1", brahms$Taxon)
# brahms$species <- trimws(brahms$species)
#
# # extract subspecies
# subspecies_match <- regexpr("subsp\\. \\w+", brahms$Taxon)
# brahms$subspecies <- substring(brahms$Taxon, subspecies_match,
#                                subspecies_match + attr(subspecies_match, "match.length") - 1)
#
# # extract variety
# variety_match <- regexpr("var\\. \\w+", brahms$Taxon)
# brahms$var <- substring(brahms$Taxon, variety_match,
#                         variety_match + attr(variety_match, "match.length") - 1)
#
# # now save the rest of the string as the author name
# # Remove species and subspecies information to get author
# brahms$author <- gsub("^(\\S+ \\S+)", "", brahms$Taxon) # gsub("^(\\w+ \\w+)", "", brahms$Taxon)  # Remove species
# brahms$author <- gsub("subsp\\. \\w+", "", brahms$author)      # Remove subspecies
# brahms$author <- gsub("var\\. \\w+", "", brahms$author)
# brahms$author <- trimws(brahms$author)
# # brahms$subspecies_name = trimws(paste(brahms$species,brahms$subspecies))
#
# # edit code to include var and other if needed
# brahms$full_name = trimws(paste(brahms$species,brahms$subspecies,brahms$var))
# brahms$full_name = gsub("\\s+"," ",brahms$full_name)


#### GET ORTHODOXY ########################################################################################

# prepare the IUCN species and get their orthodoxy
# orthodox_matches = data.frame(unique(iucn_wcvp_matched$taxon_name[iucn_wcvp_matched$banked == F]))
# dim(orthodox_matches)
# j=1
# for(i in seq(1,nrow(orthodox_matches),873)){
#   write.csv(orthodox_matches[i:(i+872),],paste0(basepath, "orthodox_to_match_",j,".csv"), row.names=FALSE )
#   j=j+1
# }

# #run the shiny seed predictor app
# runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)

# Compile orthodoxy predictions
iucn_orthodox = read.csv(paste0(basepath,"Model-results-2024-08-08_1.csv"))
for (i in 2:6){
  iucn_orthodox = rbind(iucn_orthodox,
                        read.csv(paste0(basepath,"Model-results-2024-08-08_",i,".csv")))
}

#get rid of duplicates
iucn_orthodox = iucn_orthodox[which(duplicated(iucn_orthodox$Accepted_name)==FALSE),]

# combine the two datasets
iucn_wcvp_matched_orthodox = iucn_wcvp_matched %>% left_join(iucn_orthodox[,c("Accepted_name",
                                                                              "tax.level",
                                                                              "probability.of.recalcitrance",
                                                                              "storBehav")],
                                                             by = c("taxon_name" = "Accepted_name") )
length(which(is.na(iucn_wcvp_matched_orthodox$storBehav)))





# keep unique values
iucn_wcvp_matched_orthodox = unique(iucn_wcvp_matched_orthodox)

# banked
iucn_wcvp_matched_orthodox$banked = ifelse(iucn_wcvp_matched_orthodox$taxon_name %in% brahms_unique_wcvp_matched$taxon_name,TRUE,FALSE)

# specify the categories
iucn_wcvp_matched_orthodox$category = NA
# iucn_wcvp_matched_orthodox$category[iucn_wcvp_matched_orthodox$banked == T] = "banked"
iucn_wcvp_matched_orthodox$category[which(iucn_wcvp_matched_orthodox$probability.of.recalcitrance <= 0.3)] = "orthodox"
iucn_wcvp_matched_orthodox$category[which(iucn_wcvp_matched_orthodox$probability.of.recalcitrance >= 0.7)] = "recalcitrant"
iucn_wcvp_matched_orthodox$category[which(iucn_wcvp_matched_orthodox$probability.of.recalcitrance < 0.7 &
                                      iucn_wcvp_matched_orthodox$probability.of.recalcitrance > 0.3)] = "unknown"

length(exceptional_wcvp_matched$taxon_name)
length(unique(exceptional_wcvp_matched$taxon_name))

# remove duplicates
iucn_storage_behaviour = iucn_wcvp_matched_orthodox %>% left_join(exceptional_wcvp_matched[,c("taxon_name", "Exceptional_status",
                                                                                              "EF1_seed_unavailable","EF2_desiccation_sensitive",
                                                                                              "EF3_short.lived", "EF4_deep_dormancy", "SID_Seed_Storage_Behaviour",
                                                                                              "Woody_or_non.woody", "PlantSearch_plant_collections", "PlantSearch_seed_collections",
                                                                                              "PlantSearch_tissue_culture_collections", "PlantSearch_cryopreservation_collections",
                                                                                              "PlantSearch_uncategorized_germplasm_collections")],
                                                                  by="taxon_name", relationship = "many-to-one")

iucn_storage_behaviour = unique(iucn_storage_behaviour)

iucn_storage_behaviour[iucn_storage_behaviour$category == "unknown",]
iucn_storage_behaviour[which(iucn_storage_behaviour$PlantSearch_cryopreservation_collections == 1),]
iucn_storage_behaviour[which(iucn_storage_behaviour$PlantSearch_seed_collections >0),]

iucn_storage_behaviour$category[which(iucn_storage_behaviour$Exceptional_status == "Exceptional")] = "exceptional"
# View(iucn_storage_behaviour[is.na(iucn_storage_behaviour$category),])

iucn_storage_behaviour$category[which((iucn_storage_behaviour$SID_Seed_Storage_Behaviour == "Orthodox") &
                                        is.na(iucn_storage_behaviour$category))] = "orthodox"

iucn_storage_behaviour$category[which((iucn_storage_behaviour$SID_Seed_Storage_Behaviour == "Orthodox p") &
                                        is.na(iucn_storage_behaviour$category))] = "orthodox"

iucn_storage_behaviour$category[which((iucn_storage_behaviour$SID_Seed_Storage_Behaviour == "Recalcitrant") &
                                        is.na(iucn_storage_behaviour$category))] = "recalcitrant"

iucn_storage_behaviour$category[which(iucn_storage_behaviour$EF2_desiccation_sensitive == "Yes")] = "recalcitrant"

iucn_storage_behaviour$category[which((iucn_storage_behaviour$SID_Seed_Storage_Behaviour == "Intermediate") &
                                        is.na(iucn_storage_behaviour$category))] = "intermediate"


# # # Remaining species
# # remaining = unique(iucn_storage_behaviour$taxon_name[
# #   which(is.na(iucn_storage_behaviour$category))])
# # # remaining_2 = read.csv(paste0(basepath, "remaining_to_match.csv"))
# # write.csv(remaining,paste0(basepath, "remaining_to_match.csv"), row.names=FALSE )
# #
# # #run the shiny seed predictor app
# # runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)
#
# # iucn_storage_behaviour$category[iucn_storage_behaviour$EF1_seed_unavailable == "Yes"] = "EF1_seed_unavailable"
# # iucn_storage_behaviour$category[which(iucn_storage_behaviour$EF2_desiccation_sensitive == "Yes")] = "EF2_desiccation_sensitive"
# # iucn_storage_behaviour$category[which(iucn_storage_behaviour$EF3_short.lived == "Yes")] = "EF3_short_lived"
# # iucn_storage_behaviour$category[which(iucn_storage_behaviour$EF4_deep_dormancy == "Yes")] = "EF2_desiccation_sensitive"
# # iucn_storage_behaviour$taxon_name[which(iucn_storage_behaviour$EF3_short.lived == "Yes")]
#
# orthodox_remaining = read.csv(paste0(basepath,"Model-results-2024-04-08_remaining.csv"))
#
# # remove duplicates
# orthodox_remaining = orthodox_remaining[duplicated(orthodox_remaining$Initial_List) == F, ]
# # length(unique(orthodox_remaining$Initial_List))
# # length(orthodox_remaining$Initial_List)
#
# # which(wcvp$taxon_name == "Podocarpus decumbens")
# colnames(orthodox_remaining) = c("Initial_List", "wcvp_name.y","wcvp_authors.y","wcvp_id.y" ,"wcvp_status.y",
#                                  "Accepted_wcvp_name_id","Accepted_name","Accepted_wcvp_name_authors",
#                                  "Accepted_name.1","Genus","Family","Order","tax.level",
#                                  "probability.of.recalcitrance","storBehav")
#
# NA_storage = iucn_storage_behaviour[which(is.na(iucn_storage_behaviour$category)),c(1:45,60:72)]
#
# NA_storage = NA_storage %>% left_join(orthodox_remaining,
#                                       by=c("taxon_name" = "Accepted_name"))
#
# #reorder the column names
# NA_storage = NA_storage[, colnames(iucn_storage_behaviour)]
#
# #add in the categories
# NA_storage$category[NA_storage$banked == T] = "banked"
# NA_storage$category[NA_storage$probability.of.recalcitrance <= 0.3] = "orthodox"
# NA_storage$category[NA_storage$probability.of.recalcitrance >= 0.7] = "recalcitrant"
# NA_storage$category[NA_storage$probability.of.recalcitrance < 0.7 & NA_storage$probability.of.recalcitrance > 0.3] = "intermediate"
#
# #remove the synonym row
# # NA_storage = unique(NA_storage)
# # id = NA_storage$X.1[duplicated(NA_storage$X.1)]
# # NA_storage = NA_storage[-which(NA_storage$X.1 == id)[which(NA_storage$wcvp_status.y[NA_storage$X.1 == id] == "Synonym")],]
#
# iucn_storage_behaviour[which(is.na(iucn_storage_behaviour$category)),] = NA_storage

# save the output
write.csv(iucn_storage_behaviour, paste0(basepath, "iucn_orthodocy_recalcitrance.csv"))
iucn_storage_behaviour = read.csv(paste0(basepath, "iucn_orthodocy_recalcitrance.csv"))
# length(iucn_storage_behaviour$taxon_name)
length(unique(iucn_storage_behaviour$taxon_name))
# iucn_storage_behaviour$taxon_name[iucn_storage_behaviour$taxon_name %in% iucn_CR_predictions_wcvp_matched$taxon_name]



seed_count = brahms_unique_wcvp_matched[,c("taxon_name","wcvp_authors", "wcvp_accepted_id", "accessions","summed_count")]

# create summary table where adjusted seed counts per species are added together
seed_count = seed_count %>%
  group_by(taxon_name, wcvp_authors, wcvp_accepted_id, accessions, summed_count) %>%
  tally() %>%
  mutate(summed_count_2 = sum(summed_count),
         accessions_2 = sum(accessions)) %>%
  ungroup()
seed_count = seed_count[, c("taxon_name","wcvp_authors", "wcvp_accepted_id","accessions_2", "summed_count_2")]
seed_count = seed_count[duplicated(seed_count$taxon_name)==FALSE,]
colnames(seed_count) = c("taxon_name","wcvp_authors", "wcvp_accepted_id", "accessions","summed_count")

# add the accession data as will be useful later
iucn_storage_behaviour = iucn_storage_behaviour %>% left_join(seed_count[,c("wcvp_accepted_id","accessions","summed_count")],
                                                              # by = "taxon_name",
                                                              by = "wcvp_accepted_id",
                                                              relationship = "many-to-one")





# reduce dataset size
colkeep = c("taxon_name","scientificName", "family", "genus","higher",
            "order","redlistCategory",
            "redlistCriteria", "yearPublished", "assessmentDate", "criteriaVersion",
            "populationTrend","systems", "realm","yearLastSeen","possiblyExtinct",
            "possiblyExtinctInTheWild", "scopes","banked",
            #"Genus", "Family", "Order",
            "tax.level","probability.of.recalcitrance","storBehav",
            "category","Exceptional_status","EF1_seed_unavailable",
            "EF2_desiccation_sensitive","EF3_short.lived","EF4_deep_dormancy",
            "SID_Seed_Storage_Behaviour","Woody_or_non.woody",
            "PlantSearch_plant_collections", "PlantSearch_seed_collections",
            "PlantSearch_tissue_culture_collections",
            "PlantSearch_cryopreservation_collections",
            "PlantSearch_uncategorized_germplasm_collections",
            "accessions","summed_count")

iucn_storage_behaviour = iucn_storage_behaviour[,colkeep]

length(iucn_storage_behaviour$taxon_name)
dupli = iucn_storage_behaviour$taxon_name[which(duplicated(iucn_storage_behaviour$taxon_name))]
View(iucn_storage_behaviour[which(iucn_storage_behaviour$taxon_name %in% dupli),])

# more names in IUCN than WCVP
length(unique(iucn_storage_behaviour$taxon_name))




########################################################################################################################
############## PREDICTED CR to be added to the dataset #################################################################
########################################################################################################################

# Add the IUCN predictions
# iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
# # keep only the CR ones
# CR_pred = iucn_predictions[which(iucn_predictions$category == "CR"),]
# # # remove those that are already in the databsae
# CR_pred = CR_pred[which(!(CR_pred$taxon_name %in% iucn_wcvp_matched$taxon_name)),]
# dim(CR_pred[which(!(CR_pred$ipni_id %in% iucn_wcvp_matched$wcvp_ipni_id)),])
CR_pred = iucn_CR_predictions_wcvp_matched
length(unique(CR_pred$taxon_name)) #104
iucn_storage_behaviour$taxon_name[iucn_storage_behaviour$taxon_name %in% CR_pred$taxon_name]
# CR_pred_to_add$taxon_name[!(CR_pred_to_add$taxon_name %in% CR_pred$taxon_name)]
# iucn_storage_behaviour$taxon_name[iucn_storage_behaviour$taxon_name %in% CR_pred_to_add$taxon_name]

########### See if the CR are in the bank already ######################################################################

# CR_pred_in_bank = CR_pred$taxon_name[unique(CR_pred$taxon_name) %in% unique(brahms_wcvp_matched$taxon_name)]
# Subset the msbp data

spp_count = brahms_wcvp_matched %>%
  group_by(taxon_name,wcvp_accepted_id,AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(summed_count = sum(AdjustedSeedQuantity),
         accessions = n()) %>%
  ungroup()
spp_count = spp_count[, c("taxon_name","wcvp_accepted_id", "accessions", "summed_count")]
spp_count = spp_count[duplicated(spp_count$wcvp_accepted_id)==FALSE,]

brahms_to_add = spp_count[spp_count$taxon_name %in% unique(CR_pred$taxon_name),]

# banked_CRpred_orthodocy_recalcitrance.csv
# #run the shiny seed predictor app
# runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)

recalc <- read.csv(paste0(basepath, "Model-results-2024-08-06_1_banked_crpred.csv"))

# brahms_to_add = seed_count[seed_count$taxon_name %in% unique(CR_pred$taxon_name),]
#brahms_wcvp_matched[brahms_wcvp_matched$taxon_name %in% unique(CR_pred$taxon_name),]

# # create summary table where adjusted seed counts per species are added together
# brahms_to_add = brahms_to_add %>%
#   group_by(taxon_name,wcvp_authors,wcvp_accepted_id,AdjustedSeedQuantity) %>%
#   tally() %>%
#   mutate(summed_count = sum(AdjustedSeedQuantity),
#          accessions = n()) %>%
#   ungroup()
# brahms_to_add = brahms_to_add[, c("taxon_name","wcvp_authors", "wcvp_accepted_id","accessions", "summed_count")]
# brahms_to_add = brahms_to_add[duplicated(brahms_to_add$taxon_name)==FALSE,]


# create an empty dataset that follows iucn_storage_behaviour and fill with NAs
brahms_to_add = data.frame(brahms_to_add)
brahms_to_add = brahms_to_add %>% left_join(recalc[,c("Accepted_name",
                                                      "tax.level",
                                                      "probability.of.recalcitrance",
                                                      "storBehav")],
                                            by = c("taxon_name" = "Accepted_name") )
# brahms_to_add$wcvp_accepted_id = as.integer(brahms_to_add$wcvp_accepted_id)
brahms_to_add = brahms_to_add %>% left_join(wcvp[,c("plant_name_id","family","genus",
                                                    "climate_description", "geographic_area")],
                                            by=c("wcvp_accepted_id"="plant_name_id"))

# Fill in the CR_pred data
brahms_to_add["scientificName"] = NA
brahms_to_add["redlistCategory"] = "Critically Endangered"
brahms_to_add["redlistCriteria"] = "prediction"
brahms_to_add["yearPublished"] = 2024
brahms_to_add["assessmentDate"] = NA
brahms_to_add["criteriaVersion"] = NA
brahms_to_add["populationTrend"] = NA
brahms_to_add["systems"] = NA
brahms_to_add["realm"] = brahms_to_add["climate_description"]
brahms_to_add["yearLastSeen"] = ""
brahms_to_add["possiblyExtinct"] = NA
brahms_to_add["possiblyExtinctInTheWild"] = NA
brahms_to_add["scopes"] = brahms_to_add["geographic_area"]
brahms_to_add["banked"] = TRUE
# brahms_to_add["order"] = NA
# brahms_to_add["higher"] = NA
brahms_to_add = brahms_to_add %>% left_join(rWCVP::taxonomic_mapping,
          by=c("family" = "family"))
# brahms_to_add["tax.level"] = NA
# brahms_to_add["probability.of.recalcitrance"] = NA
# brahms_to_add["storBehav"] = NA
# brahms_to_add["category"] = "banked"
brahms_to_add$category = NA
brahms_to_add$category[which(brahms_to_add$probability.of.recalcitrance <= 0.3)] = "orthodox"
brahms_to_add$category[which(brahms_to_add$probability.of.recalcitrance >= 0.7)] = "recalcitrant"
brahms_to_add$category[which(brahms_to_add$probability.of.recalcitrance < 0.7 & brahms_to_add$probability.of.recalcitrance > 0.3)] = "unknown"
brahms_to_add["Exceptional_status"] = NA
brahms_to_add["EF1_seed_unavailable"] = NA
brahms_to_add["EF2_desiccation_sensitive"] = NA
brahms_to_add["EF3_short.lived"] = NA
brahms_to_add["EF4_deep_dormancy"] = NA
brahms_to_add["SID_Seed_Storage_Behaviour"] = NA
brahms_to_add["Woody_or_non.woody"] = NA
brahms_to_add["PlantSearch_plant_collections"] = NA
brahms_to_add["PlantSearch_seed_collections"] = NA
brahms_to_add["PlantSearch_tissue_culture_collections"] = NA
brahms_to_add["PlantSearch_cryopreservation_collections"] = NA
brahms_to_add["PlantSearch_uncategorized_germplasm_collections"] = NA

#only keep the columns that match IUCN dataframe
brahms_to_add = brahms_to_add[,colkeep]

# basically only one species in the bank
length(unique(brahms_to_add$taxon_name))

###################################################################################################
####  GET CR SPECIES THAT AREN'T BANKED AND ADD ORTHODOXY #########################################

# # remove species predicted CR that already have a CR prediction from IUCN
# CR_pred_to_add = CR_pred$taxon_name[which(!(CR_pred$taxon_name %in% brahms_to_add$taxon_name))] #unique(CR_pred$taxon_name)[!(unique(CR_pred$taxon_name) %in% iucn_storage_behaviour$taxon_name)]
# #unique(CR_pred$taxon_name)[which(!(unique(CR_pred$taxon_name) %in% brahms_to_add$taxon_name))] #unique(CR_pred$taxon_name)[!(unique(CR_pred$taxon_name) %in% iucn_storage_behaviour$taxon_name)]
# write.csv(data.frame(CR_pred_to_add), paste0(basepath,"CR_predictions_for_orthodoxy_predictions.csv"), row.names=FALSE)

#run the shiny seed predictor app
# runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)

# get the orthodoxy predictions
# CR_pred_to_add = read.csv(paste0(basepath,"Model-results-2024-05-27.csv"))
CR_pred_to_add = read.csv(paste0(basepath,"Model-results-2024-08-06.csv"))
#remove duplicates
CR_pred_to_add = CR_pred_to_add[duplicated(CR_pred_to_add$Initial_List)==FALSE,]
CR_pred_to_add = CR_pred_to_add %>% left_join(wcvp[,c("taxon_name", # "plant_name_id",
                                                      "family","genus",
                                                      "climate_description", "geographic_area")],
                                              by = c("Initial_List" = "taxon_name"))
                                              #by=c("Accepted_wcvp_name_id"="plant_name_id"))

CR_pred_to_add["taxon_name"] = CR_pred_to_add["Initial_List"]
CR_pred_to_add["scientificName"] = NA
CR_pred_to_add["redlistCategory"] = "Critically Endangered"
CR_pred_to_add["redlistCriteria"] = "prediction"
CR_pred_to_add["yearPublished"] = 2024
CR_pred_to_add["assessmentDate"] = NA
CR_pred_to_add["criteriaVersion"] = NA
CR_pred_to_add["populationTrend"] = NA
CR_pred_to_add["systems"] = NA
CR_pred_to_add["realm"] = CR_pred_to_add["climate_description"]
CR_pred_to_add["yearLastSeen"] = ""
CR_pred_to_add["possiblyExtinct"] = NA
CR_pred_to_add["possiblyExtinctInTheWild"] = NA
CR_pred_to_add["scopes"] = CR_pred_to_add["geographic_area"]
CR_pred_to_add["banked"] = FALSE

CR_pred_to_add = CR_pred_to_add %>% left_join(rWCVP::taxonomic_mapping,
                                            by=c("family" = "family"))
CR_pred_to_add["category"] = NA
# CR_pred_to_add$category[CR_pred_to_add$banked == T] = "banked"
CR_pred_to_add$category[which(CR_pred_to_add$probability.of.recalcitrance <= 0.3)] = "orthodox"
CR_pred_to_add$category[which(CR_pred_to_add$probability.of.recalcitrance >= 0.7)] = "recalcitrant"
CR_pred_to_add$category[which(CR_pred_to_add$probability.of.recalcitrance < 0.7 & CR_pred_to_add$probability.of.recalcitrance > 0.3)] = "unknown"



CR_pred_to_add = CR_pred_to_add %>% left_join(exceptional_wcvp_matched[,c("taxon_name", "Exceptional_status",
                                                                          "EF1_seed_unavailable","EF2_desiccation_sensitive",
                                                                          "EF3_short.lived", "EF4_deep_dormancy", "SID_Seed_Storage_Behaviour",
                                                                          "Woody_or_non.woody", "PlantSearch_plant_collections", "PlantSearch_seed_collections",
                                                                          "PlantSearch_tissue_culture_collections", "PlantSearch_cryopreservation_collections",
                                                                          "PlantSearch_uncategorized_germplasm_collections")],
                                              by=c("taxon_name" = "taxon_name"))

CR_pred_to_add["accessions"] =  NA
CR_pred_to_add["summed_count"] =  NA

# only keep the columns that match IUCN dataframe
CR_pred_to_add = CR_pred_to_add[,colkeep]


length(unique(CR_pred_to_add$taxon_name))
iucn_storage_behaviour$taxon_name[iucn_storage_behaviour$taxon_name %in% CR_pred_to_add$taxon_name]







###### COMBINE DATA    #############################################################################################
#combine the datasets
spp_banked_recalcitrant = rbind(iucn_storage_behaviour, # banked and unbaked IUCN
                                brahms_to_add, # banked IUCN prediction
                                CR_pred_to_add) # unbanked IUCN prediction

CR_pred_to_add$taxon_name[CR_pred_to_add$taxon_name %in% iucn_storage_behaviour$taxon_name]

length(unique(iucn_storage_behaviour$taxon_name))
length(unique(brahms_to_add$taxon_name))
length(unique(CR_pred_to_add$taxon_name))
sum(length(unique(iucn_storage_behaviour$taxon_name)),
    length(unique(brahms_to_add$taxon_name)),
    length(unique(CR_pred_to_add$taxon_name)))
length(unique(spp_banked_recalcitrant$taxon_name))

# check out the duplicates
length(spp_banked_recalcitrant$taxon_name)
length(unique(spp_banked_recalcitrant$taxon_name))

test = spp_banked_recalcitrant
# test = test[which(!is.na(test$taxon_name)),]

# find duplicated names that don't have any accepted name
dupl = test$taxon_name %in% unique(test$taxon_name[ duplicated(test$taxon_name)])
dupl_nam = unique(test$taxon_name[dupl])
# du = dupl_nam[82]
dupl_nam = dupl_nam[order(dupl_nam)]
# test$keep = 0
# test$keep[which(!(test$taxon_name %in% dupl_nam))] = 1
#
# temp = data.frame(test[test$taxon_name %in% dupl_nam,])
# temp[order(temp$taxon_name ),]
# last = c()
# for (du in dupl_nam){
#   temp = data.frame(test[test$taxon_name == du,])
#   ### TO DO: remove predictions
#   if (any(temp$redlistCriteria == "prediction")){
#     test$keep[which(test$taxon_name == du)[which(temp$redlistCriteria != "prediction")]] = 1
#   } else {
#     test$keep[which(test$taxon_name == du)] = 1
#   }
# }

# test = test[test$keep == 1,]
# length(unique(test$taxon_name))
spp_banked_recalcitrant = spp_banked_recalcitrant[which(!is.na(spp_banked_recalcitrant$taxon_name)),]

####### add in the extra species categories from Dani and Hawaii ######################################
extras <- read.csv(paste0(basepath, "extras_wcvp_matched_full_name.csv"))

#combine the extra with the other data
test = spp_banked_recalcitrant %>% left_join(extras[c("taxon_name","species_name_IUCN","Storage","source")],
                                             by="taxon_name")

# # replace storage behaviour with the extras values
# test$category[which(test$category == "unknown"
#                     & !is.na(test$Storage))] = test$Storage[which(test$category == "unknown"
#                                                                   & !is.na(test$Storage))]
# create empty variable for the storage behaviour
test$category_uncertain = NA
test$category_certain = NA
test$category_uncertain_ref = NA
test$category_certain_ref = NA


# step 1: SID
# unique(test$SID_Seed_Storage_Behaviour)
# c("Orthodox","Recalcitrant?","Orthodox p","Recalcitrant","Uncertain","Intermediate","Orthodox?","Intermediate?")

id = which(is.na(test$category_uncertain) & test$SID_Seed_Storage_Behaviour %in% c("Orthodox"))
test$category_uncertain[id] = "orthodox"
test$category_uncertain_ref[id] = "SID"

id = which(is.na(test$category_certain) & test$SID_Seed_Storage_Behaviour %in% c("Orthodox"))
test$category_certain[id] = "orthodox"
test$category_certain_ref[id] = "SID"

id = which(is.na(test$category_uncertain) & test$SID_Seed_Storage_Behaviour %in% c("Recalcitrant"))
test$category_uncertain[id] = "recalcitrant"
test$category_uncertain_ref[id] = "SID"

id = which(is.na(test$category_certain) & test$SID_Seed_Storage_Behaviour %in% c("Recalcitrant"))
test$category_certain[id] = "recalcitrant"
test$category_certain_ref[id] = "SID"

id = which(is.na(test$category_uncertain) & test$SID_Seed_Storage_Behaviour %in% c("Intermediate"))
test$category_uncertain[id] = "intermediate"
test$category_uncertain_ref[id] = "SID"

id = which(is.na(test$category_certain) & test$Exceptional_status %in% c("Intermediate"))
test$category_certain[id] = "intermediate"
test$category_certain_ref[id] = "SID"


#Step 2: add the litterature review
id = which(is.na(test$category_uncertain) & !is.na(test$Storage))
test$category_uncertain[id] = test$Storage[id]
test$category_certain[id] = test$Storage[id]
test$category_uncertain_ref[id] = test$source[id]
test$category_certain_ref[id] = test$source[id]
test$category_uncertain_ref[which(test$category_uncertain_ref == "")] = "expert (Ballesteros)"
test$category_certain_ref[which(test$category_certain_ref == "")] = "expert (Ballesteros)"


# step 3: add in Pence
id = which(is.na(test$category_uncertain) & test$Exceptional_status == "Exceptional")
test$category_uncertain[id] = "exceptional"
test$category_certain[id] = "exceptional"
test$category_uncertain_ref[id] = "Pence et al. 2022"
test$category_certain_ref[id] = "Pence et al. 2022"


#step 4: add in seed storage predictor

#orthodox uncertain
id = which(is.na(test$category_uncertain) & test$probability.of.recalcitrance <= 0.3)
test$category_uncertain[id] = "orthodox"
test$category_uncertain_ref[id] = "Wyse and Dickie (2017)"

# recalcitrant uncertain
id = which(is.na(test$category_uncertain) & test$probability.of.recalcitrance >= 0.7)
test$category_uncertain[id] = "recalcitrant"
test$category_uncertain_ref[id] = "Wyse and Dickie (2017)"


# orthodox certain
id = which(is.na(test$category_certain) & test$probability.of.recalcitrance <= 0.3 & test$tax.level %in% c("Species", "Genus"))
test$category_certain[id] = "orthodox"
test$category_certain_ref[id] = "Wyse and Dickie (2017)"

# recalcitrant certain
id = which(is.na(test$category_certain) & test$probability.of.recalcitrance >= 0.7 & test$tax.level %in% c("Species", "Genus"))
test$category_certain[id] = "recalcitrant"
test$category_certain_ref[id] = "Wyse and Dickie (2017)"


# Step 5: add in the uncertain SID
# c("Orthodox","Recalcitrant?","Orthodox p","Recalcitrant","Uncertain","Intermediate","Orthodox?","Intermediate?")

id = which(is.na(test$category_uncertain) & test$SID_Seed_Storage_Behaviour %in% c("Orthodox?","Orthodox p"))
test$category_uncertain[id] = "orthodox"
test$category_uncertain_ref[id] = "SID"

id = which(is.na(test$category_uncertain) & test$SID_Seed_Storage_Behaviour %in% c("Recalcitrant?"))
test$category_uncertain[id] = "recalcitrant"
test$category_uncertain_ref[id] = "SID"

id = which(is.na(test$category_uncertain) & test$SID_Seed_Storage_Behaviour %in% c("Intermediate?"))
test$category_uncertain[id] = "intermediate"
test$category_uncertain_ref[id] = "SID"

# Step 6: add in orthodox from Pence
id = which(is.na(test$category_uncertain) & test$Exceptional_status %in% c("Probably non-exceptional","Non-exceptional" ))
test$category_uncertain[id] = "orthodox"
test$category_uncertain_ref[id] = "SID"

id = which(is.na(test$category_certain) & test$Exceptional_status %in% c("Non-exceptional" ))
test$category_certain[id] = "orthodox"
test$category_certain_ref[id] = "SID"




# have a look
View(test[, c("storBehav","probability.of.recalcitrance","SID_Seed_Storage_Behaviour",
         "Exceptional_status","Storage",
         "category_uncertain", "category_certain")])



##### SAVE ################################################

write.csv(spp_banked_recalcitrant, paste0(basepath, "spp_banked_recalcitrant.csv"), row.names=FALSE )

###################################################################################################
####                                                         ######################################
####      MAKE SURE SPECIES IN THE BANK ARE ORTHODOX         ######################################
####                                                         ######################################
###################################################################################################

#
# # # remove species predicted CR that already have a CR prediction from IUCN
# # site_counts = read.csv(paste0(basepath,"iucn_brahms_wcvp_matched_full_name.csv"))#read.csv(paste0(basepath,"IUCN_seedsampling_info.csv"))
# # site_counts = site_counts[, !(colnames(site_counts) %in% c("RECSUMMARY", "RDEFILE"))]
# # predictor = unique(site_counts$taxon_name[!is.na(site_counts$taxon_name)])#[which(!(unique(CR_pred$taxon_name) %in% brahms_to_add$taxon_name))] #unique(CR_pred$taxon_name)[!(unique(CR_pred$taxon_name) %in% iucn_storage_behaviour$taxon_name)]
# # write.csv(data.frame(predictor), paste0(basepath,"msb_CE_orthodoxy_see.csv"), row.names=FALSE)
#
# #run the shiny seed predictor app
# # runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)
#
# # orth = read.csv(paste0(basepath,"Model-results-2024-07-01.csv"))
# # test = site_counts %>% left_join(orth[, c("Initial_List","probability.of.recalcitrance", "tax.level")],
# #                                  by = c("taxon_name"="Initial_List"))
# #
# # write.csv(test, paste0(basepath,"iucn_brahms_wcvp_orthodoxy.csv"))
#
# #######################################
# ## Combine and save
#
# index_orthodoxy = read.csv(paste0(basepath, "iucn_brahms_wcvp_orthodoxy.csv"))
# spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
#
# comp = index_orthodoxy[,c("taxon_name","probability.of.recalcitrance", "tax.level")]
# comp = comp[duplicated(comp$taxon_name)==FALSE, ]
#
# temp = spp_banked_recalcitrant %>% left_join(comp,
#                                              by=c("taxon_name" = "taxon_name"))
#
# temp$probability.of.recalcitrance = ifelse( is.na(temp$probability.of.recalcitrance.x),
#                       temp$probability.of.recalcitrance.y,
#                       temp$probability.of.recalcitrance.x)
#
# temp$banked_category = NA
# temp$banked_category[temp$probability.of.recalcitrance <= 0.3] = "orthodox"
# temp$banked_category[temp$probability.of.recalcitrance >= 0.7] = "recalcitrant"
# temp$banked_category[temp$probability.of.recalcitrance < 0.7 &
#                        temp$probability.of.recalcitrance > 0.3] = "intermediate"
#
#
#
# # colnames(comp)= c("taxon_name", "banked_recalcitrance", "taxonomic_prediction_level")
# # # remove duplicates
# #
# # # add the orthodoxy to the banked data
# # new = spp_banked_recalcitrant %>% left_join(comp,
# #                                             by=c("taxon_name" = "taxon_name"))
#
#
#
# new$banked_category = NA
# new$banked_category[new$banked_recalcitrance <= 0.3] = "orthodox"
# new$banked_category[new$banked_recalcitrance >= 0.7] = "recalcitrant"
# new$banked_category[new$banked_recalcitrance < 0.7 &
#                       new$banked_recalcitrance > 0.3] = "intermediate"
#
# new = new[!is.na(new$taxon_name), ]
# write.csv(new, paste0(basepath, "spp_banked_recalcitrant.csv"), row.names=FALSE )
#




####################################################
###################################################
####################################################
#
# spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
#
#
# # prepare the IUCN species and get their orthodoxy
# # orthodox_matches = data.frame(unique(spp_banked_recalcitrant$taxon_name))
# # dim(orthodox_matches)
# # nrow(orthodox_matches)/6
# # j=1
# # for(i in seq(1,nrow(orthodox_matches),960)){
# #   write.csv(orthodox_matches[i:(i+959),],paste0(basepath, "IUCN_to_match_",j,".csv"), row.names=FALSE )
# #   j=j+1
# # }
# #
# # # #run the shiny seed predictor app
# # runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)
#
# # Compile orthodoxy predictions
# iucn_orthodox = read.csv(paste0(basepath,"Model-results-2024-08-08_1.csv"))
# for (i in 2:6){
#   iucn_orthodox = rbind(iucn_orthodox,
#                         read.csv(paste0(basepath,"Model-results-2024-08-08_",i,".csv")))
# }
#
# iucn_orthodox = iucn_orthodox[which(!is.na(iucn_orthodox$Initial_List)),]
#
# length(iucn_orthodox$Initial_List[which(iucn_orthodox$wcvp_status == "Accepted")])
# length(iucn_orthodox$Initial_List[which(iucn_orthodox$Initial_List %in% spp_banked_recalcitrant$taxon_name)])
# length(iucn_orthodox$Initial_List[which(iucn_orthodox$wcvp_name %in% spp_banked_recalcitrant$taxon_name)])
# length(iucn_orthodox$Initial_List[which(iucn_orthodox$Accepted_name %in% spp_banked_recalcitrant$taxon_name)])
#
#
#
# iucn_orthodox[which(!(iucn_orthodox$Accepted_name %in% iucn_orthodox$wcvp_name)),]
#
# which(is.na(iucn_orthodox$probability.of.recalcitrance[which(iucn_orthodox$Initial_List %in%
#                                                                spp_banked_recalcitrant$taxon_name)]))
#
# for (spp_i in unique(spp_banked_recalcitrant$taxon_name)){
#   spp_i = iucn_orthodox$Initial_List[which(duplicated(iucn_orthodox$Initial_List) == T)]
#   temp = iucn_orthodox[which(iucn_orthodox$Initial_List %in%  spp_i),]
#
# }
