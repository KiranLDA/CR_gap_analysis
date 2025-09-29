library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################
# wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

wcvp_countries <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

#load data from previous sessions
# iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"))

brahms_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_wcvp_matched_full_name_infra.csv"))

# make sure only consider predicted that aren't already CR
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_unique_wcvp_matched_full_name_infra.csv"))
brahms_unique_wcvp_matched = brahms_unique_wcvp_matched[which(!is.na(brahms_unique_wcvp_matched$species)),]

# create summary table where adjusted seed counts per species are added together
seed_count = brahms_unique_wcvp_matched[,c("taxon_name","taxon_authors", "wcvp_accepted_id", "accessions","summed_count")]
seed_count = seed_count %>%
  group_by(taxon_name, taxon_authors, wcvp_accepted_id, accessions, summed_count) %>%
  tally() %>%
  mutate(summed_count_2 = sum(summed_count),
         accessions_2 = sum(accessions)) %>%
  ungroup()
seed_count = seed_count[, c("taxon_name","taxon_authors", "wcvp_accepted_id","accessions_2", "summed_count_2")]
seed_count = seed_count[duplicated(seed_count$taxon_name)==FALSE,]
colnames(seed_count) = c("taxon_name","taxon_authors", "wcvp_accepted_id", "accessions","summed_count")



# exceptional <- read.csv(paste0(basepath, "pence_appendix1.csv"))
exceptional_wcvp_matched = read.csv(paste0(basepath,"revision_1/exceptional_unique_wcvp_matched.csv"))
exceptional_wcvp_matched$wcvp_accepted_id <- as.character(exceptional_wcvp_matched$wcvp_accepted_id)
# replace names
colnames(exceptional_wcvp_matched)[which(colnames(exceptional_wcvp_matched) %in%
                                           c("Species.name","Exceptional.status",
                                             "EF1..seed.unavailable.","EF2...desiccation.sensitive.",
                                             "EF3..short.lived..", "EF4..deep.dormancy.", "SID.Seed.Storage.Behaviour",
                                             "Woody.or.non.woody.", "PlantSearch.plant.collections", "PlantSearch.seed.collections",
                                             "PlantSearch.tissue.culture.collections", "PlantSearch.cryopreservation.collections",
                                             "PlantSearch.uncategorized.germplasm.collections"))] <- c("Species_name","Exceptional_status",
                                                                                                       "EF1_seed_unavailable","EF2_desiccation_sensitive",
                                                                                                       "EF3_short_lived", "EF4_deep_dormancy", "SID_Seed_Storage_Behaviour",
                                                                                                       "Woody_or_non_woody", "PlantSearch_plant_collections", "PlantSearch_seed_collections",
                                                                                                       "PlantSearch_tissue_culture_collections", "PlantSearch_cryopreservation_collections",
                                                                                                       "PlantSearch_uncategorized_germplasm_collections")


# Get predictions
iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_predictions_wcvp_matched.csv"))
iucn_CR_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]

# keep only the ones that aren't already in IUCN
iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(ifelse(iucn_CR_predictions_wcvp_matched$taxon_name %in%
                                                                                   iucn_wcvp_matched$taxon_name, FALSE,TRUE)),]
length(unique(iucn_CR_predictions_wcvp_matched$taxon_name)) # 197



#### GET ORTHODOXY ########################################################################################

### IUCN

# prepare the IUCN species and get their orthodoxy
# orthodox_matches = data.frame(unique(iucn_wcvp_matched$taxon_name))
# dim(orthodox_matches)
# j=1
# for(i in seq(1,nrow(orthodox_matches),810)){
#   write.csv(orthodox_matches[i:(i+809),],paste0(basepath, "revision_1/orthodox_to_match_",j,".csv"), row.names=FALSE )
#   j=j+1
# }

# # run the shiny seed predictor app
# runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)


# Compile orthodoxy predictions
iucn_orthodox = read.csv(paste0(basepath,"revision_1/Model-results-2025-09-24_1.csv"), encoding = "UTF-8")
for (i in 2:8){
  iucn_orthodox = rbind(iucn_orthodox,
                        read.csv(paste0(basepath,"revision_1/Model-results-2025-09-24_",i,".csv"), encoding = "UTF-8"))
}


# combine the two datasets
iucn_wcvp_matched_orthodox = iucn_wcvp_matched %>% left_join(iucn_orthodox[,c("Initial_List", "Accepted_name","Accepted_wcvp_name_authors",
                                                                              "tax.level",
                                                                              "probability.of.recalcitrance",
                                                                              "storBehav")],
                                                             by = c("taxon_name" = "Initial_List") )

# Remove storage behaviour for those that cannot be matched with WCVP
# (the ones that could not be matched by the storage behaviour)
iucn_wcvp_matched_orthodox[which(iucn_wcvp_matched_orthodox$taxonomic_backbone == "WFO"),
                           c("Accepted_name","Accepted_wcvp_name_authors","tax.level",
                             "probability.of.recalcitrance","storBehav")] <- NA


# get rid of duplicates
iucn_wcvp_matched_orthodox <- iucn_wcvp_matched_orthodox[which(duplicated(iucn_wcvp_matched_orthodox) == F),]


# banked
iucn_wcvp_matched_orthodox$banked = ifelse(iucn_wcvp_matched_orthodox$wcvp_accepted_id %in% brahms_unique_wcvp_matched$wcvp_accepted_id,
                                           TRUE,FALSE)



# remove duplicates
iucn_storage_behaviour = iucn_wcvp_matched_orthodox %>% left_join(exceptional_wcvp_matched[,c("taxon_name", "taxon_authors", "wcvp_accepted_id","Exceptional_status",
                                                                                              "EF1_seed_unavailable","EF2_desiccation_sensitive",
                                                                                              "EF3_short_lived", "EF4_deep_dormancy", "SID_Seed_Storage_Behaviour",
                                                                                              "Woody_or_non_woody", "PlantSearch_plant_collections", "PlantSearch_seed_collections",
                                                                                              "PlantSearch_tissue_culture_collections", "PlantSearch_cryopreservation_collections",
                                                                                              "PlantSearch_uncategorized_germplasm_collections")],
                                                                  by=c("taxon_name","taxon_authors","wcvp_accepted_id"),
                                                                  relationship = "many-to-one")

iucn_storage_behaviour = unique(iucn_storage_behaviour)


# add the accession data as will be useful later
iucn_storage_behaviour = iucn_storage_behaviour %>% left_join(seed_count[,c(#"wcvp_accepted_id",
                                                                            "taxon_name","accessions","summed_count")],
                                                              by = "taxon_name",
                                                              # by = "wcvp_accepted_id",
                                                             relationship = "many-to-one")



# reduce dataset size
colkeep = c("taxon_name","taxon_authors", "wcvp_accepted_id","scientificName", "family", "genus","higher",
            "order","redlistCategory",
            "redlistCriteria", "yearPublished", "assessmentDate", "criteriaVersion",
            "populationTrend","systems", "realm","yearLastSeen","possiblyExtinct",
            "possiblyExtinctInTheWild", "scopes","banked",
            #"Genus", "Family", "Order",
            "tax.level","probability.of.recalcitrance","storBehav",
            # "category",
            "Exceptional_status","EF1_seed_unavailable",
            "EF2_desiccation_sensitive","EF3_short_lived","EF4_deep_dormancy",
            "SID_Seed_Storage_Behaviour","Woody_or_non_woody",
            "PlantSearch_plant_collections", "PlantSearch_seed_collections",
            "PlantSearch_tissue_culture_collections",
            "PlantSearch_cryopreservation_collections",
            "PlantSearch_uncategorized_germplasm_collections",
            "accessions","summed_count")

iucn_storage_behaviour = iucn_storage_behaviour[,colkeep]
iucn_storage_behaviour = iucn_storage_behaviour[which(!is.na(iucn_storage_behaviour$taxon_name)),]

# save the output
write.csv(iucn_storage_behaviour, paste0(basepath, "revision_1/iucn_orthodoxy_recalcitrance.csv"), row.names=FALSE, fileEncoding = "UTF-8")
iucn_storage_behaviour = read.csv(paste0(basepath, "revision_1/iucn_orthodoxy_recalcitrance.csv"))




length(iucn_storage_behaviour$taxon_name) # 6501
dupli = iucn_storage_behaviour$taxon_name[which(duplicated(iucn_storage_behaviour$taxon_name))]
# View(iucn_storage_behaviour[which(iucn_storage_behaviour$taxon_name %in% dupli),])

# more names in IUCN than WCVP
length(unique(iucn_storage_behaviour$taxon_name)) # 6465
length(unique(iucn_storage_behaviour$scientificName)) # 6480




########################################################################################################################
############## PREDICTED CR to be added to the dataset #################################################################
########################################################################################################################

# Add the IUCN predictions
CR_pred = unique(iucn_CR_predictions_wcvp_matched$taxon_name)
CR_pred = CR_pred[which(!is.na(CR_pred))]
CR_pred = data.frame(CR_pred)
dim(CR_pred)
# write.csv(CR_pred, paste0(basepath, "revision_1/orthodox_to_match_predictions.csv"), row.names=FALSE , fileEncoding = "UTF-8")

# run the shiny seed predictor app
# runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)

CR_pred_to_add = read.csv(paste0(basepath,"revision_1/Model-results-2025-09-24_predictions.csv"))




# combine the two datasets
CR_pred_to_add = iucn_CR_predictions_wcvp_matched %>% left_join(CR_pred_to_add[,c("Initial_List", "Accepted_name","Accepted_wcvp_name_authors",
                                                                              "tax.level",
                                                                              "probability.of.recalcitrance",
                                                                              "storBehav")],
                                                             by = c("taxon_name" = "Initial_List") )

# Remove storage behaviour for those that cannot be matched with WCVP (the ones that could not be matched by the storage behaviour)
CR_pred_to_add[which(CR_pred_to_add$taxonomic_backbone == "WFO"),
                           c("Accepted_name","Accepted_wcvp_name_authors","tax.level",
                             "probability.of.recalcitrance","storBehav")] <- NA


# get rid of duplicates
CR_pred_to_add <- CR_pred_to_add[which(duplicated(CR_pred_to_add) == F),]



# remove duplicates
# CR_pred_to_add = CR_pred_to_add[duplicated(CR_pred_to_add$Initial_List)==FALSE,]
CR_pred_to_add = CR_pred_to_add %>% left_join(wcvp[,c("taxon_name", "taxon_authors", # "plant_name_id",
                                                      # "family","genus",
                                                      "climate_description", "geographic_area")],
                                              by = c("taxon_name", "taxon_authors")) #c("Initial_List" = "taxon_name"))
#by=c("Accepted_wcvp_name_id"="plant_name_id"))

# CR_pred_to_add["taxon_name"] = CR_pred_to_add["Initial_List"]
CR_pred_to_add["scientificName"] = NA
CR_pred_to_add["redlistCategory"] = "Critically Endangered"
CR_pred_to_add["redlistCriteria"] = "prediction"
CR_pred_to_add["yearPublished"] = 2024
CR_pred_to_add["assessmentDate"] = 2024
CR_pred_to_add["criteriaVersion"] = NA
CR_pred_to_add["populationTrend"] = NA
CR_pred_to_add["systems"] = NA
CR_pred_to_add["realm"] = CR_pred_to_add["climate_description"]
CR_pred_to_add["yearLastSeen"] = ""
CR_pred_to_add["possiblyExtinct"] = NA
CR_pred_to_add["possiblyExtinctInTheWild"] = NA
CR_pred_to_add["scopes"] = CR_pred_to_add["geographic_area"]
CR_pred_to_add["banked"] = ifelse(CR_pred_to_add$wcvp_accepted_id %in% brahms_unique_wcvp_matched$wcvp_accepted_id,
                                  TRUE,FALSE)


# CR_pred_to_add = CR_pred_to_add %>% left_join(rWCVP::taxonomic_mapping,
#                                               by=c("family" = "family"))
# CR_pred_to_add["category"] = NA
# CR_pred_to_add$category[CR_pred_to_add$banked == T] = "banked"
# CR_pred_to_add$category[which(CR_pred_to_add$probability.of.recalcitrance <= 0.3)] = "orthodox"
# CR_pred_to_add$category[which(CR_pred_to_add$probability.of.recalcitrance >= 0.7)] = "recalcitrant"
# CR_pred_to_add$category[which(CR_pred_to_add$probability.of.recalcitrance < 0.7 & CR_pred_to_add$probability.of.recalcitrance > 0.3)] = "unknown"
#

CR_pred_to_add$wcvp_accepted_id <- as.character(CR_pred_to_add$wcvp_accepted_id)

CR_pred_to_add = CR_pred_to_add %>% left_join(exceptional_wcvp_matched[,c("taxon_name","taxon_authors", "wcvp_accepted_id", "Exceptional_status",
                                                                          "EF1_seed_unavailable","EF2_desiccation_sensitive",
                                                                          "EF3_short_lived", "EF4_deep_dormancy", "SID_Seed_Storage_Behaviour",
                                                                          "Woody_or_non_woody", "PlantSearch_plant_collections", "PlantSearch_seed_collections",
                                                                          "PlantSearch_tissue_culture_collections", "PlantSearch_cryopreservation_collections",
                                                                          "PlantSearch_uncategorized_germplasm_collections")],
                                              by=c("taxon_name", "taxon_authors", "wcvp_accepted_id"))



# add counts and accessions
CR_pred_to_add = CR_pred_to_add %>% left_join(seed_count[,c("wcvp_accepted_id","accessions","summed_count")],
                                                              # by = "taxon_name",
                                                              by = "wcvp_accepted_id",
                                                              relationship = "many-to-one")


# only keep the columns that match IUCN dataframe
CR_pred_to_add = CR_pred_to_add[,colkeep]
CR_pred_to_add = CR_pred_to_add[which(!is.na(CR_pred_to_add$taxon_name)),]

# save the output
write.csv(CR_pred_to_add, paste0(basepath, "revision_1/CR_pred_orthodoxy_recalcitrance.csv"), row.names=FALSE, fileEncoding = "UTF-8")
CR_pred_to_add = read.csv(paste0(basepath, "revision_1/CR_pred_orthodoxy_recalcitrance.csv"))



###############################################################################################################
###############################################################################################################
###### COMBINE DATA    ########################################################################################
###############################################################################################################

# combine the datasets
spp_banked_recalcitrant = rbind(iucn_storage_behaviour, # banked and unbaked IUCN
                                # brahms_to_add, # banked IUCN prediction
                                CR_pred_to_add) # unbanked IUCN prediction

test = spp_banked_recalcitrant

# make sure there is no name overlap in the IUCN and predictions data
CR_pred_to_add$taxon_name[CR_pred_to_add$taxon_name %in% iucn_storage_behaviour$taxon_name]


# # how many taxa?
# length(unique(iucn_storage_behaviour$taxon_name))
# # 6465
# length(unique(CR_pred_to_add$taxon_name))
# # 197
# sum(length(unique(iucn_storage_behaviour$taxon_name)),length(unique(CR_pred_to_add$taxon_name)))
# # 6662
# length(unique(spp_banked_recalcitrant$taxon_name))
# # 6662
#
#
# # check out the duplicates
# length(spp_banked_recalcitrant$taxon_name) # 6732
# length(unique(spp_banked_recalcitrant$taxon_name)) # 6662


# test = test[which(!is.na(test$taxon_name)),]

# find duplicated names that don't have any accepted name
# dupl = test$taxon_name %in% unique(test$taxon_name[ duplicated(test$taxon_name)])
# dupl_nam = unique(test$taxon_name[dupl])
# dupl_nam = dupl_nam[order(dupl_nam)]

#######################################################################################################
####### add in the extra species categories from Dani and Hawaii ######################################
#######################################################################################################

extras <- read.csv(paste0(basepath, "revision_1/extras_wcvp_matched_full_name.csv"))
extras$source <- paste0(extras[,"Reference..chlorophyllous."],"; ",extras[,"Reference..longevity."])
extras$source[which(extras$source == "; ")] = NA
#combine the extra with the other data
test = spp_banked_recalcitrant %>% left_join(extras[c("taxon_name","Storage", "source")],
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
test$category_certain_ref[id] = "Pence et al. 2022"#"SID"

id = which(is.na(test$category_uncertain) & test$Exceptional_status %in% c("Intermediate"))
test$category_uncertain[id] = "intermediate"
test$category_uncertain_ref[id] = "Pence et al. 2022"#"SID"


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

id = which(is.na(test$category_certain) & test$Exceptional_status %in% c("Non-exceptional" ))
test$category_certain[id] = "orthodox"
test$category_certain_ref[id] = "Pence et al. 2022"#"SID"
test$category_uncertain[id] = "orthodox"
test$category_uncertain_ref[id] = "Pence et al. 2022"#"SID"



# step 4: add in seed storage predictor

# orthodox uncertain
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
id = which(is.na(test$category_uncertain) & test$Exceptional_status %in% c("Probably non-exceptional" ))
test$category_uncertain[id] = "orthodox"
test$category_uncertain_ref[id] = "Pence et al. 2022"#"SID"


# have a look
# View(test[, c("storBehav","probability.of.recalcitrance","tax.level","SID_Seed_Storage_Behaviour",
#               "Exceptional_status","Storage",
#               "category_uncertain", "category_certain")])


spp_banked_recalcitrant = test
spp_banked_recalcitrant$higher[spp_banked_recalcitrant$higher == "A"] = "Angiosperms"
spp_banked_recalcitrant$higher[spp_banked_recalcitrant$higher == "Polypodiophyta"] = "Ferns"

##### SAVE ################################################

write.csv(spp_banked_recalcitrant, paste0(basepath, "revision_1/spp_banked_recalcitrant.csv"), row.names=FALSE , fileEncoding = "UTF-8")

###################################################################################################
####                                                         ######################################
####      MAKE SURE SPECIES IN THE BANK ARE ORTHODOX         ######################################
####                                                         ######################################
###################################################################################################


# !!!! check


# # # # remove species predicted CR that already have a CR prediction from IUCN
# site_counts = read.csv(paste0(basepath,"revision_1/iucn_brahms_wcvp_matched_full_name.csv"))#read.csv(paste0(basepath,"IUCN_seedsampling_info.csv"))
# site_counts = site_counts[, !(colnames(site_counts) %in% c("RECSUMMARY", "RDEFILE"))]
# # predictor = unique(site_counts$taxon_name[!is.na(site_counts$taxon_name)])#[which(!(unique(CR_pred$taxon_name) %in% brahms_to_add$taxon_name))] #unique(CR_pred$taxon_name)[!(unique(CR_pred$taxon_name) %in% iucn_storage_behaviour$taxon_name)]
# # write.csv(data.frame(predictor), paste0(basepath,"msb_CE_orthodoxy_see.csv"), row.names=FALSE, fileEncoding = "UTF-8")
#
# #run the shiny seed predictor app
# # runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)
#
# # orth = read.csv(paste0(basepath,"Model-results-2024-07-01.csv"))
#
# spp_banked_recalcitrant = read.csv(paste0(basepath, "revision_1/spp_banked_recalcitrant.csv"))
# test = site_counts %>% left_join(spp_banked_recalcitrant,
#                                  by = c("taxon_name"="taxon_name"))
# write.csv(test, paste0(basepath,"revision_1/iucn_brahms_wcvp_orthodoxy.csv"), row.names=FALSE)


# !!!! check


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


##### Rev 1 - tested the ones wiht no results and ran again
# spp_banked_recalcitrant = read.csv(paste0(basepath, "revision_1/spp_banked_recalcitrant.csv") )
#
# test_again = spp_banked_recalcitrant[which(is.na(spp_banked_recalcitrant$probability.of.recalcitrance)),"taxon_name"]
# test_again = test_again[!duplicated(test_again) ]
# write.csv(test_again, paste0(basepath, "revision_1/test_again.csv"), row.names=FALSE )
#
#
# # run the shiny seed predictor app
# runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)


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
