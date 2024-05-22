library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################

#load data from previous session
iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched_full_name.csv"))
# brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched.csv"))
# brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched.csv"))
# exceptional_wcvp_matched = read.csv(paste0(basepath,"exceptional_wcvp_matched.csv"))
exceptional_wcvp_matched = read.csv(paste0(basepath,"exceptional_unique_wcvp_matched.csv"))
wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

# load brahms data
brahms <- read.csv(paste0(basepath,"2024-03-21_164953044-BRAHMSOnlineData.csv"))

# remove duplicates
brahms <- brahms[duplicated(brahms$AccessionNumber)==FALSE,] # removes 441 duplicates





# how many species are in the bank?
length(unique(brahms_wcvp_matched$taxon_name)) # 45780

#how many collections?
length(unique(brahms_wcvp_matched$AccessionNumber)) # 197934

# find the MSB species that are CR endangered
brahms_wcvp_matched$CR = brahms_wcvp_matched$wcvp_accepted_id %in% iucn_wcvp_matched$wcvp_accepted_id
summary(brahms_wcvp_matched$CR)
#   Mode   FALSE    TRUE
# logical  195715    2221

# how many species in the bank are CR
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$CR)])) # 371

# subset the CR data
brahms_CR = brahms_wcvp_matched[brahms_wcvp_matched$CR,]
dim(brahms_CR)

# number of IUCN listed species
length(unique(iucn_wcvp_matched$wcvp_accepted_id)) #5645

# of the CR species which one are in the bank, and which ones not?
iucn_wcvp_matched$banked = iucn_wcvp_matched$wcvp_accepted_id %in% brahms_wcvp_matched$wcvp_accepted_id
summary(iucn_wcvp_matched$banked)
#    Mode   FALSE    TRUE
# logical    5295     372

# Attach the genus data
# brahms_CR = brahms_CR  %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
#                                      by=c("wcvp_accepted_id" = "plant_name_id"))

# brahms_CR = brahms_CR  %>% left_join(rWCVP::taxonomic_mapping,
#                          by=c("family" = "family"))

# iucn_wcvp_matched = iucn_wcvp_matched %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
#                                                     by=c("wcvp_accepted_id" = "plant_name_id"))
#
# iucn_wcvp_matched = iucn_wcvp_matched %>% left_join(rWCVP::taxonomic_mapping,
#                                                     by=c("family" = "family"))


# find how many of each family there are in IUCN
iucn_higher_list  = iucn_wcvp_matched[which(duplicated(iucn_wcvp_matched$wcvp_accepted_id)==F),] %>%
  group_by(higher) %>%
  tally()


any(iucn_wcvp_matched$family ==  "Bryophyta")
# IUCN years
length(which(iucn_wcvp_matched$yearPublished < 2000)) #334
length(which(iucn_wcvp_matched$yearPublished >= 2000 & iucn_wcvp_matched$yearPublished < 2010)) #453
length(which(iucn_wcvp_matched$yearPublished >= 2010 & iucn_wcvp_matched$yearPublished < 2020)) #1970
length(which(iucn_wcvp_matched$yearPublished >= 2020)) #2893

length(wcvp$taxon_status == "Accepted")
length(wcvp$taxon_status == "Accepted")

# how many are in wcvp
length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) / 5702
length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) / 5702
5702 - length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) - length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP"))


#########################################
#number of taxa in the bank
length(unique(brahms_wcvp_matched$taxon_name)) # 45811
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$taxonomic_backbone == "WCVP")]) )# 45779
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$taxonomic_backbone == "WFO")]) )# 45779

length(unique(MSB_wcvp_matched$taxon_name[which(MSB_wcvp_matched$taxonomic_backbone == "WFO")])) # 43


#### GET ORTHODOXY ########################################################################################

# prepare the IUCN species and get their orthodoxy
orthodox_matches = data.frame(unique(iucn_wcvp_matched$taxon_name[iucn_wcvp_matched$banked == F]))
dim(orthodox_matches)
j=1
for(i in seq(1,nrow(orthodox_matches),873)){
  write.csv(orthodox_matches[i:(i+872),],paste0(basepath, "orthodox_to_match_",j,".csv"), row.names=FALSE )
  j=j+1
}

#run the shiny seed predictor app
runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)

# Compile orthodoxy predictions
iucn_orthodox = read.csv(paste0(basepath,"Model-results-2024-04-04_1.csv"))
for (i in 2:6){
  iucn_orthodox = rbind(iucn_orthodox,
                        read.csv(paste0(basepath,"Model-results-2024-04-04_",i,".csv")))
}

#get rid of duplicates
iucn_orthodox = iucn_orthodox[which(duplicated(iucn_orthodox$Accepted_name)==FALSE),]

# combine the two datasets
iucn_wcvp_matched_orthodox = iucn_wcvp_matched %>% left_join(iucn_orthodox,
                                                             by = c("taxon_name" = "Accepted_name") )
# keep unique values
iucn_wcvp_matched_orthodox = unique(iucn_wcvp_matched_orthodox)

# specify the categories
iucn_wcvp_matched_orthodox$category = NA
iucn_wcvp_matched_orthodox$category[iucn_wcvp_matched_orthodox$banked == T] = "banked"
iucn_wcvp_matched_orthodox$category[iucn_wcvp_matched_orthodox$probability.of.recalcitrance <= 0.25] = "orthodox"
iucn_wcvp_matched_orthodox$category[iucn_wcvp_matched_orthodox$probability.of.recalcitrance >= 0.75] = "recalcitrant"
iucn_wcvp_matched_orthodox$category[iucn_wcvp_matched_orthodox$probability.of.recalcitrance < 0.75 & iucn_wcvp_matched_orthodox$probability.of.recalcitrance > 0.25] = "intermediate"

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

iucn_storage_behaviour[iucn_storage_behaviour$category == "intermediate",]
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

iucn_storage_behaviour$category[which((iucn_storage_behaviour$SID_Seed_Storage_Behaviour == "Intermediate") &
                                        is.na(iucn_storage_behaviour$category))] = "intermediate"


# Remaining species
remaining = unique(iucn_storage_behaviour$taxon_name[
  which(is.na(iucn_storage_behaviour$category))])
# remaining_2 = read.csv(paste0(basepath, "remaining_to_match.csv"))
write.csv(remaining,paste0(basepath, "remaining_to_match.csv"), row.names=FALSE )

#run the shiny seed predictor app
runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)

# iucn_storage_behaviour$category[iucn_storage_behaviour$EF1_seed_unavailable == "Yes"] = "EF1_seed_unavailable"
# iucn_storage_behaviour$category[which(iucn_storage_behaviour$EF2_desiccation_sensitive == "Yes")] = "EF2_desiccation_sensitive"
# iucn_storage_behaviour$category[which(iucn_storage_behaviour$EF3_short.lived == "Yes")] = "EF3_short_lived"
# iucn_storage_behaviour$category[which(iucn_storage_behaviour$EF4_deep_dormancy == "Yes")] = "EF2_desiccation_sensitive"
# iucn_storage_behaviour$taxon_name[which(iucn_storage_behaviour$EF3_short.lived == "Yes")]

orthodox_remaining = read.csv(paste0(basepath,"Model-results-2024-04-08_remaining.csv"))

# remove duplicates
orthodox_remaining = orthodox_remaining[duplicated(orthodox_remaining$Initial_List) == F, ]
# length(unique(orthodox_remaining$Initial_List))
# length(orthodox_remaining$Initial_List)

# which(wcvp$taxon_name == "Podocarpus decumbens")
colnames(orthodox_remaining) = c("Initial_List", "wcvp_name.y","wcvp_authors.y","wcvp_id.y" ,"wcvp_status.y",
                                 "Accepted_wcvp_name_id","Accepted_name","Accepted_wcvp_name_authors",
                                 "Accepted_name.1","Genus","Family","Order","tax.level",
                                 "probability.of.recalcitrance","storBehav")

NA_storage = iucn_storage_behaviour[which(is.na(iucn_storage_behaviour$category)),c(1:45,60:72)]

NA_storage = NA_storage %>% left_join(orthodox_remaining,
                                      by=c("taxon_name" = "Accepted_name"))

#reorder the column names
NA_storage = NA_storage[, colnames(iucn_storage_behaviour)]

#add in the categories
NA_storage$category[NA_storage$banked == T] = "banked"
NA_storage$category[NA_storage$probability.of.recalcitrance <= 0.25] = "orthodox"
NA_storage$category[NA_storage$probability.of.recalcitrance >= 0.75] = "recalcitrant"
NA_storage$category[NA_storage$probability.of.recalcitrance < 0.75 & NA_storage$probability.of.recalcitrance > 0.25] = "intermediate"

#remove the synonym row
# NA_storage = unique(NA_storage)
# id = NA_storage$X.1[duplicated(NA_storage$X.1)]
# NA_storage = NA_storage[-which(NA_storage$X.1 == id)[which(NA_storage$wcvp_status.y[NA_storage$X.1 == id] == "Synonym")],]

iucn_storage_behaviour[which(is.na(iucn_storage_behaviour$category)),] = NA_storage

# save the output
# write.csv(iucn_storage_behaviour, paste0(basepath, "iucn_orthodocy_recalcitrance.csv"))
iucn_storage_behaviour = read.csv(paste0(basepath, "iucn_orthodocy_recalcitrance.csv"))
# length(iucn_storage_behaviour$taxon_name)
# length(unique(iucn_storage_behaviour$taxon_name))



seed_count = brahms_unique_wcvp_matched[,c("taxon_name","wcvp_authors", "wcvp_accepted_id", "accessions","summed_count")]

# create summary table where adjusted seed counts per species are added together
seed_count = seed_count %>%
  group_by(taxon_name,wcvp_authors, wcvp_accepted_id,accessions,summed_count) %>%
  tally() %>%
  mutate(summed_count_2 = sum(summed_count),
         accessions_2 = sum(accessions)) %>%
  ungroup()
seed_count = seed_count[, c("taxon_name","wcvp_authors", "wcvp_accepted_id","accessions_2", "summed_count_2")]
seed_count = seed_count[duplicated(seed_count$taxon_name)==FALSE,]
colnames(seed_count) = c("taxon_name","wcvp_authors", "wcvp_accepted_id", "accessions","summed_count")

# add the accession data as will be useful later
iucn_storage_behaviour = iucn_storage_behaviour %>% left_join(seed_count[,c("taxon_name","accessions","summed_count")],
                                                              by = "taxon_name",
                                                              relationship = "many-to-one")




# reduce dataset size
colkeep = c("taxon_name","scientificName", "family", "genus","redlistCategory",
            "redlistCriteria", "yearPublished", "assessmentDate", "criteriaVersion",
            "populationTrend","systems", "realm","yearLastSeen","possiblyExtinct",
            "possiblyExtinctInTheWild", "scopes","banked",  #"Genus", "Family",
            "Order", "tax.level","probability.of.recalcitrance","storBehav",
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
length(unique(iucn_storage_behaviour$taxon_name))




########################################################################################################################
############## PREDICTED CR to be added to the dataset #################################################################
########################################################################################################################

# Add the IUCN predictions
iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
# keep only the CR ones
CR_pred = iucn_predictions[which(iucn_predictions$category == "CR"),]
# remove those that are already in the databsae
CR_pred = CR_pred[which(!(CR_pred$taxon_name %in% iucn_storage_behaviour$taxon_name)),]


########### See if the CR are in the bank already ######################################################################

# CR_pred_in_bank = CR_pred$taxon_name[unique(CR_pred$taxon_name) %in% unique(brahms_wcvp_matched$taxon_name)]
# Subset the msbp data
brahms_to_add = seed_count[seed_count$taxon_name %in% unique(CR_pred$taxon_name),]
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
brahms_to_add["Order"] = NA
brahms_to_add["tax.level"] = NA
brahms_to_add["probability.of.recalcitrance"] = NA
brahms_to_add["storBehav"] = NA
brahms_to_add["category"] = "banked"
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


###################################################################################################
####  GET CR SPECIES THAT AREN'T BANKED AND ADD ORTHODOXY #########################################

# remove species predicted CR that already have a CR prediction from IUCN
CR_pred_to_add = CR_pred$taxon_name[which(!(CR_pred$taxon_name %in% brahms_to_add$taxon_name))] #unique(CR_pred$taxon_name)[!(unique(CR_pred$taxon_name) %in% iucn_storage_behaviour$taxon_name)]
#unique(CR_pred$taxon_name)[which(!(unique(CR_pred$taxon_name) %in% brahms_to_add$taxon_name))] #unique(CR_pred$taxon_name)[!(unique(CR_pred$taxon_name) %in% iucn_storage_behaviour$taxon_name)]
write.csv(data.frame(CR_pred_to_add), paste0(basepath,"CR_predictions_for_orthodoxy_predictions.csv"), row.names=FALSE)

#run the shiny seed predictor app
runApp("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Recalcitrance predictor/Copy of SW App code AH KD.R", launch.browser = T)

# get the orthodoxy predictions
CR_pred_to_add = read.csv(paste0(basepath,"Model-results-2024-04-08_pred.csv"))
#remove duplicates
CR_pred_to_add = CR_pred_to_add[duplicated(CR_pred_to_add$Accepted_name)==FALSE,]
CR_pred_to_add = CR_pred_to_add %>% left_join(wcvp[,c("plant_name_id","family","genus",
                                                      "climate_description", "geographic_area")],
                                              by=c("Accepted_wcvp_name_id"="plant_name_id"))

CR_pred_to_add["taxon_name"] = CR_pred_to_add["Accepted_name"]
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

CR_pred_to_add["category"] = NA
CR_pred_to_add$category[CR_pred_to_add$banked == T] = "banked"
CR_pred_to_add$category[CR_pred_to_add$probability.of.recalcitrance <= 0.25] = "orthodox"
CR_pred_to_add$category[CR_pred_to_add$probability.of.recalcitrance >= 0.75] = "recalcitrant"
CR_pred_to_add$category[CR_pred_to_add$probability.of.recalcitrance < 0.75 & CR_pred_to_add$probability.of.recalcitrance > 0.25] = "intermediate"



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


# length(CR_pred_to_add$taxon_name)

###### COMBINE DATA    #############################################################################################
spp_banked_recalcitrant = rbind(iucn_storage_behaviour,
                                brahms_to_add,
                                CR_pred_to_add)


# check out the duplicates
length(spp_banked_recalcitrant$taxon_name)
length(unique(spp_banked_recalcitrant$taxon_name))

test = spp_banked_recalcitrant
test = test[which(!is.na(test$taxon_name)),]
# find duplicated names that don't have any accepted name
dupl = test$taxon_name %in% unique(test$taxon_name[ duplicated(test$taxon_name)])
dupl_nam = unique(test$taxon_name[dupl])
du = dupl_nam[82]

test$keep = 0
test$keep[which(!(test$taxon_name %in% dupl_nam))] = 1

last = c()
for (du in dupl_nam){
  temp = data.frame(test[test$taxon_name == du,])
  ### TO DO: remove predictions
  if (any(temp$redlistCriteria == "prediction")){
    test$keep[which(test$taxon_name == du)[which(temp$redlistCriteria != "prediction")]] = 1
  } else {
    test$keep[which(test$taxon_name == du)] = 1
  }
}

test = test[test$keep == 1,]
write.csv(test,paste0(basepath, "spp_banked_recalcitrant.csv"), row.names=FALSE )



