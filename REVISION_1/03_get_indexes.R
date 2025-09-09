
library(shiny)
library(dplyr)
# library(rWCVP)
library(sf)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################
wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

# #load data from previous session
#
# #iucn redlist data
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"))
#
View(iucn_wcvp_matched[which(iucn_wcvp_matched$match_type == "Fuzzy (edit distance)"),
                       c("scientificName","taxon_name","match_similarity", "match_edit_distance","keep",
                         "match_logic", "taxonomic_backbone", "wcvp_rank","wcvp_status",
                         "wcvp_homotypic")])


# get rid of subspecies and varieties that could not be matched except at species level
# cbind(iucn_wcvp_matched$scientificName[which(stringr::str_count(iucn_wcvp_matched$scientificName, "\\w+") >2)],
#       iucn_wcvp_matched$taxon_name[which(stringr::str_count(iucn_wcvp_matched$scientificName, "\\w+") >2)])
# cbind(iucn_wcvp_matched$scientificName[which(stringr::str_count(iucn_wcvp_matched$taxon_name, "\\w+") >3)],
#       iucn_wcvp_matched$taxon_name[which(stringr::str_count(iucn_wcvp_matched$taxon_name, "\\w+") >3)])


# # the MSB data
# brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))
# brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name_infra.csv"))

# # get rid of subspecies and varieties that could not be matched except at species level
# cbind(brahms_wcvp_matched$full_name[which((stringr::str_count(brahms_wcvp_matched$full_name, "\\w+") > 2) &
#                                             (stringr::str_count(brahms_wcvp_matched$taxon_name, "\\w+") == 2))],
#       brahms_wcvp_matched$taxon_name[which((stringr::str_count(brahms_wcvp_matched$full_name, "\\w+") > 2) &
#                                              (stringr::str_count(brahms_wcvp_matched$taxon_name, "\\w+") == 2))])

# brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched_full_name.csv"))
#
# # the exceptional species )recalcitrant
# exceptional <- read.csv(paste0(basepath, "pence_appendix1.csv"))
# exceptional_wcvp_matched = read.csv(paste0(basepath,"exceptional_unique_wcvp_matched.csv"))
#
# # species with their IUCN predictions
# iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
# iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "iucn_predictions_wcvp_matched.csv"))
# iucn_CR_predictions = iucn_predictions[which(iucn_predictions$category == "CR"),]
# iucn_CR_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]# keep only the CR ones
# iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in% iucn_wcvp_matched$taxon_name)),]# keep only the ones that aren't already in IUCN
# iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$wcvp_ipni_id %in%
#                                                       iucn_wcvp_matched$wcvp_ipni_id))]
# iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in%
#                                                       iucn_wcvp_matched$taxon_name))]
#
# # wcvp data
# wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
# wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
#
# # combined IUCN data with banked and unbanked and CR categories
# iucn_banked_recalitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))



seed_data_to_add_1 <- read.csv(paste0(basepath, "seedcounts_2024-05-28.csv"))
seed_data_to_add_1 = seed_data_to_add_1[!(duplicated(seed_data_to_add_1$AccessionNumber)),]

# seed_data_to_add_2 <- read.csv(paste0(basepath, "seedcounts2_2024-05-28.csv"))
# seed_data_to_add_2 = seed_data_to_add_2[!(duplicated(seed_data_to_add_2$AccessionNumber)),]
#
#
#
# # germination test data
# germination <- read.csv(paste0(basepath, "germination_test_2024-05-28.csv"))
#
# # Banked IUCN
# iucn_banked <- iucn_banked_recalitrance[which(iucn_banked_recalitrance$category == "banked"),]
# iucn_banked$taxon_name

# get the tdwg to country mapping
tdwg3_countries <- read.csv(paste0(basepath, "country_tdwg3_mapping_multicountry.csv"))
tdwg3_countries$ISO_code[is.na(tdwg3_countries$ISO_code)] ="NA"

# tdwg3_countries$COUNTRY[tdwg3_countries$COUNTRY ==  "Turks\xa0and\xa0Caicos\xa0Islands"] = "Turks and Caicos Islands"
# tdwg3_countries$COUNTRY[tdwg3_countries$COUNTRY ==  "Northern\xa0Mariana\xa0Islands"] = "Northern Mariana Islands"
# tdwg3_countries$COUNTRY[tdwg3_countries$COUNTRY ==  "Falkland\xa0Islands"] = "Falkland Islands"
# tdwg3_countries$COUNTRY[tdwg3_countries$COUNTRY ==  "Cayman\xa0Islands"] ="Cayman Islands"
#
# write.csv(tdwg3_countries, paste0(basepath, "country_tdwg3_mapping.csv"), row.names = FALSE)

# ####################################################################

# get the brahms data
site_counts = read.csv(paste0(basepath,"revision_1/iucn_brahms_wcvp_orthodoxy.csv"))#"iucn_brahms_wcvp_matched_full_name.csv"))#read.csv(paste0(basepath,"IUCN_seedsampling_info.csv"))
site_counts = site_counts[, !(colnames(site_counts) %in% c("RECSUMMARY", "RDEFILE"))]

# remove thos that aren't in activate use
site_counts = site_counts[site_counts$TTH != "*",]

# Combine the earthcape and brahms online (data warehouse) data
cultivated <- read.csv(paste0(basepath, "cultivated_2024-05-28.csv"))
cultivated = cultivated[!(duplicated(cultivated$AccessionNumber)),]

cultivated2 = read.csv(paste0(basepath, "ICMS_cultivated_26_06_24.csv"))
cultivated2$AccessionNumber = sapply(1:nrow(cultivated2),
                                     function(x){as.character(as.numeric(strsplit(cultivated2$Catalogue.Number[x], "K:MSB-")[[1]][2]))})
cultivated = cultivated %>% left_join(cultivated2[,c("AccessionNumber", "Cultivated", "Derived.From", "Cultivated.Isolation.Technique")],
                                      by = c("AccessionNumber" = "AccessionNumber"))

cultivated$Cultivated = ifelse(is.na(cultivated$Cultivated),FALSE,TRUE)
cultivated$CultivatedFlag = ifelse(cultivated$CultivatedFlag == "True", TRUE, FALSE)
cultivated$Derived.From[cultivated$Derived.From == ""] = NA

cultivated$CultivatedAll = ifelse((cultivated$Cultivated | cultivated$CultivatedFlag),TRUE,FALSE)
cbind(cultivated$Cultivated, cultivated$CultivatedFlag, cultivated$CultivatedAll)


site_counts = site_counts %>% left_join(cultivated[,c("AccessionNumber","CultivatedFlag","ProjectName",
                                                      "ImageCount","Cultivated" ,"Derived.From","Cultivated.Isolation.Technique",
                                                      "CultivatedAll" )],
                                        by = c("ACCESSION" =  "AccessionNumber"))

site_counts = site_counts[which(!is.na(site_counts$taxon_name)),]

# "ACCESSION", "PLANTTOTAL", "PLANTSAMP", "PCSEED", "PCSAMPLED", "WILDCULT"


# "ACCESSION", "PLANTTOTAL", "PLANTSAMP", "PCSEED", "PCSAMPLED", "WILDCULT"

################# GET the latest germination test ###################

# # make the data smaller with specific columns
# germination_last_test = germination[,c("PassFail", "Result",
#                                        "DateStarted", "AccessionNumber")]
#
# # convert date
# germination_last_test$DateStarted = as.Date(germination_last_test$DateStarted, "%d/%m/%Y %H:%M:%S")
#
# # find duplicates
# dupl = germination_last_test$AccessionNumber %in% unique(germination_last_test$AccessionNumber[ duplicated(germination_last_test$AccessionNumber)])
# dupl_nam = unique(germination_last_test$AccessionNumber[dupl])
# germination_last_test$keep = 0
#
# # loop over duplicates
# for (du in dupl_nam){
#   to_keep = which(germination_last_test$AccessionNumber == du)
#   temp = germination_last_test[to_keep,]
#   i = which(temp$DateStarted == max(temp$DateStarted))
#   if (length(i) > 1){
#     i = i[which(temp$Result[i] == max(temp$Result[i]))]
#     if (length(i) > 1){
#       i = i[1]
#     }
#   }
#   germination_last_test$keep[to_keep[i]] = 1
# }
#
# germination_last_test = germination_last_test[germination_last_test$keep == 1,]
# write.csv(germination_last_test,
#           paste0(basepath, "germination_last_test.csv"))
germination_last_test = read.csv(paste0(basepath, "germination_last_test.csv"))

# # add the germination tests
# brahms_germination_wcvp_matched = brahms_wcvp_matched %>% left_join(germination_last_test[,c("PassFail", "Result",
#                                                  "DateStarted", "AccessionNumber")],
#                                   by= "AccessionNumber")
#
#
# brahms_germination_wcvp_matched = brahms_germination_wcvp_matched %>% left_join(seed_data_to_add_1[,c("AccessionNumber",
#                                                 "DistributionPolicy",
#                                                 "DateCollected",
#                                                 "NumberPlantsSampled",
#                                                 "NumberPlantsLocated",
#                                                 "DateDonated",
#                                                 "DateGermplasmBanked")],
#                           by= "AccessionNumber")
#
# brahms_germination_wcvp_matched = brahms_germination_wcvp_matched %>% left_join(seed_data_to_add_2[,c("AccessionNumber",
#                                                 "DonorOrg",
#                                                 "MajorRegion",
#                                                 "MinorRegion",
#                                                 "LocalityText",
#                                                 "CurrentSeedQuantity",
#                                                 "LocationSummary")],
#                           by= "AccessionNumber")
#
#
# brahms_germination_wcvp_matched = brahms_germination_wcvp_matched %>% left_join(cultivated[,c("AccessionNumber",
#                                                                                               "CultivatedFlag")],
#                                                                                 by= "AccessionNumber")
#
# write.csv(brahms_germination_wcvp_matched,
#           paste0(basepath, "brahms_germination_wcvp_matched.csv"))

site_counts = site_counts %>% left_join(germination_last_test[,c("PassFail", "Result",
                                                                 "DateStarted", "AccessionNumber")],
                                        by= c("ACCESSION" =  "AccessionNumber"))


site_counts = site_counts %>% left_join(seed_data_to_add_1[,c("AccessionNumber",
                                                              #"DistributionPolicy",
                                                              "DateCollected",
                                                              #"NumberPlantsSampled",
                                                              #"NumberPlantsLocated",
                                                              "DateDonated",
                                                              "DateGermplasmBanked")],
                                        by= c("ACCESSION" =  "AccessionNumber"))


# site_counts = site_counts %>% left_join(brahms_wcvp_matched[,c("AccessionNumber","wcvp_accepted_id","taxonomic_backbone",
#                                                  "taxon_name","family","higher","order"  )],
#                                  by= c("ACCESSION" =  "AccessionNumber"))
# brahms_germination_wcvp_matched = read.csv(paste0(basepath, "brahms_germination_wcvp_matched.csv"))




#######################################################################################

# The ‘Information index’
# contains three variables and scores given depending on the availability of data
# are shown in brackets:
#
# i) ‘Geographic’:
#    - ‘coordinates’ (1),
#    - ‘area’ (0.75),
#    - ‘country’ (0.5),
#    - ‘no information’ (0);
#
# ii) ‘verified taxonomy’:
#      - ‘yes’ (1),
#      - ‘not verified’ or ‘no information’ (0);
#
# iii) ‘collection year’:
#     - ‘yes’ (1),
#     - ‘no information’ (0).
#
# The ‘Viability index’ contains two variables:
#   - ‘current count’:
#       -‘yes’ (1),
#       -‘no information’ (0);
#   - ‘adjusted count’:
#       - ‘yes’ (1),
#       - ‘no information’ (0);
#   - ‘germination test’:
#       - ‘recent test’ (1),
#       - ‘test older than 15 years’ (0.5),
#       - ‘no test information available’ (0).
#
# The ‘Genetic diversity’ index was calculated differently depending
# if the collection was originated from cultivated or wild materials.
# For cultivated collections,
# 1 was given if relation to parent and if related to the isolation technique
# information are available,
# 0.5 if only one of them was available, and
# 0 when no information available.
#
# For ex situ collections directly sampled from wild populations,
# three variables were considered:
# ‘Plant sampled’ (number of plants which seeds were collected from),
# ‘Plant total’ (total number of plants in the population), and
# ‘Plant seed’ (total number of individuals with mature seeds in the population).
#
# Given that we aim at evaluating if 50 mature individuals were collected or, when not,
# if all individuals in the population or all mature individuals were collected;
# the scores weighted when the
# three variables are available (1), when
# ‘Plant sampled’ and ‘Plant total’ or ‘Plant seed’ (0.8),
# ‘Plant sampled’ (0.6), ‘Plant total’ and ‘Plant seed’ (0.4), ‘
# Plant total’ or ‘Plant seed’ (0.2),
# ‘no info’ (0).

#########################################################################################
# GET THE INFORMATION INDEX #############################################################
#########################################################################################

#####################################
####    Geographic INDEX    #########
#####################################
# i) ‘Geographic’:
#    - ‘coordinates’ (1),
#    - ‘area’ (0.75),
#    - ‘country’ (0.5),
#    - ‘no information’ (0);

# no inforation
site_counts$geographic_index = 0

# country
site_counts$geographic_index = ifelse(site_counts$COUNTRY %in% c("?", "Unknown" ),
                                      site_counts$geographic_index,
                                      0.5)
# area (locality)
site_counts$geographic_index = ifelse(site_counts$LOCNOTES == "",
                                      site_counts$geographic_index,
                                      0.75)
# coordinates
site_counts$geographic_index = ifelse(site_counts$LAT == 0,
                                      site_counts$geographic_index,
                                      1)

summary(as.factor(site_counts$geographic_index))/length(site_counts$geographic_index)
# 0           0.5         0.75           1
# 0.008517888 0.106473595 0.378620102 0.506388416
mean(site_counts$geographic_index)
# 0.8435903

#####################################
####    taxonomy INDEX      #########
#####################################
# ii) ‘verified taxonomy’:
#      - ‘yes’ (1),
#      - ‘not verified’ or ‘no information’ (0);

# no inforation
site_counts$taxonomy_index = 0

# name matched
site_counts$taxonomy_index = ifelse(is.na(site_counts$taxonomic_backbone),
                                    site_counts$taxonomy_index,
                                    1)

summary(as.factor(site_counts$taxonomy_index))/length(site_counts$taxonomy_index)

mean(site_counts$taxonomy_index)
# 1


#####################################
####    year INDEX          #########
#####################################
# iii) ‘collection year’:
#     - ‘yes’ (1),
#     - ‘no information’ (0).

# no inforation
site_counts$year_index = 0

# name matched  # DONORDATE
site_counts$year_index = ifelse(is.na(site_counts$DateCollected),
                                site_counts$year_index,
                                1)

summary(as.factor(site_counts$year_index))/length(site_counts$year_index)
# 0          1
# 0.06516184 0.93483816
mean(site_counts$year_index)
# 0.9348382

############################################################
##     Combine for information index    ######################
############################################################
site_counts$information_index = (site_counts$year_index +
                                   site_counts$taxonomy_index +
                                   site_counts$geographic_index)


site_counts$information_index = site_counts$information_index/3

mean(site_counts$information_index) #  0.9261428


#########################################################################################
# GET THE VIABILITY INDEX   #############################################################
#########################################################################################

#####################################
####   count INDEX          #########
#####################################
#   - ‘current count’:
#       -‘yes’ (1),
#       -‘no information’ (0);

# no inforation
site_counts$count_index = 0

# name matched
site_counts$CURRCOUNT[site_counts$CURRCOUNT == 0] = NA
site_counts$count_index = ifelse(is.na(site_counts$CURRCOUNT),
                                 0,
                                 1)

summary(as.factor(site_counts$count_index))/ length(site_counts$count_index)
# 0         1
# 0.5902896 0.4097104
mean(site_counts$count_index)
# 0.4097104

#####################################
####   adjcount INDEX       #########
#####################################
#   - ‘adjusted count’:
#       - ‘yes’ (1),
#       - ‘no information’ (0);

# no information
site_counts$adjcount_index = 0

# name matched
site_counts$ADJSTCOUNT[site_counts$ADJSTCOUNT == 0] = NA
site_counts$adjcount_index = ifelse(is.na(site_counts$ADJSTCOUNT),
                                    site_counts$adjcount_index,
                                    1)

summary(as.factor(site_counts$adjcount_index))/ length(site_counts$adjcount_index)
# 0         1
# 0.6899489 0.3100511
mean(site_counts$adjcount_index)
# 0.3100511

#####################################
####  germination INDEX     #########
#####################################
#   - ‘germination test’:
#       - ‘recent test’ (1),
#       - ‘test older than 15 years’ (0.5),
#   - ‘no test information available’ (0).
as.Date(site_counts$LASTTEST, format =  "%d/%m/%Y")

# name a germination test
# site_counts$germination_index = ifelse(site_counts$DateStarted >= 2009,
#                                                            1,0.5)
site_counts$germination_index = ifelse(as.numeric(format(as.Date(site_counts$LASTTEST, format =  "%d/%m/%Y"),"%Y")) >= 2009,
                                       1,0.5)
# no information
site_counts$germination_index[is.na(site_counts$germination_index)] = 0

summary(as.factor(site_counts$germination_index))/ length(site_counts$germination_index)
# 0          0.5        1
# 0.86328790 0.01660988 0.12010221
mean(site_counts$germination_index)
# 0.1284072

############################################################
##     Combine for Viability index    ######################
############################################################
site_counts$viability_index = (site_counts$count_index +
                                 site_counts$adjcount_index +
                                 site_counts$germination_index)
site_counts$viability_index = site_counts$viability_index/3

mean(site_counts$viability_index) #  0.2827229


#########################################################################################
# GET THE GENETIC DIVERSITY INDEX   #####################################################
#########################################################################################

# The ‘Genetic diversity’ index was calculated differently depending
# if the collection was originated from cultivated or wild materials.

#####################################
####  cultivation INDEX     #########
#####################################

# For cultivated collections,
# 1 was given if relation to parent and if related to the isolation technique information are available,
# 0.5 if only one of them was available, and
# 0 when no information available.


# If it comes from a cultivated plant
site_counts$cultivation_index = ifelse(site_counts$CultivatedAll == TRUE,
                                       0.5,0)
# no information
site_counts$cultivation_index[is.na(site_counts$cultivation_index)] = 0

site_counts$cultivation_index = ifelse(!is.na(site_counts$Derived.From),
                                       site_counts$cultivation_index + 0.5,
                                       site_counts$cultivation_index + 0)


summary(as.factor(site_counts$cultivation_index))/ length(site_counts$cultivation_index)
# 0           0.5         1
# 0.926320273 0.067717206 0.005962521

mean(site_counts$cultivation_index)
# 0.03982112

#####################################
####      ex situ INDEX     #########
#####################################

# For ex situ collections directly sampled from wild populations,
# three variables were considered:
# ‘Plant sampled’ (number of plants which seeds were collected from),
# ‘Plant total’ (total number of plants in the population), and
# ‘Plant seed’ (total number of individuals with mature seeds in the population).
#
# Given that we aim at evaluating if 50 mature individuals were collected or, when not,
# if all individuals in the population or all mature individuals were collected;
# the scores weighted when the
# three variables are available (1), when
# ‘Plant sampled’ and ‘Plant total’ or ‘Plant seed’ (0.8),
# ‘Plant sampled’ (0.6),
# ‘Plant total’ and ‘Plant seed’ (0.4),
# ‘Plant total’ or ‘Plant seed’ (0.2),
# ‘no info’ (0).

data.frame(cbind(site_counts$NumberPlantsSampled,
                 site_counts$NumberPlantsLocated,
                 site_counts$exsitu_index))

site_counts$exsitu_index = ifelse((site_counts$PLANTTOTAL != "") &
                                    (site_counts$PLANTSAMP != "") &
                                    (!is.na(site_counts$PCSEED)),
                                  1,
                                  ifelse((((site_counts$PLANTTOTAL != "") &
                                             (site_counts$PLANTSAMP != "")) |
                                            (!is.na(site_counts$PCSEED))),
                                         0.8,
                                         ifelse((site_counts$PLANTSAMP != ""),
                                                0.6,
                                                ifelse(((site_counts$PLANTTOTAL != "") &
                                                          (!is.na(site_counts$PCSEED))),
                                                       0.4,
                                                       ifelse(((site_counts$PLANTTOTAL != "") |
                                                                 (!is.na(site_counts$PCSEED))),
                                                              0.2,
                                                              0
                                                       )
                                                )
                                         )
                                  )
)



# name a germination test
# site_counts$exsitu_index = ifelse(site_counts$NumberPlantsSampled == "",
#                                                       0,0.5)
# site_counts$exsitu_index = ifelse(site_counts$NumberPlantsLocated == "",
#                                                       site_counts$exsitu_index + 0,
#                                                       site_counts$exsitu_index + 0.5)
# site_counts$exsitu_index = ifelse(is.na(site_counts$exsitu_index),
#                                                       0,site_counts$exsitu_index)
summary(as.factor(site_counts$exsitu_index))/ length(site_counts$exsitu_index)
# 0           0.2         0.6         0.8         1
# 0.690800681 0.007240204 0.014480409 0.194633731 0.092844974


mean(site_counts$exsitu_index)
# 0.2586882


############################################################
##     Combine for genetic index    ######################
############################################################
# site_counts$genetic_index = (site_counts$cultivation_index +
#                                                    site_counts$exsitu_index)
# site_counts$genetic_index = site_counts$genetic_index/2
site_counts$genetic_index = ifelse(site_counts$CultivatedAll == T,
                                   site_counts$cultivation_index,
                                   site_counts$exsitu_index)
site_counts$genetic_index = ifelse(is.na(site_counts$genetic_index),
                                   site_counts$exsitu_index,
                                   site_counts$genetic_index)
mean(site_counts$genetic_index) #  0.2755301
# summary(site_counts$genetic_index) #  0.2743186




#####################################################
####      Get stats                         #########
#####################################################

mean(site_counts$information_index) #  0.9261428
mean(site_counts$viability_index)   #  0.2827229
mean(site_counts$genetic_index)     #  0.2743186

site_counts$total_index = ((site_counts$information_index + site_counts$viability_index +
                              site_counts$genetic_index)/3)

mean(site_counts$total_index)       #  0.4943948

######################################
####   Get Targets       #############
######################################

# Target 1 aims to preserve at least one valid collection with 1050 seeds
# for all CR plants. The term “valid collection” is used to define a collection
# that is representative of the genetic diversity of the sampled population
# (see more details below). Target 1 allows long term storage as well as the
# application of species recovery programmes in extreme situations (e.g.,
# after extinction in the wild). An adapted Target 1 is proposed when
# 1050 seeds is reached when accounting for all valid collections for a
# single species from at least 50 different plants stored ex situ .
# Target 2 is achieved when a CR species has valid ex situ
# collections which are representative of the natural distribution range of
# each species. For those species which were assigned the CR category due to
# criterium B of the IUCN (i.e., Extent of occurrence (EOO) < 100 km2 (B1),
# or with an Area of occupancy (AOO) < 10 km² (B2)), one collection would
# achieve this target.

site_counts$Target_1a = ifelse(site_counts$ADJSTCOUNT >= 1050,
                               TRUE,FALSE)
site_counts$Target_1a[is.na(site_counts$Target_1a)] = FALSE


# from at least 50 different plants stored ex situ
site_counts$Target_1b = ifelse((as.numeric(site_counts$PLANTSAMP) >= 50) | (site_counts$PLANTSAMP %in% c("11-100","100-1000","25-50",">100","200-500")),
                               TRUE,
                               FALSE)
site_counts$Target_1b[is.na(site_counts$Target_1b)] = FALSE

# Combine for target 1

site_counts$Target_1 = ifelse((site_counts$Target_1a & site_counts$Target_1b),
                              TRUE,FALSE)

cbind(site_counts$ADJSTCOUNT,
      site_counts$Target_1a,
      site_counts$PLANTSAMP,
      site_counts$Target_1b,
      site_counts$Target_1)

length(which(site_counts$Target_1))


##### TARGET 2   ###########################

# # find species listed based on their range criteria
# iucn_dict = data.frame(cbind(iucn_wcvp_matched$taxon_name,
#                              iucn_wcvp_matched$redlistCriteria,
#                              grepl("B", iucn_wcvp_matched$redlistCriteria, ignore.case=FALSE)))
#
#
#
# colnames(iucn_dict) = c("taxon_name","redlistCriteria","Target_2")
#
#
# site_counts = site_counts %>%
#   left_join(iucn_dict, by= c("taxon_name"),
#             relationship = "many-to-many")


##############################################
# New target 2
#############################################

# read in the TDWG data

TDWGS <- sf::st_read("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/level3/level3.shp")
TDWGS <- sf::st_make_valid(TDWGS)

# add a new column with the reformatted name
site_counts$NewCountryName = site_counts$COUNTRY

# rename the weird comments
site_counts$NewCountryName[site_counts$NewCountryName == "?"] = NA
site_counts$NewCountryName[site_counts$NewCountryName == "Saint Helena, Ascension, and Tristan da Cunha"] = "Saint Helena"
site_counts$NewCountryName[site_counts$NewCountryName == "St. Helena, Ascension & Tristan da Cunha"] = "Saint Helena"
site_counts$NewCountryName[site_counts$NewCountryName == "United States"] = "United States of America"
site_counts$NewCountryName[site_counts$NewCountryName == "Unknown"] = NA

# variable to fill in
site_counts$collections_native_country = NA
site_counts$collections_native_tdwg = NA
site_counts$collections_country = NA
site_counts$proportion_range_banked_species = NA
site_counts$proportion_range_banked = NA
site_counts$proportion_country_range_banked = NA
site_counts$proportion_native_countries_banked = NA
site_counts$collections_outside_range = NA
site_counts$collections = NA


# for every species
for (spp_i in unique(site_counts$taxon_name)){

  # spp_i = unique(site_counts$taxon_name)[1]
  # spp_i = "Chassalia laikomensis"
  # spp_i = site_counts$taxon_name[989]
  # spp_i = site_counts$taxon_name[577] #accross canada and us,
  # spp_i = "Oxalis corniculata"
  # spp_i = "Solanum viarum"

  # get tdwg that spp is founds in
  index = which(site_counts$taxon_name %in% spp_i)
  country_data = site_counts[index,] %>% left_join(wcvp_countries[which(wcvp_countries$introduced == 0),
                                                                  c("plant_name_id", "area_code_l3","area")],
                                                   by = c("wcvp_accepted_id" = "plant_name_id"),
                                                   relationship = "many-to-many")

  ##### TDWG DATA  #############

  # get banked tdwg
  sub <- TDWGS[which(TDWGS$LEVEL3_COD %in% unique(country_data$area_code_l3)),] #TDWGS %>% select(TDWGS$LEVEL3_CODLEVEL3_COD %in% TDWG_codes) #
  occs <- sf::st_as_sf(site_counts[index,c("ID","SPECIES", "LATDEC", "LONGDEC")], coords = c("LONGDEC","LATDEC"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occs <- sf::st_transform(occs, crs = sf:: st_crs(sub))
  occs <- cbind(occs, sf::st_coordinates(occs))
  banked_tdwg_list <- sub$LEVEL3_COD[which(rowSums(sf::st_contains(sub, occs, sparse = FALSE)) > 0)]

  # ggplot() +
  #   geom_sf(data = sub, aes(fill = LEVEL3_COD)) +
  #   geom_point(data = occs, aes(x = X, y = Y),
  #              size = 1, shape = 19, fill = "black")
  #


  ############ TDWG per Country ####################

  # get tdwg distribution for species
  country_dta <- bind_cols(tdwg_code = country_data$area_code_l3,
                           tdwg_name = country_data$area)
  country_dta <- country_dta[which(!duplicated (country_dta)),]

  #match tdwg to country
  country_dta <- country_dta %>% left_join(tdwg3_countries[,c("LEVEL3_COD", "COUNTRY")],
                                           by = c("tdwg_code" = "LEVEL3_COD"))

  # add collections per tdwg
  country_dta <- country_dta %>% left_join( bind_cols(LEVEL3_COD = sub$LEVEL3_COD,
                                                      collections_native_tdwg = rowSums(sf::st_contains(sub, occs, sparse = FALSE))),
                                           by = c("tdwg_code" = "LEVEL3_COD"))

  # add proportion of tdwgs banked
  country_dta$proportion_range_banked <- length(which(country_dta$collections_native_tdwg >0))/nrow(country_dta)

  # add proportion of tdwgs per country banked
  country_dta <- country_dta  %>% group_by(COUNTRY) %>%
    mutate(proportion_country_range_banked = length(which(collections_native_tdwg >0))/length(collections_native_tdwg)) %>% ungroup()

  # add proportion of countries banked
  country_dta <- country_dta  %>% group_by(COUNTRY) %>%
    mutate(proportion_native_countries_banked = length(any(collections_native_tdwg > 0))/length(unique(country_dta$COUNTRY))) %>% ungroup()


  # add the collections outside range
  country_dta$collections_outside_range <- length(index) -  sum(country_dta$collections_native_tdwg)
  # country_dta <- bind_rows(country_dta, data.frame(tdwg_code = "ZZZ",
  #                                                  tdwg_name = "Outside Native Range",
  #                                                  COUNTRY = "Outside Native Range",
  #                                                  collections_tdwg = as.numeric(collection_no) - sum(country_dta$collections_tdwg)))
  # # add collections per country
  country_dta <- country_dta  %>% group_by(COUNTRY) %>%
    mutate(collections_native_country = sum(collections_native_tdwg)) %>% ungroup()

  ### add back into the main dataset
  merge_data = site_counts[index,c("ID","ACCESSION","taxon_name", "NewCountryName")] %>% left_join(country_dta,
                                          by= c("NewCountryName" = "COUNTRY"))

  occs_2_tdwg <- data.frame(sf::st_join(occs, TDWGS, join = st_within))[,c("ID","SPECIES","X","Y","LEVEL3_NAM","LEVEL3_COD")]

  merge_data <- merge_data %>% left_join(occs_2_tdwg,by= "ID")

  # collections per tdgw
  merge_data <- merge_data %>% group_by(LEVEL3_COD) %>%
    mutate(collections_tdwg = n()) %>% ungroup()

  # collections per country
  merge_data <- merge_data %>% group_by(NewCountryName) %>%
    mutate(collections_country = n()) %>% ungroup()

  # remove the extra columns
  merge_data <- merge_data[,c("ID","ACCESSION","taxon_name","NewCountryName","LEVEL3_NAM","LEVEL3_COD",
                              "collections_tdwg", "collections_native_tdwg","proportion_range_banked","proportion_country_range_banked",
                              "proportion_native_countries_banked","collections_outside_range","collections_country","collections_native_country") ]

  #rename
  colnames(merge_data) <- c("ID","ACCESSION","taxon_name","NewCountryName","tdwg_name","tdwg_code",
                            "collections_tdwg", "collections_native_tdwg","proportion_range_banked","proportion_country_range_banked",
                            "proportion_native_countries_banked","collections_outside_range","collections_country","collections_native_country")



  merge_data$collections_native_country[which(is.na(merge_data$collections_native_country))] <- 0
  merge_data$collections_native_tdwg[which(is.na(merge_data$collections_native_tdwg))] <- 0
  merge_data$collections_country[which(is.na(merge_data$collections_country))] <- 0
  merge_data$proportion_range_banked_species <- merge_data$proportion_range_banked
  if (max(merge_data$proportion_range_banked_species, na.rm = T ) == -Inf){
    merge_data$proportion_range_banked_species <- 0
  }else{
    merge_data$proportion_range_banked_species <- max(merge_data$proportion_range_banked_species, na.rm = T )
  }
  merge_data$proportion_range_banked[which(is.na(merge_data$proportion_range_banked))] <- 0
  merge_data$proportion_country_range_banked[which(is.na(merge_data$proportion_country_range_banked))] <- 0
  merge_data$proportion_native_countries_banked[which(is.na(merge_data$proportion_native_countries_banked))] <- 0
  merge_data$collections_outside_range <- max(merge_data$collections_outside_range, na.rm=T)
  merge_data$collections <- length(index)

  site_counts$collections_native_country[index] = merge_data$collections_native_country
  site_counts$collections_native_tdwg[index] = merge_data$collections_native_tdwg
  site_counts$collections_country[index] = merge_data$collections_country
  site_counts$proportion_range_banked_species[index] = merge_data$proportion_range_banked_species
  site_counts$proportion_range_banked[index] = merge_data$proportion_range_banked
  site_counts$proportion_country_range_banked[index] = merge_data$proportion_country_range_banked
  site_counts$proportion_native_countries_banked[index] = merge_data$proportion_native_countries_banked
  site_counts$collections_outside_range[index] = merge_data$collections_outside_range
  site_counts$collections[index] = merge_data$collections
}

site_counts$Target_2a = ifelse(site_counts$prop_range_banked == 1,
                               TRUE,FALSE)
site_counts$Target_2a[is.na(site_counts$Target_2a)] = FALSE

site_counts$Target_2b  = ifelse(site_counts$collection_number >= 5,
                                TRUE,FALSE)

site_counts$Target_2 = ifelse(((site_counts$Target_2a & site_counts$Target_2b) & site_counts$Target_1b),
                              TRUE,FALSE)

# grepl("B1", iucn_wcvp_matched$redlistCriteria, ignore.case=FALSE)
# ifelse("B1" %in% iucn_wcvp_matched$redlistCriteria[1] ,1,0)
# grepl("B1", iucn_wcvp_matched$redlistCriteria, ignore.case=FALSE)
# unlist(lapply(iucn_wcvp_matched$redlistCriteria, function(x){ifelse("B1" %in% x,1,0)}))

######################################################################################################################
# Save
write.csv(site_counts, paste0(basepath,"iucn_brahms_indexes_targets.csv"))
######################################################################################################################



