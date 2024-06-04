


library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################

#load data from previous session

#iucn redlist data
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))

# the MSB data
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched_full_name.csv"))

# the exceptional species )recalcitrant
exceptional <- read.csv(paste0(basepath, "pence_appendix1.csv"))
exceptional_wcvp_matched = read.csv(paste0(basepath,"exceptional_unique_wcvp_matched.csv"))

# species with their IUCN predictions
iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "iucn_predictions_wcvp_matched.csv"))
iucn_CR_predictions = iucn_predictions[which(iucn_predictions$category == "CR"),]
iucn_CR_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]# keep only the CR ones
iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in% iucn_wcvp_matched$taxon_name)),]# keep only the ones that aren't already in IUCN
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$wcvp_ipni_id %in%
                                                      iucn_wcvp_matched$wcvp_ipni_id))]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in%
                                                      iucn_wcvp_matched$taxon_name))]

# wcvp data
wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

# combined IUCN data with banked and unbanked and CR categories
iucn_banked_recalitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))

seed_data_to_add_1 <- read.csv(paste0(basepath, "seedcounts_2024-05-28.csv"))
seed_data_to_add_1 = seed_data_to_add_1[!(duplicated(seed_data_to_add_1$AccessionNumber)),]

seed_data_to_add_2 <- read.csv(paste0(basepath, "seedcounts2_2024-05-28.csv"))
seed_data_to_add_2 = seed_data_to_add_2[!(duplicated(seed_data_to_add_2$AccessionNumber)),]

cultivated <- read.csv(paste0(basepath, "cultivated_2024-05-28.csv"))
cultivated = cultivated[!(duplicated(cultivated$AccessionNumber)),]


# germination test data
germination <- read.csv(paste0(basepath, "germination_test_2024-05-28.csv"))

# Banked IUCN
iucn_banked <- iucn_banked_recalitrance[which(iucn_banked_recalitrance$category == "banked"),]
iucn_banked$taxon_name

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


brahms_germination_wcvp_matched = read.csv(paste0(basepath, "brahms_germination_wcvp_matched.csv"))


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
brahms_germination_wcvp_matched$geographic_index = 0

# country
brahms_germination_wcvp_matched$geographic_index = ifelse(is.na(brahms_germination_wcvp_matched$CountryName),
                                                          brahms_germination_wcvp_matched$geographic_index,
                                                          0.5)
# area (locality)
brahms_germination_wcvp_matched$geographic_index = ifelse(is.na(brahms_germination_wcvp_matched$LocalityText),
                                                          brahms_germination_wcvp_matched$geographic_index,
                                                          0.75)
# coordinates
brahms_germination_wcvp_matched$geographic_index = ifelse(is.na(brahms_germination_wcvp_matched$Latitude),
                                                          brahms_germination_wcvp_matched$geographic_index,
                                                          1)

summary(as.factor(brahms_germination_wcvp_matched$geographic_index))

#####################################
####    taxonomy INDEX      #########
#####################################
# ii) ‘verified taxonomy’:
#      - ‘yes’ (1),
#      - ‘not verified’ or ‘no information’ (0);

# no inforation
brahms_germination_wcvp_matched$taxonomy_index = 0

# name matched
brahms_germination_wcvp_matched$taxonomy_index = ifelse(is.na(brahms_germination_wcvp_matched$taxonomic_backbone),
                                                          brahms_germination_wcvp_matched$taxonomy_index,
                                                          1)

summary(as.factor(brahms_germination_wcvp_matched$taxonomy_index))

#####################################
####    year INDEX          #########
#####################################
# iii) ‘collection year’:
#     - ‘yes’ (1),
#     - ‘no information’ (0).

# no inforation
brahms_germination_wcvp_matched$year_index = 0

# name matched
brahms_germination_wcvp_matched$year_index = ifelse(is.na(brahms_germination_wcvp_matched$DateCollected),
                                                        brahms_germination_wcvp_matched$year_index,
                                                        1)

summary(as.factor(brahms_germination_wcvp_matched$year_index))

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
brahms_germination_wcvp_matched$count_index = 0

# name matched
brahms_germination_wcvp_matched$CurrentSeedQuantity[brahms_germination_wcvp_matched$CurrentSeedQuantity == 0] = NA
brahms_germination_wcvp_matched$count_index = ifelse(is.na(brahms_germination_wcvp_matched$CurrentSeedQuantity),
                                                    brahms_germination_wcvp_matched$count_index,
                                                    1)

summary(as.factor(brahms_germination_wcvp_matched$count_index))


#####################################
####   adjcount INDEX       #########
#####################################
#   - ‘adjusted count’:
#       - ‘yes’ (1),
#       - ‘no information’ (0);

# no information
brahms_germination_wcvp_matched$adjcount_index = 0

# name matched
brahms_germination_wcvp_matched$AdjustedSeedQuantity[brahms_germination_wcvp_matched$AdjustedSeedQuantity == 0] = NA
brahms_germination_wcvp_matched$adjcount_index = ifelse(is.na(brahms_germination_wcvp_matched$AdjustedSeedQuantity),
                                                     brahms_germination_wcvp_matched$adjcount_index,
                                                     1)

summary(as.factor(brahms_germination_wcvp_matched$adjcount_index))

#####################################
####  germination INDEX     #########
#####################################
#   - ‘germination test’:
#       - ‘recent test’ (1),
#       - ‘test older than 15 years’ (0.5),
#   - ‘no test information available’ (0).

# name a germination test
brahms_germination_wcvp_matched$germination_index = ifelse(brahms_germination_wcvp_matched$DateStarted >= 2009,
                                                           1,0.5)
# no information
brahms_germination_wcvp_matched$germination_index[is.na(brahms_germination_wcvp_matched$germination_index)] = 0

summary(as.factor(brahms_germination_wcvp_matched$germination_index))


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


# name a germination test
brahms_germination_wcvp_matched$cultivation_index = ifelse(brahms_germination_wcvp_matched$CultivatedFlag >= "True",
                                                           1,0)
# no information
brahms_germination_wcvp_matched$germination_index[is.na(brahms_germination_wcvp_matched$germination_index)] = 0

summary(as.factor(brahms_germination_wcvp_matched$germination_index))

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

data.frame(cbind(brahms_germination_wcvp_matched$NumberPlantsSampled,
      brahms_germination_wcvp_matched$NumberPlantsLocated,
      brahms_germination_wcvp_matched$exsitu_index))


# name a germination test
brahms_germination_wcvp_matched$exsitu_index = ifelse(brahms_germination_wcvp_matched$NumberPlantsSampled == "",
                                                           0,0.5)
brahms_germination_wcvp_matched$exsitu_index = ifelse(brahms_germination_wcvp_matched$NumberPlantsLocated == "",
                                                           brahms_germination_wcvp_matched$exsitu_index + 0,
                                                           brahms_germination_wcvp_matched$exsitu_index + 0.5)

summary(as.factor(brahms_germination_wcvp_matched$exsitu_index))







