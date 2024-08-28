
library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################
wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

# #load data from previous session
#
# #iucn redlist data
# iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))
#
# # the MSB data
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))
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
#
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
site_counts = read.csv(paste0(basepath,"iucn_brahms_wcvp_orthodoxy.csv"))#"iucn_brahms_wcvp_matched_full_name.csv"))#read.csv(paste0(basepath,"IUCN_seedsampling_info.csv"))
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

summary(as.factor(site_counts$geographic_index))

length(site_counts$geographic_index)
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

summary(as.factor(site_counts$taxonomy_index))

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

summary(as.factor(site_counts$year_index))


############################################################
##     Combine for information index    ######################
############################################################
site_counts$information_index = (site_counts$year_index +
                                   site_counts$taxonomy_index +
                                   site_counts$geographic_index)


site_counts$information_index = site_counts$information_index/3

mean(site_counts$information_index) #  0.9258117


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

summary(as.factor(site_counts$count_index))


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

summary(as.factor(site_counts$adjcount_index))

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

summary(as.factor(site_counts$germination_index))


############################################################
##     Combine for Viability index    ######################
############################################################
site_counts$viability_index = (site_counts$count_index +
                                 site_counts$adjcount_index +
                                 site_counts$germination_index)
site_counts$viability_index = site_counts$viability_index/3

mean(site_counts$viability_index) #  0.2844925


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


summary(as.factor(site_counts$cultivation_index))

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
summary(as.factor(site_counts$exsitu_index))

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
# summary(site_counts$genetic_index) #  0.2755301




#####################################################
####      Get stats                         #########
#####################################################

mean(site_counts$information_index) #  0.9258117
mean(site_counts$viability_index)   #  0.2844925
mean(site_counts$genetic_index)     #  0.2755301

site_counts$total_index = ((site_counts$information_index + site_counts$viability_index +
                              site_counts$genetic_index)/3)

mean(site_counts$total_index)       #  0.4953397

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



# add a new column with the reformatted name
site_counts$NewCountryName = site_counts$COUNTRY

# rename the weird comments
site_counts$NewCountryName[site_counts$NewCountryName == "?"] = NA
site_counts$NewCountryName[site_counts$NewCountryName == "Saint Helena, Ascension, and Tristan da Cunha"] = "Saint Helena"
site_counts$NewCountryName[site_counts$NewCountryName == "St. Helena, Ascension & Tristan da Cunha"] = "Saint Helena"
site_counts$NewCountryName[site_counts$NewCountryName == "United States"] = "United States of America"
site_counts$NewCountryName[site_counts$NewCountryName == "Unknown"] = NA

# variable to fill in
site_counts$prop_range_banked = NA

# for every species
for (spp_i in unique(site_counts$taxon_name)){

  # get tdwg that spp is founds in
  index = which(site_counts$taxon_name %in% spp_i)
  country_data = site_counts[index,] %>% left_join(wcvp_countries[,c("plant_name_id", "area_code_l3","area")],
                           by = c("wcvp_accepted_id" = "plant_name_id"),
                           relationship = "many-to-many")

  # get code
  TDWG_codes = unique(country_data$area_code_l3)

  # get countries from tdwg
  country_list = unique(tdwg3_countries$COUNTRY[which(tdwg3_countries$LEVEL3_COD %in% TDWG_codes)])

  # get banked country
  banked_country_list =  unique(country_data$NewCountryName)
  banked_country_list = banked_country_list[!is.na(banked_country_list)]

  # calculate proportion of range banked
  prop_range_banked =  length(which(country_list %in% banked_country_list))/length(country_list)
  site_counts$prop_range_banked[index] = prop_range_banked

}

site_counts$Target_2a = ifelse(site_counts$prop_range_banked == 1,
                               TRUE,FALSE)
site_counts$Target_2a[is.na(site_counts$Target_2a)] = FALSE

site_counts$Target_2 = ifelse((site_counts$Target_2a & site_counts$Target_1b),
                              TRUE,FALSE)

# grepl("B1", iucn_wcvp_matched$redlistCriteria, ignore.case=FALSE)
# ifelse("B1" %in% iucn_wcvp_matched$redlistCriteria[1] ,1,0)
# grepl("B1", iucn_wcvp_matched$redlistCriteria, ignore.case=FALSE)
# unlist(lapply(iucn_wcvp_matched$redlistCriteria, function(x){ifelse("B1" %in% x,1,0)}))

######################################################################################################################
# Save
write.csv(site_counts, paste0(basepath,"iucn_brahms_indexes_targets.csv"))
######################################################################################################################



