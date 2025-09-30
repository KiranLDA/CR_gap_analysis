# check 700
library(shiny)
library(dplyr)
library(rWCVP)
library(sf)
library(geodata)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################

wcvp <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
# wcvp$plant_name_id <- as.character(wcvp$plant_name_id)

wcvp_countries <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

# read in the TDWG data
TDWGS <- sf::st_read("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/level3/level3.shp")
TDWGS <- sf::st_make_valid(TDWGS)
TDWGS <- sf::st_transform(TDWGS, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


world <- sf::st_as_sf(world(path="."))
world <- sf::st_make_valid(world)
world <- sf::st_transform(world, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


# load data from previous session ##########################################

# iucn redlist data
# iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"))

# seed_data_to_add_1 <- read.csv(paste0(basepath, "seedcounts_2024-05-28.csv"))
# seed_data_to_add_1 = seed_data_to_add_1[!(duplicated(seed_data_to_add_1$AccessionNumber)),]


# get the tdwg to country mapping
tdwg3_countries <- read.csv(paste0(basepath, "country_tdwg3_mapping_multicountry.csv"), encoding = "UTF-8")
tdwg3_countries$ISO_code[is.na(tdwg3_countries$ISO_code)] ="NA"

# ####################################################################

# get the brahms data and see which IUCN ones are in the bank
brahms_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_wcvp_matched_full_name_infra.csv"), encoding = "UTF-8")
iucn_orthodoxy = read.csv(paste0(basepath,"revision_1/spp_banked_recalcitrant.csv"), encoding = "UTF-8")

brahms_iucn = brahms_wcvp_matched[which(brahms_wcvp_matched$taxon_name  %in% iucn_orthodoxy$taxon_name ),]
nrow(brahms_iucn)

# prep the data
site_counts = brahms_iucn
site_counts$LATDEC[which(site_counts$LATDEC == 0.00)] <- NA
site_counts$LAT[which(site_counts$LAT == 0)] <- NA
site_counts$LONGDEC[which(site_counts$LONGDEC == 0.00)] <- NA
site_counts$LONG[which(site_counts$LONG == 0)] <- NA


# Combine the earthcape and brahms online (data warehouse) data
cultivated <- read.csv(paste0(basepath, "cultivated_2024-05-28.csv"), encoding = "UTF-8")
cultivated = cultivated[!(duplicated(cultivated$AccessionNumber)),]

cultivated2 = read.csv(paste0(basepath, "ICMS_cultivated_26_06_24.csv"), encoding = "UTF-8")
cultivated2$AccessionNumber = sapply(1:nrow(cultivated2),
                                     function(x){as.character(as.numeric(strsplit(cultivated2$Catalogue.Number[x],
                                                                                  "K:MSB-")[[1]][2]))})
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

#######################################################################################
#                     INDEXES
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
#  GET THE INFORMATION INDEX #############################################################
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
site_counts$geographic_index = ifelse(is.na(site_counts$LAT),
                                      site_counts$geographic_index,
                                      1)

summary(as.factor(site_counts$geographic_index))/length(site_counts$geographic_index)
# 0          0.5        0.75        1
# 0.02327386 0.10628394 0.36190846 0.50853375

mean(site_counts$geographic_index)
# 0.8331071


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
# 0           1
# 0.008533747 0.991466253

mean(site_counts$taxonomy_index)
# 0.9982993


#####################################
####    year INDEX          #########
#####################################
# iii) ‘collection year’:
#     - ‘yes’ (1),
#     - ‘no information’ (0).

# no inforation
site_counts$year_index = 0

# convert to date format
# or should it be DONORDATE?
site_counts$DateCollected <- paste0(site_counts$DAY,"/",
                                    site_counts$MONTH,"/",
                                    site_counts$YEAR)
site_counts$DateCollected[which(site_counts$DateCollected == "0/0/0")] = NA


site_counts$DateCollected <- as.Date(site_counts$DateCollected , format =  "%d/%m/%Y")

head(data.frame(collected = site_counts$DateCollected,
                donated=site_counts$DONORDATE,
                banked=site_counts$BANKDATE,
                entry=site_counts$ENTRYDATE))


site_counts$year_index = ifelse(is.na(site_counts$DateCollected), site_counts$year_index, 1)

summary(as.factor(site_counts$year_index))/length(site_counts$year_index)
# 0         1
# 0.3871218 0.6128782

mean(site_counts$year_index)
# 0.6128782

############################################################
##     Combine for information index    ######################
############################################################
site_counts$information_index = (site_counts$year_index +
                                   site_counts$taxonomy_index +
                                   site_counts$geographic_index)


site_counts$information_index = site_counts$information_index/3

mean(site_counts$information_index) # 0.8124838


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
site_counts$count_index = ifelse(is.na(site_counts$CURRCOUNT), 0, 1)

summary(as.factor(site_counts$count_index))/ length(site_counts$count_index)
# 0         1
# 0.5430566 0.4569434

mean(site_counts$count_index)
# 0.4569434

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
# 0.6547711 0.3452289

mean(site_counts$adjcount_index)
# 0.3452289

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
# 0.84328937 0.01706749 0.13964313

mean(site_counts$germination_index)
# 0.1481769

############################################################
##     Combine for Viability index    ######################
############################################################
site_counts$viability_index = (site_counts$count_index +
                                 site_counts$adjcount_index +
                                 site_counts$germination_index)
site_counts$viability_index = site_counts$viability_index/3

mean(site_counts$viability_index) #  0.316783


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
# 0           0.5          1
# 0.924359969 0.070209465 0.005430566

mean(site_counts$cultivation_index)
# 0.0405353

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

# data.frame(cbind(site_counts$NumberPlantsSampled,
#                  site_counts$NumberPlantsLocated,
#                  site_counts$exsitu_index))

site_counts$exsitu_index = ifelse((site_counts$PLANTTOTAL != "") &
                                    (site_counts$PLANTSAMP != "") &
                                    # (!is.na(site_counts$PCSEED)),
                                    (site_counts$PCSEED != ""),
                                  1,
                                  ifelse((((site_counts$PLANTTOTAL != "") &
                                             (site_counts$PLANTSAMP != "")) |
                                            # (!is.na(site_counts$PCSEED))),
                                            (site_counts$PCSEED  != "")),
                                         0.8,
                                         ifelse((site_counts$PLANTSAMP != ""),
                                                0.6,
                                                ifelse(((site_counts$PLANTTOTAL != "") &
                                                          # (!is.na(site_counts$PCSEED))),
                                                          (site_counts$PCSEED  != "")),

                                                       0.4,
                                                       ifelse(((site_counts$PLANTTOTAL != "") |
                                                                 # (!is.na(site_counts$PCSEED))),
                                                                 (site_counts$PCSEED  != "")),
                                                              0.2,
                                                              0
                                                       )
                                                )
                                         )
                                  )
)


summary(as.factor(site_counts$exsitu_index))/ length(site_counts$exsitu_index)
# 0           0.2         0.6         0.8         1
# 0.637315749 0.006594259 0.015515904 0.236617533 0.103956555


mean(site_counts$exsitu_index)
# 0.303879


############################################################
##     Combine for genetic index    ######################
############################################################

site_counts$genetic_index = ifelse(site_counts$CultivatedAll == T,
                                   site_counts$cultivation_index,
                                   site_counts$exsitu_index)
site_counts$genetic_index = ifelse(is.na(site_counts$genetic_index),
                                   site_counts$exsitu_index,
                                   site_counts$genetic_index)
mean(site_counts$genetic_index)
# 0.3164856




#####################################################
####      Get stats                         #########
#####################################################

mean(site_counts$information_index) #  0.8124838
mean(site_counts$viability_index)   #  0.316783
mean(site_counts$genetic_index)     #  0.3164856

site_counts$total_index = ((site_counts$information_index + site_counts$viability_index +
                              site_counts$genetic_index)/3)

mean(site_counts$total_index)       #  0.4819175


############################################################################
############################################################################
####   Get Targets                                             #############
############################################################################
############################################################################

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

############################################################################
#####            TARGET 1                        ###########################
############################################################################

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

length(which(site_counts$Target_1)) #49

############################################################################
#####            TARGET 2                        ###########################
############################################################################


# add a new column with the reformatted name
site_counts$NewCountryName = site_counts$COUNTRY

# rename the weird comments
site_counts$NewCountryName[site_counts$NewCountryName == "?"] = NA
site_counts$NewCountryName[site_counts$NewCountryName == "Saint Helena, Ascension, and Tristan da Cunha"] = "Saint Helena"
site_counts$NewCountryName[site_counts$NewCountryName == "St. Helena, Ascension & Tristan da Cunha"] = "Saint Helena"
site_counts$NewCountryName[site_counts$NewCountryName == "United States"] = "United States of America"
site_counts$NewCountryName[site_counts$NewCountryName == "Unknown"] = NA
# View(data.frame(new=site_counts$NewCountryName, old=site_counts$COUNTRY))

site_counts$wcvp_accepted_id = as.character(site_counts$wcvp_accepted_id)
wcvp_countries$plant_name_id = as.character(wcvp_countries$plant_name_id)

site_counts$id_wfo_wcvp = NA
site_counts$id_wfo_wcvp = as.character(site_counts$id_wfo_wcvp)

# What proportion wont' have a distribution?
length(which(site_counts$taxonomic_backbone %in% c("WFO")))
# 9
length(which(site_counts$taxonomic_backbone %in% c("WFO", "WCVP")))
# 2556
length(which(site_counts$taxonomic_backbone %in% c("WFO")))/ length(which(site_counts$taxonomic_backbone %in% c("WFO", "WCVP")))
#  0.003521127


### setup data for target 2

site_counts$collections <- NA                               # total numeber of collections

site_counts$collections_per_tdwg <- NA                      # collections that are inside each tdwg (native or not)
site_counts$collections_per_native_tdwg <- NA               # collections that are inside each native tdwg
site_counts$collections_all_native_tdwg <- NA               # collections that are inside native tdwg
site_counts$collections_outside_tdwg <- NA                  # collections that are outside native tdwg

site_counts$collections_per_country <- NA                   # collections that are inside each country (native or not)
site_counts$collections_per_native_country <- NA            # collections that are inside each native country
site_counts$collections_all_native_country <- NA            # collections that are inside native countries
site_counts$collections_outside_country <- NA               # collections that are outside native countries

site_counts$proportion_native_tdwg <- NA                    # porportion of native tdwgs banked
site_counts$proportion_outside_tdwg <- NA                   # porportion of not-native tdwgs banked
site_counts$proportion_native_country <- NA                 # porportion of native countries banked
site_counts$proportion_outside_country <- NA                # porportion of not-native countries banked

site_counts$proportion_collections_native_tdwg <- NA        # porportion of native tdwgs banked
site_counts$proportion_collections_outside_tdwg <- NA       # porportion of not-native tdwgs banked
site_counts$proportion_collections_native_country <- NA     # porportion of native countries banked
site_counts$proportion_collections_outside_country <- NA    # porportion of not-native countries banked


# for every species
for (spp_i in unique(site_counts$taxon_name)){
  print(spp_i)
  # spp_i = "Euphorbia origanoides"
  # spp_i = "Atocion compactum"
  # spp_i = "Grevillea curviloba subsp. incurva"
  # spp_i = unique(site_counts$taxon_name)[1]
  # spp_i = "Chassalia laikomensis"
  # spp_i = site_counts$taxon_name[989]
  # spp_i = site_counts$taxon_name[577] #accross canada and us,
  # spp_i = "Oxalis corniculata"
  # spp_i = "Solanum viarum"

  # get data rows for subsetting that species
  index = which(site_counts$taxon_name %in% spp_i)

  temp <- site_counts[index,c("ID","ACCESSION","taxon_name","wcvp_accepted_id","COUNTRY","NewCountryName","LATDEC","LONGDEC")]

  temp$collections <- length(unique(temp$ACCESSION[!is.na(temp$ACCESSION)]))

  temp$tdwg_code_occs <- NA                          # use lat long to get tdwg code
  temp$tdwg_name_occs <- NA                          # use lat long to get tdwg name
  temp$country_name_occs <- NA                       # use lat long to get  country name
  temp$tdwg_code_from_country <- NA                  # use country name in MSB to get tdwg code
  temp$tdwg_name_from_country <- NA                  # use country name in MSB to get tdwg name
  # temp$tdwg_code_banked <- NA
  # temp$tdwg_name_banked <- NA
  # temp$country_name_banked <- NA
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get tdwg and country from GPS locations
  occs <- sf::st_as_sf(temp[,c("ID","taxon_name", "LATDEC", "LONGDEC")],
                       coords = c("LONGDEC","LATDEC"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                       na.fail = F)
  # occs <- sf::st_transform(occs, crs = sf:: st_crs(sub))
  occs <- cbind(occs, sf::st_coordinates(occs))

  for(rowi in which(!is.na(temp$LATDEC))){
    idx <- which(sf::st_contains(TDWGS, occs[rowi,], sparse = FALSE))
    idz <- which(sf::st_contains(world, occs[rowi,],sparse = FALSE))
    temp$tdwg_code_occs[rowi] = ifelse(length(idx>0),TDWGS$LEVEL3_COD[idx], NA)
    temp$tdwg_name_occs[rowi] = ifelse(length(idx>0),TDWGS$LEVEL3_NAM[idx], NA)
    temp$country_name_occs[rowi] = ifelse(length(idz>0), world$NAME_0[idz], NA)
  }
  temp$tdwg_code_banked <- temp$tdwg_code_occs
  temp$tdwg_name_banked <- temp$tdwg_name_occs
  temp$country_name_banked <- temp$country_name_occs

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get tdwg and country from names
  for(rowi in which(is.na(temp$tdwg_code_occs))){
    idx <- which(tdwg3_countries$COUNTRY %in% temp$NewCountryName[rowi])
    temp$tdwg_code_from_country[rowi] <- ifelse(length(idx)==1,
                                                tdwg3_countries$LEVEL3_COD[idx],
                                                "Unknown_many")
    temp$tdwg_name_from_country[rowi] <- ifelse(length(idx)==1,
                                                tdwg3_countries$LEVEL3_NAM[idx],
                                                "Unknown_many")

  }
  temp$tdwg_code_banked <- ifelse(!is.na(temp$tdwg_code_occs),
                                  temp$tdwg_code_occs,
                                  temp$tdwg_code_from_country)

  temp$tdwg_name_banked <- ifelse(!is.na(temp$tdwg_name_occs),
                                  temp$tdwg_name_occs,
                                  temp$tdwg_name_from_country)

  temp$country_name_banked <- ifelse(!is.na(temp$country_name_occs),
                                     temp$country_name_occs,
                                     temp$NewCountryName)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # list of tdwg codes and countries for each species
  tdwg_country_info <- data.frame(wcvp_accepted_id = unique(temp$wcvp_accepted_id)) %>%
    left_join(wcvp_countries[which(wcvp_countries$introduced == 0),
                             c("plant_name_id", "area_code_l3","area")],
              by = c("wcvp_accepted_id" = "plant_name_id"),
              relationship = "many-to-many")

  tdwg_country_info <- tdwg_country_info %>%  left_join(tdwg3_countries[,c("LEVEL3_COD", "COUNTRY")],
                                                        by = c("area_code_l3" = "LEVEL3_COD"))

  # see if native tdwg
  temp$native_tdwg <- ifelse(temp$tdwg_code_banked %in% tdwg_country_info$area_code_l3, TRUE, FALSE)
  temp$native_tdwg <- ifelse(temp$tdwg_code_banked %in% c("Unknown_many"), NA, temp$native_tdwg)

  # see if native country
  temp$native_country <- ifelse(temp$country_name_banked %in% tdwg_country_info$COUNTRY, TRUE, FALSE)
  temp$native_country <- ifelse(is.na(temp$country_name_banked) , NA, temp$native_country)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collections per tdwg and country

  # collections across all native tdgw
  temp <- temp %>%
    group_by(native_tdwg) %>%
    mutate(collections_all_native_tdwg = n(),
           collections_outside_tdwg = n()) %>% ungroup()

  temp$collections_all_native_tdwg = ifelse(temp$native_tdwg == TRUE, temp$collections_all_native_tdwg, 0)
  temp$collections_outside_tdwg = ifelse(temp$native_tdwg == FALSE, temp$collections_outside_tdwg, 0)


  # collections across all native country
  temp <- temp %>%
    group_by(native_country) %>%
    mutate(collections_all_native_country = n(),
           collections_outside_country = n()) %>% ungroup()

  temp$collections_all_native_country = ifelse(temp$native_country == TRUE, temp$collections_all_native_country, 0)
  temp$collections_outside_country = ifelse(temp$native_country == FALSE, temp$collections_outside_country, 0)

  # collections per native tdgw
  temp <- temp %>% group_by(tdwg_code_banked) %>% mutate(collections_per_tdwg = n()) %>% ungroup()
  temp$collections_per_native_tdwg = ifelse(temp$native_tdwg == TRUE, temp$collections_per_tdwg, 0)

  # collections across all native country
  temp <- temp %>% group_by(country_name_banked) %>% mutate(collections_per_country = n()) %>% ungroup()
  temp$collections_per_native_country = ifelse(temp$native_country == TRUE, temp$collections_per_country, 0)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # proportion per tdgw
  all_tdwgs <- unique(tdwg_country_info$area_code_l3)
  all_countries <- unique(tdwg_country_info$COUNTRY)

  # proportion of native or non-native areas banked

  # porportion of native tdwgs banked
  temp$proportion_native_tdwg <- length(which(unique(temp$tdwg_code_banked[which(temp$native_tdwg == TRUE)]) %in%
                                                all_tdwgs))/length(all_tdwgs)
  # porportion of not-native tdwgs banked
  temp$proportion_outside_tdwg <- length(which(unique(temp$tdwg_code_banked[which(temp$native_tdwg == FALSE)]) %in%
                                                 all_tdwgs))/length(all_tdwgs)
  # porportion of native countries banked
  temp$proportion_native_country <- length(which(unique(temp$country_name_banked[which(temp$native_country == TRUE)]) %in%
                                                   all_countries))/length(all_countries)
  # porportion of not-native countries banked
  temp$proportion_outside_country <- length(which(unique(temp$country_name_banked[which(temp$native_country == FALSE)]) %in%
                                                    all_countries))/length(all_countries)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # proportion of collections banked in native or non-native range

  # porportion of native tdwgs banked
  temp$proportion_collections_native_tdwg <- temp$collections_all_native_tdwg/temp$collections

  # porportion of not-native tdwgs banked
  temp$proportion_collections_outside_tdwg <- temp$collections_outside_tdwg/temp$collections

  # porportion of native countries banked
  temp$proportion_collections_native_country <- temp$collections_all_native_country/temp$collections

  # porportion of not-native countries banked
  temp$proportion_collections_outside_country <- temp$collections_outside_country/temp$collections

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # combine with dataset

  site_counts$collections[index] <- temp$collections # total numeber of collections
  #~
  site_counts$collections_per_tdwg[index] <- temp$collections_per_tdwg # collections that are inside each tdwg (native or not)
  #~
  site_counts$collections_per_native_tdwg[index] <- temp$collections_per_native_tdwg # collections that are inside each native tdwg
  site_counts$collections_all_native_tdwg[index] <- temp$collections_all_native_tdwg # collections that are inside native tdwg
  site_counts$collections_outside_tdwg[index] <- temp$collections_outside_tdwg # collections that are outside native tdwg
  #~
  site_counts$collections_per_country[index] <- temp$collections_per_country # collections that are inside each country (native or not)
  #~
  site_counts$collections_per_native_country[index] <- temp$collections_per_native_country # collections that are inside each native country

  site_counts$collections_all_native_country[index] <- temp$collections_all_native_country # collections that are inside native countries

  site_counts$collections_outside_country[index] <- temp$collections_outside_country # collections that are outside native countries
  site_counts$proportion_native_tdwg[index] <- temp$proportion_native_tdwg # porportion of native tdwgs banked
  site_counts$proportion_outside_tdwg[index] <- temp$proportion_outside_tdwg # porportion of not-native tdwgs banked
  site_counts$proportion_native_country[index] <- temp$proportion_native_country # porportion of native countries banked
  site_counts$proportion_outside_country[index] <- temp$proportion_outside_country # porportion of not-native countries banked
  site_counts$proportion_collections_native_tdwg[index] <- temp$proportion_collections_native_tdwg   # porportion of native tdwgs banked
  site_counts$proportion_collections_outside_tdwg[index] <- temp$proportion_collections_outside_tdwg   # porportion of not-native tdwgs banked
  site_counts$proportion_collections_native_country[index] <- temp$proportion_collections_native_country   # porportion of native countries banked
  site_counts$proportion_collections_outside_country[index] <- temp$proportion_collections_outside_country  # porportion of not-native countries banked

}


### Country range banked
# all of native countries banked and with over 5 collections in these
site_counts$Target_2a = ifelse(site_counts$proportion_native_country == 1, # but could also be proportion_native_tdwg?
                               TRUE,FALSE)
site_counts$Target_2a[is.na(site_counts$Target_2a)] = FALSE

site_counts$Target_2b  = ifelse(site_counts$collections_all_native_country >= 5, #collections >= 5,
                                TRUE,FALSE)

site_counts$Target_2 = ifelse(((site_counts$Target_2a & site_counts$Target_2b) & site_counts$Target_1b),
                              TRUE,FALSE)

length(which(site_counts$Target_2)) #35



######################################################################################################################
# Save
write.csv(site_counts, paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"), row.names = F, fileEncoding = "UTF-8")
######################################################################################################################



