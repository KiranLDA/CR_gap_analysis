library(shiny)
library(dplyr)
library(V.PhyloMaker2)
library(geodata)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

wcvp <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

wcvp_countries <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")


###### Find the CR species in the dataset ##################################################################

world <- sf::st_as_sf(world(path="."))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load data from previous session
iucn <- read.table(paste0(basepath, "revision_1/redlist_2025-1_downloaded_17_09_2025/assessments.csv" ),
                   sep = ",", quote = "\"",
                   dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"), encoding = "UTF-8")
iucn_wcvp_matched$higher[which(iucn_wcvp_matched$higher == "A")] = "Angiosperms"

# predictions
iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"), encoding = "UTF-8")
iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_predictions_wcvp_matched.csv"), encoding = "UTF-8")

# iucn data in the bank with calculated targets
indexes = read.csv(paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"), encoding = "UTF-8")
CR_msbp <- read.csv(paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"), encoding = "UTF-8")

# seedbank data
brahms = read.csv(paste0(basepath,"revision_1/brahms_cleaned.csv"), encoding  = "UTF-8")
brahms_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_wcvp_matched_full_name_infra.csv"), encoding = "UTF-8")

# Access and benefits sharing data
abs <- read.csv(paste0(basepath,"ABSCH-Country-List_03_07_24.csv"), encoding = "UTF-8")
permissions = read.csv(paste0(basepath,"ABSCH-Country-List_03_07_24.csv"))

# make sure only consider predicted that aren't already CR
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_unique_wcvp_matched_full_name_infra.csv"), encoding = "UTF-8")

exceptional <- read.csv(paste0(basepath, "revision_1/list_exceptional_status_18_09_2025.csv"), encoding = "UTF-8")
exceptional_wcvp_matched = read.csv(paste0(basepath,"revision_1/exceptional_unique_wcvp_matched.csv"), encoding = "UTF-8")

# iucn species and their categories
iucn_banked_recalcitrance <- read.csv(paste0(basepath, "revision_1/spp_banked_recalcitrant.csv"), encoding = "UTF-8")

# Geographic information
country_stats = read.csv(paste0(basepath, "revision_1/country_stats.csv"))
iucn_wcvp_matched_countries_tdwg3 <- read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched_countries_tdwg3_maps.csv"),encoding = "UTF-8")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Prep the datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# convert to dates
indexes$BESTEVERD[which(indexes$BESTEVERD == "  /  /    ")] <- NA
indexes$BESTEVERD <- as.Date(indexes$BESTEVERD , format =  "%d/%m/%Y")

indexes$FIRSTTEST[which(indexes$FIRSTTEST == "  /  /    ")] <- NA
indexes$FIRSTTEST <- as.Date(indexes$FIRSTTEST , format =  "%d/%m/%Y")

indexes$LASTTEST[which(indexes$LASTTEST == "  /  /    ")] <- NA
indexes$LASTTEST <- as.Date(indexes$LASTTEST , format =  "%d/%m/%Y")

indexes$XRAYDATE[which(indexes$XRAYDATE == "  /  /    ")] <- NA
indexes$XRAYDATE <- as.Date(indexes$XRAYDATE , format =  "%d/%m/%Y")

# get the data
indexes$plants_sampled = as.numeric(indexes$PLANTSAMP)
indexes$plants_sampled[indexes$PLANTSAMP == "11-100"] = 50
indexes$plants_sampled[indexes$PLANTSAMP == "100-1000"] = 100
indexes$plants_sampled[indexes$PLANTSAMP == "25-50"] = 50
indexes$plants_sampled[indexes$PLANTSAMP == ">100"] = 100
indexes$plants_sampled[indexes$PLANTSAMP == "200-500"] = 200
indexes$plants_sampled[indexes$PLANTSAMP == "12+"] = 12
indexes$plants_sampled[indexes$PLANTSAMP == ">3"] = 3
indexes$plants_sampled[indexes$PLANTSAMP == "3 clumps"] = 3
indexes$plants_sampled[indexes$PLANTSAMP == ">30"] = 30
indexes$plants_sampled[indexes$PLANTSAMP == "<10"] = 10
indexes$plants_sampled[indexes$PLANTSAMP == "~10"] = 10
indexes$plants_sampled[indexes$PLANTSAMP == ">20"] = 20
indexes$plants_sampled[indexes$PLANTSAMP == "~20"] = 20
indexes$plants_sampled[indexes$PLANTSAMP == "20+"] = 20
indexes$plants_sampled[indexes$PLANTSAMP == "< 10"] = 10

indexes$plants_sampled[is.na(indexes$plants_sampled)] = 0
indexes$ADJSTCOUNT[is.na(indexes$ADJSTCOUNT)] = 0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# convert to dates

brahms_wcvp_matched$BESTEVERD[which(brahms_wcvp_matched$BESTEVERD == "  /  /    ")] <- NA
brahms_wcvp_matched$BESTEVERD <- as.Date(brahms_wcvp_matched$BESTEVERD , format =  "%d/%m/%Y")

brahms_wcvp_matched$FIRSTTEST[which(brahms_wcvp_matched$FIRSTTEST == "  /  /    ")] <- NA
brahms_wcvp_matched$FIRSTTEST <- as.Date(brahms_wcvp_matched$FIRSTTEST , format =  "%d/%m/%Y")

brahms_wcvp_matched$LASTTEST[which(brahms_wcvp_matched$LASTTEST == "  /  /    ")] <- NA
brahms_wcvp_matched$LASTTEST <- as.Date(brahms_wcvp_matched$LASTTEST , format =  "%d/%m/%Y")

brahms_wcvp_matched$XRAYDATE[which(brahms_wcvp_matched$XRAYDATE == "  /  /    ")] <- NA
brahms_wcvp_matched$XRAYDATE <- as.Date(brahms_wcvp_matched$XRAYDATE , format =  "%d/%m/%Y")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  SUBSET CR PREDICTIONS

# keep only the CR ones
iucn_CR_predictions = iucn_predictions[which(iucn_predictions$category == "CR"),]
iucn_CR_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]

# keep only the ones that aren't already in IUCN
iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in% iucn_wcvp_matched$taxon_name)),]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$wcvp_ipni_id %in%
                                                      iucn_wcvp_matched$wcvp_ipni_id))]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in%
                                                      iucn_wcvp_matched$taxon_name))]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  DATA PREP FAMILY TREE


# Keep one representative species per family
family_reps <- tips.info.TPL[!duplicated(tips.info.TPL$family), ]

# Create the species list in the format required by phylo.maker()
sp.list <- data.frame(
  species = family_reps$species,
  genus = family_reps$genus,
  family = family_reps$family,
  stringsAsFactors = FALSE
)

# Build the tree (using scenario S3 for best interpolation)
result <- phylo.maker(
  sp.list = sp.list,
  tree = GBOTB.extended.TPL,
  nodes = nodes.info.1.TPL,
  scenarios = "S3",
  output.tree = TRUE
)

family_tree <- result$scenario.3

# renamte the tips by family not species
for (tipi in 1:length(family_tree$tip.label)){
  family_tree$tip.label[tipi] <- sp.list$family[which(sp.list$species %in% family_tree$tip.label[tipi])]
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA PREP GEOGRAPHIC INFORMATION

permissions$NewCountryName = permissions$Country
permissions$NewCountryName[which(permissions$NewCountryName == "Bolivia (Plurinational State of)")] = "Bolivia"
permissions$NewCountryName[which(permissions$NewCountryName == "Brunei Darussalam")] = "Brunei"
permissions$NewCountryName[which(permissions$NewCountryName == "Czechia")] = "Czech Republic"
permissions$NewCountryName[which(permissions$NewCountryName == "Democratic People's Republic of Korea" )] = "North Korea"
permissions$NewCountryName[which(permissions$NewCountryName == "Iran (Islamic Republic of)")] = "Iran"
permissions$NewCountryName[which(permissions$NewCountryName == "Lao People's Democratic Republic")] = "Laos"
permissions$NewCountryName[which(permissions$NewCountryName == "Micronesia (Federated States of)" )] = "Micronesia"
permissions$NewCountryName[which(permissions$NewCountryName == "Netherlands (Kingdom of the)")] = "Netherlands"
permissions$NewCountryName[which(permissions$NewCountryName == "North Macedonia")] = "Macedonia"
permissions$NewCountryName[which(permissions$NewCountryName == "Republic of Korea")] = "South Korea"
permissions$NewCountryName[which(permissions$NewCountryName == "Republic of Moldova")] = "Moldova"
permissions$NewCountryName[which(permissions$NewCountryName == "Russian Federation")] = "Russia"
permissions$NewCountryName[which(permissions$NewCountryName == "Sao Tome and Principe")] = "São Tomé and Príncipe"
permissions$NewCountryName[which(permissions$NewCountryName == "State of Palestine")] = "Palestine"
permissions$NewCountryName[which(permissions$NewCountryName == "Syrian Arab Republic")] = "Syria"
permissions$NewCountryName[which(permissions$NewCountryName == "Timor-Leste")] = "East Timor"
permissions$NewCountryName[which(permissions$NewCountryName == "Türkiye")]  =  "Turkey"
permissions$NewCountryName[which(permissions$NewCountryName == "United Kingdom of Great Britain and Northern Ireland")] = "United Kingdom"
permissions$NewCountryName[which(permissions$NewCountryName == "United Republic of Tanzania")] = "Tanzania"
permissions$NewCountryName[which(permissions$NewCountryName == "United States of America")] = "United States"
permissions$NewCountryName[which(permissions$NewCountryName == "Venezuela (Bolivarian Republic of)")] = "Venezuela"
permissions$NewCountryName[which(permissions$NewCountryName == "Viet Nam") ] =  "Vietnam"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data prep
iucn_wcvp_matched_countries_tdwg3$Target_1 = ifelse((iucn_wcvp_matched_countries_tdwg3$seeds_per_spp_country >= 1050) &
                                                      (iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country >= 50), 1, 0)


# add the proportion of range collected
iucn_dict = unique(data.frame(cbind(taxon_name = indexes$taxon_name,
                                    proportion_native_country = indexes$proportion_native_country)))

iucn_wcvp_matched_countries_tdwg3 = iucn_wcvp_matched_countries_tdwg3 %>%
  left_join(iucn_dict, by= c("taxon_name"))

iucn_wcvp_matched_countries_tdwg3$proportion_native_country[which(is.na(iucn_wcvp_matched_countries_tdwg3$proportion_native_country))] = 0

# estimate targets
iucn_wcvp_matched_countries_tdwg3$proportion_native_country = as.numeric(iucn_wcvp_matched_countries_tdwg3$proportion_native_country)
iucn_wcvp_matched_countries_tdwg3$Target_2a = ifelse((iucn_wcvp_matched_countries_tdwg3$proportion_native_country == 1) , 1, 0)


iucn_wcvp_matched_countries_tdwg3$Target_2b = ifelse((iucn_wcvp_matched_countries_tdwg3$accessions_per_spp_country >= 5) , 1, 0)
# collections_per_native_country
iucn_wcvp_matched_countries_tdwg3$Target_2 = ifelse((iucn_wcvp_matched_countries_tdwg3$Target_2a & iucn_wcvp_matched_countries_tdwg3$Target_2b &
                                                       iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country >= 50), 1, 0)


# orthodox banked unbanked certain
iucn_wcvp_matched_countries_tdwg3$orthodox_banked_certain = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                      iucn_wcvp_matched_countries_tdwg3$category_certain == "orthodox"), 1, 0)
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_certain = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                        iucn_wcvp_matched_countries_tdwg3$category_certain == "orthodox"), 1, 0)

# orthodox banked unbanked uncertain
iucn_wcvp_matched_countries_tdwg3$orthodox_banked_uncertain = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                        iucn_wcvp_matched_countries_tdwg3$category_uncertain == "orthodox"), 1, 0)
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_uncertain = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                          iucn_wcvp_matched_countries_tdwg3$category_uncertain == "orthodox"), 1, 0)

# recalcitrant/exceptional banked unbanked certain
iucn_wcvp_matched_countries_tdwg3$exceptional_banked_certain = ifelse(((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                          iucn_wcvp_matched_countries_tdwg3$category_certain == "recalcitrant") |
                                                                         (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                            iucn_wcvp_matched_countries_tdwg3$category_certain == "exceptional")), 1, 0)

iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_certain = ifelse(((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                            iucn_wcvp_matched_countries_tdwg3$category_certain == "recalcitrant") |
                                                                           (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                              iucn_wcvp_matched_countries_tdwg3$category_certain == "exceptional")), 1, 0)

# recalcitrant/exceptional banked unbanked uncertain
iucn_wcvp_matched_countries_tdwg3$exceptional_banked_uncertain = ifelse(((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                            iucn_wcvp_matched_countries_tdwg3$category_uncertain == "recalcitrant") |
                                                                           (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                              iucn_wcvp_matched_countries_tdwg3$category_uncertain == "exceptional")), 1, 0)

iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_uncertain = ifelse(((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                              iucn_wcvp_matched_countries_tdwg3$category_uncertain == "recalcitrant") |
                                                                             (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                                iucn_wcvp_matched_countries_tdwg3$category_uncertain == "exceptional")), 1, 0)


iucn_wcvp_matched_countries_tdwg3$orthodox_banked_certain[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_banked_certain)] = 0
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_certain[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_certain)] = 0
iucn_wcvp_matched_countries_tdwg3$exceptional_banked_certain[is.na(iucn_wcvp_matched_countries_tdwg3$exceptional_banked_certain)] = 0
iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_certain[is.na(iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_certain)] = 0

iucn_wcvp_matched_countries_tdwg3$orthodox_banked_uncertain[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_banked_uncertain)] = 0
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_uncertain[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_uncertain)] = 0
iucn_wcvp_matched_countries_tdwg3$exceptional_banked_uncertain[is.na(iucn_wcvp_matched_countries_tdwg3$exceptional_banked_uncertain)] = 0
iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_uncertain[is.na(iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_uncertain)] = 0


country_names = data.frame(unique(iucn_wcvp_matched_countries_tdwg3[,"NewCountryName"]))
colnames(country_names) = "NewCountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")],
                                            by=c("NewCountryName" = "NAME_0"))

country_counts_map = country_stats %>% left_join(country_names)
country_counts_map = world %>% left_join(country_counts_map)
country_counts_map$sum_CR[is.na(country_counts_map$sum_CR)] = 0
country_counts_map$sum_CR_banked[is.na(country_counts_map$sum_CR_banked)] =0


country_counts_map$log_CR = log(country_counts_map$sum_CR+1)
country_counts_map$log_CR_banked = log(country_counts_map$sum_CR_banked+1)
country_counts_map$proportion = country_counts_map$sum_CR_banked/country_counts_map$sum_CR
country_counts_map$proportion[is.na(country_counts_map$proportion)] = 0
country_counts_map$log_proportion = log(country_counts_map$proportion+1)

# add the permit data
country_counts_map = country_counts_map %>% left_join(permissions, by = c("NAME_0" = "NewCountryName"))

# targets per country
country_counts_map$prop_Target_1 = country_counts_map$sum_Target1/(country_counts_map$sum_CR_banked + 0.0001)
country_counts_map$prop_Target_2 = country_counts_map$sum_Target2/(country_counts_map$sum_CR_banked + 0.0001)
country_counts_map$prop_Target_1[is.na(country_counts_map$prop_Target_1)] = 0
country_counts_map$prop_Target_2[is.na(country_counts_map$prop_Target_2)] = 0

# access and benefit sharing
country_counts_map$NFP = ifelse(country_counts_map$ABS_National_Focal_Point_NFP >= 1,1,0)
country_counts_map$CNA = ifelse(country_counts_map$Competent_National_Authority_CNA >= 1,1,0)
country_counts_map$IRCC = ifelse(country_counts_map$Internationally_Recognized_Certificates_Compliance_IRCC >= 1,1,0)
country_counts_map$NR = country_counts_map$Interim_National_Reports_Implementation_Nagoya_Protocol_NR



country_counts_map$NFP[is.na(country_counts_map$NFP)] = 0
country_counts_map$CNA[is.na(country_counts_map$CNA)] = 0
country_counts_map$IRCC[is.na(country_counts_map$IRCC)] = 0
country_counts_map$NR[is.na(country_counts_map$NR)] = 0

country_counts_map$permits = (country_counts_map$NFP  +
                                country_counts_map$CNA  +
                                country_counts_map$IRCC  +
                                country_counts_map$NR)
country_counts_map$permits[is.na(country_counts_map$permits_4)] = 0

# proportion banked
country_counts_map$prop_banked = country_counts_map$sum_CR_banked/(country_counts_map$sum_CR_pred+0.001)
country_counts_map$prop_banked[is.na(country_counts_map$prop_banked)] = 0

country_counts_map$sum_orthodox_banked_certain[is.na(country_counts_map$sum_orthodox_banked_certain)] = 0
country_counts_map$sum_orthodox_unbanked_certain[is.na(country_counts_map$sum_orthodox_unbanked_certain)] = 0
country_counts_map$sum_exceptional_banked_certain[is.na(country_counts_map$sum_exceptional_banked_certain)] = 0
country_counts_map$sum_exceptional_unbanked_certain[is.na(country_counts_map$sum_exceptional_unbanked_certain)] = 0

country_counts_map$sum_orthodox_banked_uncertain[is.na(country_counts_map$sum_orthodox_banked_uncertain)] = 0
country_counts_map$sum_orthodox_unbanked_uncertain[is.na(country_counts_map$sum_orthodox_unbanked_uncertain)] = 0
country_counts_map$sum_exceptional_banked_uncertain[is.na(country_counts_map$sum_exceptional_banked_uncertain)] = 0
country_counts_map$sum_exceptional_unbanked_uncertain[is.na(country_counts_map$sum_exceptional_unbanked_uncertain)] = 0

country_counts_map$sum_CR_pred[is.na(country_counts_map$sum_CR_pred)] = 0


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#######################################################################################
###                NAME MATCHING STATS
#######################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


###############################################################
# how many names have been matched from IUCN to wcvp
###############################################################


# how many were name matched where
length(unique(iucn$scientificName)) # 6520 # 5702 before matching

length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")]))/length(unique(iucn$scientificName)) #0.993865
# 0.9814417

length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")]))
# 6399 to WCVP

#### Accepted for wcvp
(length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$wcvp_status == "Accepted" &
               # iucn_wcvp_matched$wcvp_homotypic &
               iucn_wcvp_matched$taxonomic_backbone == "WCVP")]))/
  length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")])))
# 0.9129551

#### Synonym for wcvp
(length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$wcvp_status == "Synonym" &
                                                        is.na(iucn_wcvp_matched$wcvp_homotypic) &
                                                        iucn_wcvp_matched$taxonomic_backbone == "WCVP")]))/
    length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")])))
# 0.0329739


#### Homotypic Synonym for wcvp
(length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$wcvp_status == "Synonym" &
                                                        iucn_wcvp_matched$wcvp_homotypic &
                                                        iucn_wcvp_matched$taxonomic_backbone == "WCVP")]))/
    length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")])))
# 0.05188311

# remaining names to match
(length(unique(iucn_wcvp_matched$scientificName[which(is.na(iucn_wcvp_matched$taxonomic_backbone))])) +
length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$taxonomic_backbone == "WFO")])))
# 81

# matched by WFO
length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$taxonomic_backbone == "WFO")]))# 66
# unresolved
length(unique(iucn_wcvp_matched$scientificName[which(is.na(iucn_wcvp_matched$taxonomic_backbone))]))# 15

# Total names starting with
length(unique(iucn_wcvp_matched$scientificName)) # 6480 # 5667 were matched
length(unique(iucn_wcvp_matched$taxon_name[which(!is.na(iucn_wcvp_matched$taxon_name))])) # 6436 to this many names


## What years are the IUCN assessments from
length(which(iucn_banked_recalcitrance$yearPublished < 2000)) #293 #334
length(which(iucn_banked_recalcitrance$yearPublished >= 2000 & iucn_banked_recalcitrance$yearPublished < 2010)) # 462 #472
length(which(iucn_banked_recalcitrance$yearPublished >= 2010 & iucn_banked_recalcitrance$yearPublished < 2020)) # 2020 #1973
length(which(iucn_banked_recalcitrance$yearPublished[which(iucn_banked_recalcitrance$redlistCriteria != "prediction")] >= 2020)) # 3701 #2895
# length(iucn_banked_recalcitrance$yearPublished[which(iucn_banked_recalcitrance$redlistCriteria == "prediction")]) # 233



###############################################################
# how many names have been matched from IUCN predictions to wcvp
###############################################################

# how many of the predicted IUCN species were matched

# how many are CR?
length(unique(iucn_CR_predictions_wcvp_matched$full_name)) # 198
length(unique(iucn_CR_predictions_wcvp_matched$taxon_name[which(!is.na(iucn_CR_predictions_wcvp_matched$taxon_name))])) # 199

length(unique(iucn_CR_predictions_wcvp_matched$full_name[which(iucn_CR_predictions_wcvp_matched$taxonomic_backbone == "WCVP")])) # 199
length(unique(iucn_CR_predictions_wcvp_matched$full_name[which(iucn_CR_predictions_wcvp_matched$taxonomic_backbone == "WFO")])) # 0


###############################################################
# final number of CR and CRpred
###############################################################

length(unique(iucn_wcvp_matched$taxon_name[which(!is.na(iucn_wcvp_matched$taxon_name))])) + length(unique(iucn_CR_predictions_wcvp_matched$taxon_name[which(!is.na(iucn_CR_predictions_wcvp_matched$taxon_name))]))
# 6635
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$taxon_name))]))
# 6635

summary(as.factor(iucn_banked_recalcitrance$higher[which(!is.na(iucn_banked_recalcitrance$taxon_name))]))
# Angiosperms       Bryophyta       Ferns     Gymnosperms     Lycophytes   Marchantiophyta
# 6469              27              98        88              17           10

###############################################################
# Exceptional species
###############################################################

# how many species are the exceptional species
length(unique(exceptional$Species.name)) # 23792 #23530 before matching
length(unique(exceptional_wcvp_matched$Species.name)) # 22586 #22283 were matched
length(unique(exceptional_wcvp_matched$taxon_name)) # 22588 #22298 to this many new names
length(unique(which(exceptional_wcvp_matched$taxonomic_backbone == "WCVP"))) # 22588 #22251 this many from WCVP
length(unique(which(exceptional_wcvp_matched$taxonomic_backbone == "WFO"))) #0  #47 from WFO
length(unique(exceptional_wcvp_matched$Species.name))/length(unique(exceptional$Species.name)) # 0.9493107


###################################################################
##  MSB that are name matched to wcvp
###################################################################

##### MSB #########################################################

# how many accessions
length(unique(brahms$ACCESSION)) #223876 start
# length(unique(brahms_wcvp_matched$ACCESSION)) # 223876 #197934

# how many names were matched
length(unique(brahms$full_name)) # 56755 to start with
length(unique(brahms_wcvp_matched$full_name[which(!is.na(brahms_wcvp_matched$taxon_name))])) # 54405 #matched to these names
length(unique(brahms_wcvp_matched$taxon_name[which(!is.na(brahms_wcvp_matched$taxon_name))])) # 52874 #matched to these names

# by WCVP
length(unique(brahms_wcvp_matched$taxon_name[which(!is.na(brahms_wcvp_matched$taxon_name) &
                                                            brahms_wcvp_matched$taxonomic_backbone == "WCVP")]))
# 52637
(length(unique(brahms_wcvp_matched$taxon_name[which(!is.na(brahms_wcvp_matched$taxon_name) &
                                                         brahms_wcvp_matched$taxonomic_backbone == "WCVP")]))/
    length(unique(brahms_wcvp_matched$taxon_name[which(!is.na(brahms_wcvp_matched$taxon_name))])))
#0.9955176


# by WFO
length(unique(brahms_wcvp_matched$taxon_name[which(!is.na(brahms_wcvp_matched$taxon_name) &
                                                     brahms_wcvp_matched$taxonomic_backbone == "WFO")]))
# 348
(length(unique(brahms_wcvp_matched$taxon_name[which(!is.na(brahms_wcvp_matched$taxon_name) &
                                                      brahms_wcvp_matched$taxonomic_backbone == "WFO")]))/
    length(unique(brahms_wcvp_matched$taxon_name[which(!is.na(brahms_wcvp_matched$taxon_name))])))
#0.006581685
# overlap because some sub-species were matched by WFO to a taxon name also in WCVP

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################################################
###    CR in MSB
###################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# how many accessions
length(unique(CR_msbp$ACCESSION)) # 2523 accessions
length(unique(CR_msbp$taxon_name)) # 476 taxa
length(unique(CR_msbp$full_name)) # 486 taxa

# how many are predictions
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))
# 483

# how many are predictions
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$redlistCriteria == "prediction" &
                                                           iucn_banked_recalcitrance$banked == T)]))
# 7


# how many are predictions
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$redlistCriteria != "prediction" &
                                                           iucn_banked_recalcitrance$banked == T)]))
# 476

# % of brahms species that are CR
(length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$taxon_name %in% CR_msbp$taxon_name)]))/
    length(unique(brahms_wcvp_matched$taxon_name)))
# 0.01126963

# % of brahms accesssions that are CR
(length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$ACCESSION %in% CR_msbp$ACCESSION)]))/
  length(unique(brahms_wcvp_matched$ACCESSION)))
# 0.01126963

# How many families?
length(unique(CR_msbp$family)) # 96
length(unique(CR_msbp$order)) # 41

# find how many of each family there are the bank
CR_MSB_higher_list  = CR_msbp[which(duplicated(CR_msbp$taxon_name)==F),] %>%
  group_by(higher) %>%
  tally()

CR_MSB_higher_list
# higher          n
# <chr>       <int>
# 1 Angiosperms   458
# 2 Ferns           5
# 3 Gymnosperms    11
# 4 Lycophytes      2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############################################################################################
#### IUCN Family information
##############################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Format data for stats
tr <- family_tree
numtip <- length(tr$tip.label)

# Get number of species per family that are CR and that are banked
fam_count = iucn_banked_recalcitrance[,c("family", "banked", "accessions")] %>%
  group_by(family) %>%
  summarize(
    CR_species = n(),
    banked_species = length(which(banked)),
    total_accessisions = sum(accessions[!is.na(accessions)])
  )

test = data.frame(tr$tip.label) %>% left_join(fam_count,
                                              by = c("tr.tip.label" = "family"))
fam_count$prop_banked = fam_count$banked_species/fam_count$CR_species
fam_count = unique(fam_count)

##### Get stats from tree #################

# what families have the most CR?
head(data.frame(fam_count %>% arrange(desc(CR_species))))
#        family CR_species banked_species total_accessisions prop_banked
# 1   Rubiaceae        428             14                 26  0.03271028
# 2   Myrtaceae        365             22                 54  0.06027397
# 3    Fabaceae        322             34                 74  0.10559006
# 4   Lauraceae        318              0                  0  0.00000000
# 5  Asteraceae        289             39                 71  0.13494810
# 6 Orchidaceae        281             26                 50  0.09252669

# what proportion of families have CR
length(fam_count$family[which(fam_count$CR_species > 0)]) # 244
length(fam_count$family[which(fam_count$CR_species > 0)])/length(tr$tip.label)
# 0.5062241

# what proportion of families have CR and at least one banked
length(fam_count$family[which(fam_count$banked_species > 0 & fam_count$CR_species > 0)]) # 96
length(fam_count$family[which(fam_count$banked_species > 0 & fam_count$CR_species > 0)])/length(tr$tip.label)
# 0.3934426
length(fam_count$family[which(fam_count$banked_species > 0 & fam_count$CR_species > 0)])/length(fam_count$family)
# 0.3934426


# what proportion of families 50% family banked
length(fam_count$family[which(fam_count$banked_species > 0 & fam_count$prop_banked >= 0.5)]) # 15
length(fam_count$family[which(fam_count$banked_species > 0 & fam_count$prop_banked >= 0.5)])/length(fam_count$family)
# 0.06147541

# what proportion of families 100% family banked
length(fam_count$family[which(fam_count$banked_species > 0 & fam_count$prop_banked == 1)]) # 15
length(fam_count$family[which(fam_count$banked_species > 0 & fam_count$prop_banked == 1)])/length(fam_count$family)
# 0.03688525
fam_count$family[which(fam_count$banked_species > 0 & fam_count$prop_banked == 1)]
# [1] "Byblidaceae"   "Cistaceae"     "Frankeniaceae"
# [4] "Kewaceae"      "Moringaceae"   "Nymphaeaceae"
# [7] "Onagraceae"    "Paulowniaceae" "Stylidiaceae"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####################################################################################
#  GEOGRAPHIC
######################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# countries with over 50% of their CR taxa banked
country_counts_map$NAME_0[ which(country_counts_map$prop_banked > 0.5)]
# [1] "Australia"      "Germany"        "Spain"          "United Kingdom"
# [5] "Namibia"        "Saint Helena"

# country numbers with over 50% of their CR taxa banked
country_counts_map$sum_CR_banked [ which(country_counts_map$prop_banked > 0.5)]
# 73 13 69 22  8 23

# m = st_buffer(country_counts_map, 0)
# country_counts_map.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
#                                                            xmax = 180,
#                                                            ymin = -90,
#                                                            ymax = 90))),
#                                       crs = PROJ)

temp <- iucn_wcvp_matched_countries_tdwg3[which(!is.na(iucn_wcvp_matched_countries_tdwg3$taxon_name)),]

# number of CR species
length(unique(temp$taxon_name[which(temp$redlistCriteria != "prediction")]))
#6436 # 5654
length(unique(temp$taxon_name[which(temp$redlistCriteria == "prediction")]))
#199 # 96
length(unique(temp$taxon_name))
#6635 # 5750


# number of countries
# length(unique(temp$NewCountryName[which(temp$redlistCriteria != "prediction")]))
# # 166 # 166
# length(unique(temp$NewCountryName[which(temp$redlistCriteria == "prediction")]))
# # 49 # 38
length(unique(temp$NewCountryName))
# 166 # 166


# countries with more than 10 CR species
country_data2 = data.frame(country_counts_map)# data.frame(country_counts_map.prj)
country_data2 = country_data2[,1:(ncol(country_data2)-1)]
# country_data2$NewCountryName[country_data2$sum_CR_pred >= 10]
length(unique(country_data2$NewCountryName[country_data2$sum_CR_pred < 10])) # 46

# countries with more than 100CR species
country_data2$NewCountryName[country_data2$sum_CR_pred >= 150]
# [1] "Brazil"
# [2] "China"
# [3] "Cameroon"
# [4] "Colombia"
# [5] "Cuba"
# [6] "Ecuador"
# [7] "Indonesia"
# [8] "India"
# [9] "Madagascar"
# [10] "Mexico"
# [11] "New Caledonia"
# [12] "Peru"
# [13] "Philippines"
# [14] "Papua New Guinea"
# [15] "Tanzania"
# [16] "United States"
# [17] "South Africa"
length(country_data2$NewCountryName[country_data2$sum_CR_pred >= 150]) #17



# countries with the highest % of CR species
country_stats$prop_CR = country_stats$sum_CR_pred/length(unique(temp$taxon_name))
test = country_stats %>%
  arrange(desc(prop_CR)) %>%  # arrange in descending order
  slice(1:12)
test[,c("NewCountryName","prop_CR")]
#      NewCountryName    prop_CR
# 1        Madagascar 0.08304446
# 2     United States 0.06902788
# 3         Indonesia 0.06028636
# 4       Philippines 0.05697061
# 5           Ecuador 0.04853052
# 6            Brazil 0.04491334
# 7            Mexico 0.04295403
# 8          Colombia 0.04220045
# 9  Papua New Guinea 0.03767898
# 10         Cameroon 0.02803316
# 11         Tanzania 0.02697815
# 12            China 0.02652600

# countries with over 50% of spp banked
country_data2$NewCountryName[country_data2$prop_banked >= 0.5]
# "Australia"      "Botswana"       "Spain"
# "United Kingdom" "Namibia"        "Saint Helena"

country_data2$sum_CR_banked[country_data2$NewCountryName %in% country_data2$NewCountryName[country_data2$prop_banked >= 0.5]]
#  74  2 66 22  4 22


#countries with highest proportion meeting target 1
country_data2$NewCountryName[country_data2$prop_Target_1 >= 0.9]
# "United Arab Emirates" "Austria"              "Botswana"
# "Cayman Islands"       "Oman"                 "Saudi Arabia"
# "Sierra Leone"         "Yemen"                "Zambia"
# "Zimbabwe"

length(unique(country_data2$NewCountryName[country_data2$prop_Target_1 <= 0.1]))
# 134

#countries with highest proportion meeting target 2
country_data2$NewCountryName[country_data2$prop_Target_2 >= 0.9]
#0
length(unique(country_data2$NewCountryName[country_data2$prop_Target_2 <= 0.1]))
#160

# countries that meet both targets
country_data2$NewCountryName[country_data2$prop_Target_2 >= 0.9 & country_data2$prop_Target_1 >= 0.9] #0

# nunmber pf countries with each permission level
sum(country_data2$NFP)  #173
sum(country_data2$CNA)  #79
sum(country_data2$IRCC) #27
sum(country_data2$NR)   #99

# countries with all four ABS
unique(country_data2$NewCountryName[country_data2$permits == 4])
# [1] "Argentina"          NA                   "Belgium"
# [4] "Benin"              "Bulgaria"           "Belarus"
# [7] "Bhutan"             "Côte d'Ivoire"      "Cameroon"
# [10] "Dominican Republic" "Spain"              "Ethiopia"
# [13] "France"             "Guatemala"          "Guyana"
# [16] "India"              "Kenya"              "Laos"
# [19] "Mexico"             "Panama"             "Peru"
# [22] "Uruguay"            "Vietnam"            "South Africa"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#######################################################################################
### Get index stats
#######################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(indexes$information_index)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.3333  0.6667  0.9167  0.8128  1.0000  1.0000

summary(indexes$viability_index)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.0000  0.0000  0.3114  0.6667  1.0000

summary(indexes$genetic_index)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.0000  0.0000  0.3118  0.8000  1.0000

summary(indexes$total_index)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.1111  0.2222  0.3056  0.4787  0.7944  1.0000

# Info index stats #####################


summary(as.factor(indexes$geographic_index))/ length(indexes$geographic_index)
#          0        0.5       0.75          1
# 0.02334784 0.10605461 0.36723387 0.50336367
mean(indexes$geographic_index)
# 0.8318164


summary(as.factor(indexes$taxonomy_index))/ length(indexes$taxonomy_index)
# 1
# 1
mean(indexes$taxonomy_index)
# 1


summary(as.factor(indexes$year_index))/ length(indexes$year_index)
#         0         1
# 0.3933518 0.6066482
mean(indexes$year_index)
# 0.6066482

### Viability index

summary(as.factor(indexes$count_index))/ length(indexes$count_index)
#        0        1
# 0.547685 0.452315
mean(indexes$count_index)
# 0.452315


summary(as.factor(indexes$adjcount_index))/ length(indexes$adjcount_index)
#         0         1
# 0.6612584 0.3387416
mean(indexes$adjcount_index)
# 0.3387416


summary(as.factor(indexes$germination_index))/ length(indexes$germination_index)
# 0          0.5        1
# 0.84843688 0.01701622 0.13454689
mean(indexes$germination_index)
#0.143055

### Germination testing

# proportion of CR accessions tested ~~~~~~~~~~~
nrow(indexes[which(!is.na(indexes$LASTTEST)),])/nrow(indexes)
# 0.1516345
nrow(indexes)
# 2527


# how many with over 75% viability
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] >= 75))/
  nrow(indexes[which(!is.na(indexes$BESTEVERD)),]) # 0.75

# how many with over 100% viability
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] == 100)) #129
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] < 100)) #129
summary(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD) & indexes$BESTEVER < 100)]) #129
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.00   50.00   81.00   68.98   92.00   99.00

# how many with over 0% viability
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] == 0)) #0



## GENETIC INDEX
summary(as.factor(indexes$cultivation_index))/ length(indexes$cultivation_index)
# 0           0.5         1
# 0.924416304 0.070043530 0.005540166
mean(indexes$cultivation_index)
# 0.04056193


summary(as.factor(indexes$exsitu_index))/ length(indexes$exsitu_index)
# 0           0.2         0.6         0.8         1
# 0.642659280 0.006727345 0.015037594 0.232686981 0.102888801
mean(indexes$exsitu_index)
# 0.2994064



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################
#           Proposed  targets per CR plant species
####################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# per species data
spp_count = indexes[,c("taxon_name", "ADJSTCOUNT", "plants_sampled")] %>%
  group_by(taxon_name) %>%
  summarize(
    accessions = n(), # Number of times the species appears
    summed_count = sum(ADJSTCOUNT), # Sum of seed count per species
    summed_sampled = sum(plants_sampled) # Sum of plants sampled per species
  )


to_add = iucn_banked_recalcitrance[,c("taxon_name", "redlistCriteria")]
to_add = to_add[duplicated(to_add$taxon_name)==FALSE,]
spp_count = spp_count %>% left_join(to_add, by = "taxon_name")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#############################
# Initial stats
#############################

# number of species
length(unique(spp_count$taxon_name))
# 476
length(unique(indexes$taxon_name))
# 476

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################
# Target 1
#############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# how many species meet target 1 individually
length(unique(indexes$taxon_name[which(indexes$Target_1)]))
# 35

# how many collections meet target 1 individually
length(which(indexes$Target_1))
# 46


# define new combined target 1
spp_count$Target_1 = (spp_count$summed_count >= 1050 &
                        spp_count$summed_sampled >= 50)

# how many species meet target 1 once collections are merged?
length(unique(spp_count$taxon_name[which(spp_count$Target_1)]))
# 46

round(length(which(spp_count$Target_1 == T))/ length(spp_count$summed_count)*100,2)
# 9.66

#############################
# TARGET 1A
#############################

# how many species meet target 1 individually
length(unique(indexes$taxon_name[which(indexes$Target_1a)]))
# 154

# round(length(which(indexes$Target_1a))/ length(indexes$Target_1a)*100,2)
# # 12.74

round(length(unique(indexes$taxon_name[which(indexes$Target_1a)]))/
        length(unique(indexes$taxon_name))*100,2 )
#32.35

#############################
# TARGET 1B
#############################

# how many species meet target 1 individually
length(unique(indexes$taxon_name[which(indexes$Target_1b)]))
# 154

# round(length(which(indexes$Target_1b))/ length(indexes$Target_1b)*100,2)
# # 12.74

round(length(unique(indexes$taxon_name[which(indexes$Target_1b)]))/
        length(unique(indexes$taxon_name))*100,2 )
#32.35


####################################
# how many species have no data?
#####################################


length(unique(indexes$taxon_name[indexes$ADJSTCOUNT == 0]))
# 235

# percentage
round(length(unique(indexes$taxon_name[indexes$ADJSTCOUNT == 0]))/
        length(unique(indexes$taxon_name))*100,2)
# 49.37

length(unique(indexes$taxon_name[indexes$plants_sampled == 0]))
# 346

# percentage
round(length(unique(indexes$taxon_name[indexes$plants_sampled == 0]))/
        length(unique(indexes$taxon_name))*100,2)
# 72.69


# how many IUCN taxa still need banking?
length(unique(iucn_banked_recalcitrance$taxon_name[which(!iucn_banked_recalcitrance$banked)]))
# 6153

# how many species don't yet meet target 1 of all the IUCN
names = unique(indexes$taxon_name[which(indexes$Target_1)])
length(which(!(unique(iucn_banked_recalcitrance$taxon_name) %in% names)))
# 6600

# how many are banked but don't meet target 1
length(which(!(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked)]) %in% names)))
# 448

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################
# Target 2
#############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iucn_dict = unique(data.frame(cbind(taxon_name = indexes$taxon_name,
                                    proportion_native_country = indexes$proportion_native_country,
                                    collections_all_native_country = indexes$collections_all_native_country)))

# get rid of duplicates
dupl = unique(iucn_dict$taxon_name[which(duplicated(iucn_dict$taxon_name))])
for (namei in dupl){
  sub = iucn_dict[which(iucn_dict$taxon_name %in% namei),]
  new = data.frame(taxon_name = namei,
                   proportion_native_country = max(sub$proportion_native_country, na.rm=TRUE),
                   collections_all_native_country = max(sub$collections_all_native_country, na.rm=TRUE))
  iucn_dict = iucn_dict[-which(iucn_dict$taxon_name == namei),]
  iucn_dict = rbind(iucn_dict, new)
}

# add on to target count data
spp_count = spp_count %>%
  left_join(iucn_dict, by= c("taxon_name"))

# View(spp_count)

#############################
# Target 2
#############################

# how many species meet target 2 individually
length(unique(indexes$taxon_name[which(indexes$Target_2)]))
# 16

# how many collections meet target 2 individually
length(which(indexes$Target_2))
# 37

# define new combined target 1
spp_count$Target_2 = (spp_count$summed_count >= 1050 &
                        spp_count$collections_all_native_country >= 5 &
                        spp_count$proportion_native_country == 1)

# how many species meet target 1 once collections are merged?
length(unique(spp_count$taxon_name[which(spp_count$Target_2)]))
# 26

round(length(which(spp_count$Target_2 == T))/ length(spp_count$summed_count)*100,2)
# 5.46

#############################
# TARGET 2A
#############################

# how many species meet target 1 individually
length(unique(indexes$taxon_name[which(indexes$Target_2a)]))
# 344

# round(length(which(indexes$Target_1a))/ length(indexes$Target_1a)*100,2)
# # 12.74

round(length(unique(indexes$taxon_name[which(indexes$Target_2a)]))/
        length(unique(indexes$taxon_name))*100,2 )
#72.27

#############################
# TARGET 2B
#############################

# how many species meet target 1 individually
length(unique(indexes$taxon_name[which(indexes$Target_2b)]))
# 123

# round(length(which(indexes$Target_1b))/ length(indexes$Target_1b)*100,2)
# # 12.74

round(length(unique(indexes$taxon_name[which(indexes$Target_2b)]))/
        length(unique(indexes$taxon_name))*100,2 )
#25.84



# average proportion of countries collected from across the range of a banked CR taxa
round(mean(indexes$proportion_native_country)*100,2)
# 86.9

#############################

# how many species don't yet meet target 2 of all the IUCN
names = unique(indexes$taxon_name[which(indexes$Target_2)])
round(length(which(!(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked)]) %in% names)))/
        length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked)]))*100,2)
# 96.69

length(which(!(unique(iucn_banked_recalcitrance$taxon_name) %in% names)))
# 6619

# how many are banked but don't meet target 2
length(which(!(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked)]) %in% names)))
# 467

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############################################################################
#  GEOGRAPHIC STATS FOR TARGETS
##############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TARGET 1

# what countries have over 90% CR meeting target 1
country_data2$NAME_0[which(country_data2$prop_Target_1 >0.9)][order(country_data2$NAME_0[which(country_data2$prop_Target_1 >0.9)])]
# [1] "Austria"              "Botswana"
# [3] "Cayman Islands"       "Oman"
# [5] "Saudi Arabia"         "Sierra Leone"
# [7] "United Arab Emirates" "Yemen"
# [9] "Zambia"               "Zimbabwe"

# less than 10% CR meeting target 1
length(unique(country_data2$NAME_0[which(country_data2$prop_Target_1 <0.1)]))
#199

# TARGET 2

# what countries have over 90% CR meeting target 1
country_data2$NAME_0[which(country_data2$prop_Target_2 >0.1)][order(
  country_data2$NAME_0[which(country_data2$prop_Target_2 >0.1)])]
# [1] "Australia"      "Cayman Islands"
# [3] "New Zealand"    "Portugal"
# [5] "Saint Helena"   "Sierra Leone"

# less than 10% CR meeting target 1
length(unique(country_data2$NAME_0[which(country_data2$prop_Target_2 <0.1)]))
#225



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############################################################################
# SEED STORAGE BEHAVIOR PROPORTIONS (BANKED/UNBANKED)
##############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ALL  ###################################
# seed storage behaviour tool number of predictions
length(unique(iucn_banked_recalcitrance$taxon_name[which((!is.na(iucn_banked_recalcitrance$probability.of.recalcitrance)) |
                                                           iucn_banked_recalcitrance$Exceptional_status == "Exceptional" |
                                                           (!is.na(iucn_banked_recalcitrance$Storage)) |
                                                           (!is.na(iucn_banked_recalcitrance$SID_Seed_Storage_Behaviour)))]))

# 6555

### WYSE and DICKIE

# seed storage behaviour tool number of predictions
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$probability.of.recalcitrance))]))
# 6423


### PENCE

# seed storage behaviour tool number of predictions
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$Exceptional_status == "Exceptional")]))
# 351

### SID
# Seed Information Database
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$SID_Seed_Storage_Behaviour))]))
# 351


### HAWAII
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$Storage))]))
# 427



##############################################################################
#  CONSERVATIVE (CERTAIN) ESTIMATES OF ORTHODOXY
##############################################################################


# unbanked taxa
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                           iucn_banked_recalcitrance$category_certain != "unknown" &
                                                           iucn_banked_recalcitrance$banked == F)]))
# 2223

# PENCE ~~~~~~~~~~~~~~
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                           iucn_banked_recalcitrance$category_certain != "unknown" &
                                                           (iucn_banked_recalcitrance$category_certain_ref == "Pence et al. 2022") &
                                                           iucn_banked_recalcitrance$banked == F)]))
# 11

# percentage
round(length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                 iucn_banked_recalcitrance$category_certain != "unknown" &
                                                           (iucn_banked_recalcitrance$category_certain_ref == "Pence et al. 2022") &
                                                           iucn_banked_recalcitrance$banked == F)])) /
  length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                             iucn_banked_recalcitrance$category_certain != "unknown" &
                                                             iucn_banked_recalcitrance$banked == F)]))*100,2)
#0.49


# SID ~~~~~~~~~~~~~~
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                           iucn_banked_recalcitrance$category_certain != "unknown" &
                                                           (iucn_banked_recalcitrance$category_certain_ref == "SID") &
                                                           iucn_banked_recalcitrance$banked == F)]))
# 33

# percentage
round(length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                 iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                 (iucn_banked_recalcitrance$category_certain_ref == "SID") &
                                                                 iucn_banked_recalcitrance$banked == F)])) /
        length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                   iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                   iucn_banked_recalcitrance$banked == F)]))*100,2)
#1.48


# Hawaii expert ~~~~~~~~~~~~~~
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                           iucn_banked_recalcitrance$category_certain != "unknown" &
                                                           (!(iucn_banked_recalcitrance$category_certain_ref %in% c("SID","Pence et al. 2022","Wyse and Dickie (2017)"))) &
                                                           iucn_banked_recalcitrance$banked == F)]))
# 292

# percentage
round(length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                 iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                 (!(iucn_banked_recalcitrance$category_certain_ref %in% c("SID","Pence et al. 2022","Wyse and Dickie (2017)"))) &
                                                                 iucn_banked_recalcitrance$banked == F)])) /
        length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                   iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                   iucn_banked_recalcitrance$banked == F)]))*100,2)
#13.14


# WYSE and DICKIE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                           iucn_banked_recalcitrance$category_certain != "unknown" &
                                                           (iucn_banked_recalcitrance$category_certain_ref == "Wyse and Dickie (2017)") &
                                                           iucn_banked_recalcitrance$banked == F)]))
# 1887

# percentage
round(length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                 iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                 (iucn_banked_recalcitrance$category_certain_ref == "Wyse and Dickie (2017)") &
                                                                 iucn_banked_recalcitrance$banked == F)])) /
        length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                   iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                   iucn_banked_recalcitrance$banked == F)]))*100,2)
#84.89


# WYSE and DICKIE precision level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(as.factor(iucn_banked_recalcitrance$tax.level[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                              iucn_banked_recalcitrance$category_certain != "unknown" &
                                                           (iucn_banked_recalcitrance$category_certain_ref == "Wyse and Dickie (2017)") &
                                                           iucn_banked_recalcitrance$banked == F)]))
# Genus Species
# 1875      18

# percentage
round(summary(as.factor(iucn_banked_recalcitrance$tax.level[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                    iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                    (iucn_banked_recalcitrance$category_certain_ref == "Wyse and Dickie (2017)") &
                                                                    iucn_banked_recalcitrance$banked == F)])) /
        length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                   iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                   iucn_banked_recalcitrance$banked == F)]))*100,2)
# Genus Species
# 84.35    0.81


# Numbers of different categories (orthodox, exceptional,) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(as.factor(iucn_banked_recalcitrance$category_certain[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                     iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                     iucn_banked_recalcitrance$banked == F)]))
# exceptional intermediate     orthodox recalcitrant
#          42           85         1505          612
42 + 612
# 654

# percentage
round(summary(as.factor(iucn_banked_recalcitrance$category_certain[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                     iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                     iucn_banked_recalcitrance$banked == F)]))
/
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F)]))*100,2)

# exceptional intermediate     orthodox recalcitrant
#        0.68         1.38        24.46         9.95

0.68 + 9.95
# 10.63



# unknown
length(unique(iucn_banked_recalcitrance$taxon_name[which((is.na(iucn_banked_recalcitrance$category_certain ) |
                                                           iucn_banked_recalcitrance$category_certain == "unknown") &
                                                           iucn_banked_recalcitrance$banked == F)]))
# 3939

round(length(unique(iucn_banked_recalcitrance$taxon_name[which((is.na(iucn_banked_recalcitrance$category_certain ) |
                                                            iucn_banked_recalcitrance$category_certain == "unknown") &
                                                           iucn_banked_recalcitrance$banked == F)]))/
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F)]))*100,2)



##############################################################################
#  LIKELY (UNCERTAIN) ESTIMATES OF ORTHODOXY
##############################################################################


# WYSE and DICKIE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                           iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                           (iucn_banked_recalcitrance$category_uncertain_ref == "Wyse and Dickie (2017)") &
                                                           iucn_banked_recalcitrance$banked == F)]))
# 5056

# percentage
round(length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                                 iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                                 (iucn_banked_recalcitrance$category_uncertain_ref == "Wyse and Dickie (2017)") &
                                                                 iucn_banked_recalcitrance$banked == F)])) /
        length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                                   iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                                   iucn_banked_recalcitrance$banked == F)]))*100,2)
# 93.68


# WYSE and DICKIE precision level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(as.factor(iucn_banked_recalcitrance$tax.level[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                              iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                              (iucn_banked_recalcitrance$category_uncertain_ref == "Wyse and Dickie (2017)") &
                                                              iucn_banked_recalcitrance$banked == F)]))
# Genus Species
# 1875      18

# percentage
round(summary(as.factor(iucn_banked_recalcitrance$tax.level[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                                    iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                                    (iucn_banked_recalcitrance$category_uncertain_ref == "Wyse and Dickie (2017)") &
                                                                    iucn_banked_recalcitrance$banked == F)])) /
        length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                                   iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                                   iucn_banked_recalcitrance$banked == F)]))*100,2)
# Family   Genus   Order Species
# 2409    1875     770      18


# Numbers of different categories (orthodox, exceptional,) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(as.factor(iucn_banked_recalcitrance$category_uncertain[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                                     iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                                     iucn_banked_recalcitrance$banked == F)]))
# exceptional intermediate     orthodox recalcitrant
#          42           88         4338          960
42 + 960
# 1002

# percentage of everything
round(summary(as.factor(iucn_banked_recalcitrance$category_uncertain[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                                           iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                                           iucn_banked_recalcitrance$banked == F)]))/
        length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F)]))*100,2)

# exceptional intermediate     orthodox recalcitrant
#        0.68         1.43        70.50        15.60

0.68 + 15.60
# 16.28



# unknown
length(unique(iucn_banked_recalcitrance$taxon_name[which((is.na(iucn_banked_recalcitrance$category_uncertain ) |
                                                            iucn_banked_recalcitrance$category_uncertain == "unknown") &
                                                           iucn_banked_recalcitrance$banked == F)]))
# 3939

round(length(unique(iucn_banked_recalcitrance$taxon_name[which((is.na(iucn_banked_recalcitrance$category_uncertain ) |
                                                                  iucn_banked_recalcitrance$category_uncertain == "unknown") &
                                                                 iucn_banked_recalcitrance$banked == F)]))/
        length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F)]))*100,2)

# 12.84


##############################################################################################################
####### Wyse and Dickie species, family, genus, information
##############################################################################################################

# wyse and dickie how many were not predicted?
length(unique(iucn_banked_recalcitrance$taxon_name[which(is.na(iucn_banked_recalcitrance$probability.of.recalcitrance))]))
# 247

# percentage
round(length(unique(iucn_banked_recalcitrance$taxon_name[which(is.na(iucn_banked_recalcitrance$probability.of.recalcitrance))]))/
  length(unique(iucn_banked_recalcitrance$taxon_name))*100,2)
# 3.72

# at taxonomic level
summary(as.factor(iucn_banked_recalcitrance$tax.level[which(!is.na(iucn_banked_recalcitrance$probability.of.recalcitrance))]))
# Family   Genus   Order Species
#   3011    2411     916     121

# percentage
round(summary(as.factor(iucn_banked_recalcitrance$tax.level[which(!is.na(iucn_banked_recalcitrance$probability.of.recalcitrance))])) /
        length(unique(iucn_banked_recalcitrance$taxon_name[which(!is.na(iucn_banked_recalcitrance$probability.of.recalcitrance))]))*100,2)
# Family   Genus   Order Species
#  46.88   37.54   14.26    1.88



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################
### ORTHODOXY FAMILY TREE
###############################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

########################################
#  CONSERVATIVE (CERTAIN)
########################################

tr <- family_tree
numtip <- length(tr$tip.label)

# Get number of species per family that are CR and that are banked
fam_count = iucn_wcvp_matched_countries_tdwg3[,c("family", "category_certain")] %>%
  group_by(family) %>%
  summarize(
    n_orthodox = length(which(category_certain == "orthodox")),
    n_inter = length(which(category_certain == "intermediate")),
    n_exceptional = (length(which(category_certain == "exceptional")) +
                       length(which(category_certain == "recalcitrant"))),
    n_unknown = (length(which(is.na(category_certain))) +
                   length(which(category_certain == "unknown"))),
    p_orthodox = length(which(category_certain == "orthodox"))/n(),
    p_inter = length(which(category_certain == "intermediate"))/n(),
    p_exceptional = (length(which(category_certain == "exceptional")) +
                       length(which(category_certain == "recalcitrant")))/n(),
    p_unknown = (length(which(is.na(category_certain))) +
                   length(which(category_certain == "unknown")))/n(),
    tot = n()
  )


# find out which class is most common (more orthodox or exceptional in family?)
test = data.frame(tr$tip.label) %>% left_join(fam_count,
                                              by = c("tr.tip.label" = "family"))
df = test[,c("p_orthodox","p_exceptional", "p_inter","p_unknown")]
df_with_max_col <- df %>%
  mutate(Max_Column = apply(., 1, function(x) names(.)[which.max(x)]))

# how many are mainly orthodox
length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_orthodox")])
# 74

round(length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_orthodox")])/
  length(which(!is.na(test$tot)))*100,2)
# 33.79

# how many are mainly exceptional
length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_exceptional")])
# 8
test$tr.tip.label[which(df_with_max_col$Max_Column == "p_exceptional")]
# [1] "Fagaceae"         "Elaeocarpaceae"   "Dipterocarpaceae" "Meliaceae"        "Myrtaceae"
# [6] "Myristicaceae"    "Araucariaceae"    "Hymenophyllaceae"

# how many are mainly intermediate
length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_inter")])
# 7
test$tr.tip.label[which(df_with_max_col$Max_Column == "p_inter")]
# [1] "Aspleniaceae"     "Dennstaedtiaceae"
# [3] "Pteridaceae"      "Lindsaeaceae"
# [5] "Marattiaceae"     "Lycopodiaceae"
# [7] "Isoetaceae"



###########################
#  LIKELY (UNCERTAIN)
##########################

tr <- family_tree
numtip <- length(tr$tip.label)

# Get number of species per family that are CR and that are banked
fam_count = iucn_wcvp_matched_countries_tdwg3[,c("family", "category_uncertain")] %>%
  group_by(family) %>%
  summarize(
    n_orthodox = length(which(category_uncertain == "orthodox")),
    n_inter = length(which(category_uncertain == "intermediate")),
    n_exceptional = (length(which(category_uncertain == "exceptional")) +
                       length(which(category_uncertain == "recalcitrant"))),
    n_unknown = (length(which(is.na(category_uncertain))) +
                   length(which(category_uncertain == "unknown"))),
    p_orthodox = length(which(category_uncertain == "orthodox"))/n(),
    p_inter = length(which(category_uncertain == "intermediate"))/n(),
    p_exceptional = (length(which(category_uncertain == "exceptional")) +
                       length(which(category_uncertain == "recalcitrant")))/n(),
    p_unknown = (length(which(is.na(category_uncertain))) +
                   length(which(category_uncertain == "unknown")))/n(),
    tot = n()
  )



# find out which class is most common (more orthodox or exceptional in family?)
test = data.frame(tr$tip.label) %>% left_join(fam_count,
                                              by = c("tr.tip.label" = "family"))
df = test[,c("p_orthodox","p_exceptional", "p_inter","p_unknown")]
df_with_max_col <- df %>%
  mutate(Max_Column = apply(., 1, function(x) names(.)[which.max(x)]))

# how many are mainly orthodox
length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_orthodox")])
# 176

round(length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_orthodox")])/
        length(which(!is.na(test$tot)))*100,2)
# 33.79

# how many are mainly exceptional
length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_exceptional")])
# 12

test$tr.tip.label[which(df_with_max_col$Max_Column == "p_exceptional")]
# [1] "Sapotaceae"       "Fagaceae"         "Rhizophoraceae"   "Chrysobalanaceae" "Elaeocarpaceae"
# [6] "Dipterocarpaceae" "Meliaceae"        "Myrtaceae"        "Myristicaceae"    "Lauraceae"
# [11] "Araucariaceae"    "Hymenophyllaceae"

# how many are mainly intermediate
length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_inter")])
# 7

test$tr.tip.label[which(df_with_max_col$Max_Column == "p_inter")]
# [1] "Aspleniaceae"     "Dennstaedtiaceae" "Pteridaceae"      "Lindsaeaceae"     "Marattiaceae"
# [6] "Lycopodiaceae"    "Isoetaceae"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################################################################
## Orthodoxy in the MSB
##################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# how many in the bank
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))
# 483


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Certain (Conservative)

summary(as.factor(iucn_banked_recalcitrance$category_uncertain[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                       iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                       iucn_banked_recalcitrance$banked == T)]))
# exceptional intermediate     orthodox recalcitrant
#           7            6          257            4
7 + 4
# 11

# percentage of everything
round(summary(as.factor(iucn_banked_recalcitrance$category_uncertain[which(!is.na(iucn_banked_recalcitrance$category_certain ) &
                                                                             iucn_banked_recalcitrance$category_certain != "unknown" &
                                                                             iucn_banked_recalcitrance$banked == T)]))/
        length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))*100,2)

# exceptional intermediate     orthodox recalcitrant
#       1.45         1.24        53.21         0.83

1.45 + 0.83
# 2.28

# unknown
length(unique(iucn_banked_recalcitrance$taxon_name[which((is.na(iucn_banked_recalcitrance$category_certain ) |
                                                            iucn_banked_recalcitrance$category_certain == "unknown") &
                                                           iucn_banked_recalcitrance$banked == T)]))
# 214

round(length(unique(iucn_banked_recalcitrance$taxon_name[which((is.na(iucn_banked_recalcitrance$category_certain ) |
                                                                  iucn_banked_recalcitrance$category_certain == "unknown") &
                                                                 iucn_banked_recalcitrance$banked == T)]))/
        length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))*100,2)

# 44.31


# what species are exceptional?
iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T &
                                             iucn_banked_recalcitrance$category_certain %in% c("recalcitrant","exceptional"))]
# [1] "Aquilaria malaccensis"    "Elaeocarpus bojeri"       "Calamus inermis"          "Grammitis ascensionensis" "Persoonia acerosa"
# [6] "Persoonia pauciflora"     "Eucalyptus dolorosa"      "Eucalyptus imlayensis"    "Eucalyptus recurva"       "Quercus camusiae"
# [11] "Syzygium maire"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Uncertain (likely)


summary(as.factor(iucn_banked_recalcitrance$category_uncertain[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                                       iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                                       iucn_banked_recalcitrance$banked == T)]))
# exceptional intermediate     orthodox recalcitrant
#           7            7          430           16
7 + 16
# 23

# percentage of everything
round(summary(as.factor(iucn_banked_recalcitrance$category_uncertain[which(!is.na(iucn_banked_recalcitrance$category_uncertain ) &
                                                                             iucn_banked_recalcitrance$category_uncertain != "unknown" &
                                                                             iucn_banked_recalcitrance$banked == T)]))/
        length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))*100,2)

# exceptional intermediate     orthodox recalcitrant
#        1.45         1.45        89.03         3.31

1.45 + 3.31
# 4.76

# unknown
length(unique(iucn_banked_recalcitrance$taxon_name[which((is.na(iucn_banked_recalcitrance$category_uncertain ) |
                                                            iucn_banked_recalcitrance$category_uncertain == "unknown") &
                                                           iucn_banked_recalcitrance$banked == T)]))
# 28

round(length(unique(iucn_banked_recalcitrance$taxon_name[which((is.na(iucn_banked_recalcitrance$category_uncertain ) |
                                                                  iucn_banked_recalcitrance$category_uncertain == "unknown") &
                                                                 iucn_banked_recalcitrance$banked == T)]))/
        length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))*100,2)

# 5.8


# what species are exceptional?
iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T &
                                             iucn_banked_recalcitrance$category_uncertain %in% c("recalcitrant","exceptional"))]
# [1] "Reinhardtia paiewonskiana"                  "Aquilaria malaccensis"                      "Masoala madagascariensis"
# [4] "Elaeocarpus bojeri"                         "Chrysophyllum oliviforme subsp. oliviforme" "Acanthophoenix rubra"
# [7] "Hyophorbe lagenicaulis"                     "Hyophorbe vaughanii"                        "Pseudophoenix ekmanii"
# [10] "Calamus inermis"                            "Hyophorbe verschaffeltii"                   "Dictyosperma album"
# [13] "Grammitis ascensionensis"                   "Persoonia acerosa"                          "Persoonia pauciflora"
# [16] "Eucalyptus dolorosa"                        "Eucalyptus imlayensis"                      "Attalea butyracea"
# [19] "Latania verschaffeltii"                     "Eucalyptus recurva"                         "Quercus camusiae"
# [22] "Hedyscepe canterburyana"                    "Syzygium maire"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###########################################################
#   ORTHODOXY COUNTRY DATA
############################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CERTAIN/CONSERVATIVE

# countries with the most unbanked orthodox species
country_data2[order(country_data2$sum_orthodox_unbanked_certain, decreasing = T)[1:10], c("NewCountryName", "sum_orthodox_unbanked_certain")]
#     NewCountryName sum_orthodox_unbanked_certain
# 216  United States                           253
# 131     Madagascar                           123
# 229   South Africa                           104
# 60         Ecuador                            95
# 132         Mexico                            69
# 45        Colombia                            68
# 212       Tanzania                            62
# 97           India                            47
# 162    Philippines                            44
# 39           China                            42
#

# countries with the most unbanked orthodox species
country_data2[order(country_data2$sum_exceptional_unbanked_certain, decreasing = T)[1:10], c("NewCountryName", "sum_exceptional_unbanked_certain")]
#       NewCountryName sum_exceptional_unbanked_certain
# 95         Indonesia                              112
# 162      Philippines                              100
# 146         Malaysia                               51
# 221          Vietnam                               45
# 164 Papua New Guinea                               38
# 39             China                               37
# 30            Brazil                               35
# 97             India                               32
# 216    United States                               24
# 131       Madagascar                               22
#

# UNCERTAIN/LIKELY

# countries with the most unbanked orthodox species
country_data2[order(country_data2$sum_orthodox_unbanked_uncertain, decreasing = T)[1:10], c("NewCountryName", "sum_orthodox_unbanked_uncertain")]
#     NewCountryName sum_orthodox_unbanked_uncertain
# 131     Madagascar                             364
# 216  United States                             309
# 60         Ecuador                             264
# 45        Colombia                             222
# 132         Mexico                             222
# 30          Brazil                             211
# 162    Philippines                             206
# 95       Indonesia                             198
# 212       Tanzania                             152
# 41        Cameroon                             148

# countries with the most unbanked exceptional species
country_data2[order(country_data2$sum_exceptional_unbanked_uncertain, decreasing = T)[1:10], c("NewCountryName", "sum_exceptional_unbanked_uncertain")]
#       NewCountryName sum_exceptional_unbanked_uncertain
# 95         Indonesia                                164
# 162      Philippines                                122
# 164 Papua New Guinea                                 96
# 146         Malaysia                                 75
# 30            Brazil                                 61
# 131       Madagascar                                 59
# 221          Vietnam                                 52
# 122        Sri Lanka                                 46
# 39             China                                 45
# 97             India                                 39


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################################################
#        ACCESS AND BENEFITS SHARING
#############################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Create a matrix of 0 and 1 for the different criteria
str(abs)
abs$st.nagoya<- ifelse(abs$status=="Party", as.numeric(1),as.numeric(0))
abs$st.nfp <- ifelse(abs$ABS_National_Focal_Point_NFP>0,as.numeric(1),as.numeric(0))
abs$st.cna <- ifelse(abs$Competent_National_Authority_CNA>0,as.numeric(1),as.numeric(0))
abs$st.ircc <- ifelse(abs$Internationally_Recognized_Certificates_Compliance_IRCC>0,as.numeric(1),as.numeric(0))

# Create ABS index : Sum the 0-1 value of the four criteria
abs$index.abs<- rowSums(abs[,c("st.nagoya","st.nfp","st.cna","st.ircc")])

#check if country names match between databases
abs$Country<- as.character(abs$Country)

#correct the mismatches
indexes$COUNTRY_ABS = indexes$COUNTRY
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "United Kingdom"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Bermuda"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "St. Helena, Ascension & Tristan da Cunha"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Saint Helena, Ascension, and Tristan da Cunha"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Jersey"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Montserrat"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Cayman Islands"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Anguilla"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Turks and Caicos Islands"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "British Virgin Islands"] = "United Kingdom of Great Britain and Northern Ireland"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Taiwan"] = "China"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Vietnam"] = "Viet Nam"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Syria"] = "Syrian Arab Republic"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "United States"] = "United States of America"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Puerto Rico"] = "United States of America"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Turkey"] = "Türkiye"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Tanzania"] = "United Republic of Tanzania"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Cape Verde"] = "Cabo Verde"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Unknown"] = NA
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "?"] = NA


# merge with the data
indexes = left_join(indexes, abs,
                    by=c("COUNTRY_ABS"="Country"))
unique(indexes$COUNTRY_ABS[is.na(indexes$status)])

# number of accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$index.abs == 0)]))   # 11
length(unique(indexes$ACCESSION[which(indexes$index.abs == 1)]))   # 1055
length(unique(indexes$ACCESSION[which(indexes$index.abs == 2)]))   # 161
length(unique(indexes$ACCESSION[which(indexes$index.abs == 3)]))   # 370
length(unique(indexes$ACCESSION[which(indexes$index.abs == 4)]))   # 870

# Percentage accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$index.abs == 0)]))/length(unique(indexes$ACCESSION))   # 0.00433925
length(unique(indexes$ACCESSION[which(indexes$index.abs == 1)]))/length(unique(indexes$ACCESSION))   # 0.4161736
length(unique(indexes$ACCESSION[which(indexes$index.abs == 2)]))/length(unique(indexes$ACCESSION))   # 0.06351085
length(unique(indexes$ACCESSION[which(indexes$index.abs == 3)]))/length(unique(indexes$ACCESSION))   # 0.1459566
length(unique(indexes$ACCESSION[which(indexes$index.abs == 4)]))/length(unique(indexes$ACCESSION))   # 0.3431953

# Countries with nagoya (regardless of banked)
length(unique(abs$Country[which(abs$st.nfp == 1)]))    # 177
length(unique(abs$Country[which(abs$st.cna == 1)]))    # 79
length(unique(abs$Country[which(abs$st.ircc == 1)]))   # 27
length(unique(abs$Country[which(abs$st.nagoya == 1)])) # 141


# Countries with all 4 in place
unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 4)])[order(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 4)]) )]
# [1] "Argentina"          "Bhutan"             "Bulgaria"
# [4] "Cameroon"           "Dominican Republic" "France"
# [7] "India"              "Kenya"              "Mexico"
# [10] "Peru"               "South Africa"       "Spain"
# [13] "Viet Nam"

# Percentage accessions with specific ABS categories
round(length(unique(indexes$ACCESSION[which(indexes$st.nfp == 1)]))/length(unique(indexes$ACCESSION))*100,2)      # 96.87
round(length(unique(indexes$ACCESSION[which(indexes$st.nagoya == 1)]))/length(unique(indexes$ACCESSION))*100,2)   # 55.45
round(length(unique(indexes$ACCESSION[which(indexes$st.cna == 1)]))/length(unique(indexes$ACCESSION))*100,2)      # 49.23
round(length(unique(indexes$ACCESSION[which(indexes$st.ircc == 1)]))/length(unique(indexes$ACCESSION))*100,2)     # 34.48


# number of accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$st.nfp == 1)]))      # 2444
length(unique(indexes$ACCESSION[which(indexes$st.nagoya == 1)]))   # 1399
length(unique(indexes$ACCESSION[which(indexes$st.cna == 1)]))      # 1242
length(unique(indexes$ACCESSION[which(indexes$st.ircc == 1)]))     # 870


# Countries with nagoya
length(unique(indexes$COUNTRY_ABS[which(indexes$st.nfp == 1)]))    # 67
length(unique(indexes$COUNTRY_ABS[which(indexes$st.nagoya == 1)])) # 50
length(unique(indexes$COUNTRY_ABS[which(indexes$st.cna == 1)]))    # 30
length(unique(indexes$COUNTRY_ABS[which(indexes$st.ircc == 1)]))   # 13
