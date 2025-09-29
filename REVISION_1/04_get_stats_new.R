library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

# wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

wcvp_countries <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

###### Find the CR species in the dataset ##################################################################

# iucn data in the bank with calcualted targets
indexes = read.csv(paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"))

# seedbank data
brahms_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_wcvp_matched_full_name_infra.csv"))


# iucn species and their categories
iucn_banked_recalcitrance <- read.csv(paste0(basepath, "revision_1/spp_banked_recalcitrant.csv"))
iucn_banked_recalcitrance$higher[iucn_banked_recalcitrance$higher == "A"] = "Angiosperms"


# Access and benefits sharing data
abs <- read.csv(paste0(basepath,"ABSCH-Country-List_03_07_24.csv"))


#load data from previous session
iucn <- read.table(paste0(basepath, "revision_1/redlist_2025-1_downloaded_17_09_2025/assessments.csv" ),
                   sep = ",", quote = "\"",
                   dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"))
iucn_wcvp_matched$higher[which(iucn_wcvp_matched$higher == "A")] = "Angiosperms"

# make sure only consider predicted that aren't already CR
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_unique_wcvp_matched_full_name_infra.csv"))

exceptional <- read.csv(paste0(basepath, "revision_1/list_exceptional_status_18_09_2025.csv"))
exceptional_wcvp_matched = read.csv(paste0(basepath,"revision_1/exceptional_unique_wcvp_matched.csv"))

iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_predictions_wcvp_matched.csv"))

# keep only the CR ones
iucn_CR_predictions = iucn_predictions[which(iucn_predictions$category == "CR"),]
iucn_CR_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]

# keep only the ones that aren't already in IUCN
iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in% iucn_wcvp_matched$taxon_name)),]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$wcvp_ipni_id %in%
                                                      iucn_wcvp_matched$wcvp_ipni_id))]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in%
                                                      iucn_wcvp_matched$taxon_name))]


CR_msbp <- read.csv(paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"))
CR_msbp


##############################################################
###       GET some stats
##############################################################


##############################################################
###      IUCN
##############################################################


# how many names have been matched from IUCN
length(unique(iucn$scientificName)) #6520 before matching #5702
length(unique(iucn_wcvp_matched$scientificName)) # 6480  were matched  #5667
length(unique(iucn$scientificName)) - length(unique(iucn_wcvp_matched$scientificName)) # 40
test = iucn[which(!(iucn$scientificName %in% iucn_wcvp_matched$scientificName)),]
length(unique(iucn_wcvp_matched$taxon_name)) #6465 #5654 to this many new names
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP"))) # 6399 # 5618 this many from WCVP
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WFO"))) # 66 #49 from WFO


# find how many of each family there are in IUCN
iucn_higher_list  = iucn_wcvp_matched[which(duplicated(iucn_wcvp_matched$taxon_name)==F),] %>%
  group_by(higher) %>%
  tally()

iucn_higher_list
#   higher              n
# 1 Angiosperms      6223
# 2 Bryophyta          27
# 3 Ferns              99
# 4 Gymnosperms        89
# 5 Lycophytes         16
# 6 Marchantiophyta    10
# 7 Polypodiophyta      1

sum(iucn_higher_list$n) # 6465

## What years are the IUCN assessments from
length(which(iucn_banked_recalcitrance$yearPublished < 2000)) #294
length(which(iucn_banked_recalcitrance$yearPublished >= 2000 & iucn_banked_recalcitrance$yearPublished < 2010)) #467
length(which(iucn_banked_recalcitrance$yearPublished >= 2010 & iucn_banked_recalcitrance$yearPublished < 2020)) #2036
length(which(iucn_banked_recalcitrance$yearPublished[which(iucn_banked_recalcitrance$redlistCriteria != "prediction")] >= 2020)) # 3711
# because predictions are all from 2024

length(wcvp$taxon_status == "Accepted") # 1440076


# how many are in wcvp
length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) # 6399 # 5595
length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) / length(unique(iucn$scientificName)) # 0.9814417 # 0.9812347
length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) # 66 # 64
length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) / length(unique(iucn$scientificName)) # 0.0101227  # 0.01122413
length(unique(iucn$scientificName)) - length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) - length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP"))
# 55 # 43 not matched to either WCVP or WFO
55 / length(unique(iucn$scientificName)) # 0.008435583 # 0.007541174

summary(as.factor(unique(paste0(iucn_banked_recalcitrance$banked, "_",iucn_banked_recalcitrance$redlistCriteria == "prediction"))))
summary(iucn_banked_recalcitrance$banked)
#    Mode   FALSE    TRUE
# logical    6336     403

########################################
# BANKED

#####################################
### % placed names     ##############

##### CR in the seed bank
length(unique(indexes$ACCESSION)) # 2566 #2348

# total names
length(unique(indexes$taxon_name)) # 495 #372
length(unique(indexes$taxon_name[which(indexes$accepted_name == T)])) #  477 #366

length(unique(indexes$SPECIES)) # 508 #379
length(unique(indexes$SPECIES[indexes$accepted_name == T])) # 478 #366


#####################
# Families
length(unique(indexes$family[!duplicated(indexes$taxon_name)])) #97 #82
# orders
length(unique(indexes$order[!duplicated(indexes$taxon_name)])) #41 #38
# genera
summary(as.factor(indexes$higher[!duplicated(indexes$taxon_name)]))
 # Angiosperms       Ferns Gymnosperms  Lycophytes
 #         475           6          12           2
 # old     361           5           6           2

summary(as.factor(iucn_banked_recalcitrance$higher[which(!duplicated(iucn_banked_recalcitrance$taxon_name) &
                                                           iucn_banked_recalcitrance$banked)]))
# Angiosperms       Ferns Gymnosperms  Lycophytes
#     378           6          11           2
# old 359           5           7           2



# compare to total IUCN/PRED together
# Families
length(unique(iucn_banked_recalcitrance$family[!duplicated(iucn_banked_recalcitrance$taxon_name)])) #247 #238
# orders
length(unique(iucn_banked_recalcitrance$order[!duplicated(iucn_banked_recalcitrance$taxon_name)])) #66 #64
# genera
summary(as.factor(iucn_banked_recalcitrance$higher[!duplicated(iucn_banked_recalcitrance$taxon_name)]))
# Angiosperms       Bryophyta           Ferns     Gymnosperms      Lycophytes  Marchantiophyta  Polypodiophyta
# 6420              27                  99        89              16           10               1
# 5613              27                  95        72              14           10

# angiosperms
359/5535              # 0.06485998
# gymnosperms
6/72                  # 0.08333333
# ferns
5/92                  # 0.05434783
# lycophytes
2/14                  # 0.1428571

(summary(as.factor(indexes$higher[!duplicated(indexes$taxon_name)])) /
    summary(as.factor(iucn_banked_recalcitrance$higher[!duplicated(iucn_banked_recalcitrance$taxon_name)])) )

summary(as.factor(iucn_banked_recalcitrance$higher[which(!duplicated(iucn_banked_recalcitrance$taxon_name) &
                    (iucn_banked_recalcitrance$redlistCriteria == "prediction"))]))
