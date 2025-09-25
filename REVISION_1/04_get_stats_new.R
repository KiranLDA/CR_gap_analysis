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
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"))

# make sure only consider predicted that aren't already CR
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_unique_wcvp_matched_full_name_infra.csv"))

# brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched.csv"))
# brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched.csv"))
# exceptional_wcvp_matched = read.csv(paste0(basepath,"exceptional_wcvp_matched.csv"))
exceptional <- read.csv(paste0(basepath, "pence_appendix1.csv"))
exceptional_wcvp_matched = read.csv(paste0(basepath,"revision_1/exceptional_unique_wcvp_matched.csv"))

iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_predictions_wcvp_matched.csv"))

iucn_CR_predictions = iucn_predictions[which(iucn_predictions$category == "CR"),]
# keep only the CR ones
iucn_CR_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]
# # keep only the ones that aren't already in IUCN
iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in% iucn_wcvp_matched$taxon_name)),]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$wcvp_ipni_id %in%
                                                      iucn_wcvp_matched$wcvp_ipni_id))]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in%
                                                      iucn_wcvp_matched$taxon_name))]


CR_msbp <- read.csv(paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"))
CR_msbp

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


##############################################################
###       GET some stats
##############################################################


##############################################################
###      IUCN
##############################################################


# how many names have been matched from IUCN
length(unique(iucn$scientificName)) # 5702 before matching
length(unique(iucn_wcvp_matched$scientificName)) # 5666 #5667 were matched
length(unique(iucn$scientificName)) - length(unique(iucn_wcvp_matched$scientificName))
test = iucn[which(!(iucn$scientificName %in% iucn_wcvp_matched$scientificName)),]
length(unique(iucn_wcvp_matched$taxon_name)) # 5667 #5654 to this many new names
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP"))) # 5595 # 5618 this many from WCVP
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WFO"))) # 64 #49 from WFO

iucn_wcvp_matched$higher[which(iucn_wcvp_matched$higher == "A")] = "Angiosperms"

# find how many of each family there are in IUCN
iucn_higher_list  = iucn_wcvp_matched[which(duplicated(iucn_wcvp_matched$taxon_name)==F),] %>%
  group_by(higher) %>%
  tally()

iucn_higher_list
# higher               n
# 1 Angiosperms        5449
# 2 Bryophyta          27
# 3 Ferns              95
# 4 Gymnosperms        72
# 5 Lycophytes         14
# 6 Marchantiophyta    10

sum(iucn_higher_list$n)

## What years are the IUCN assessments from
length(which(iucn_banked_recalcitrance$yearPublished < 2000)) #337
length(which(iucn_banked_recalcitrance$yearPublished >= 2000 & iucn_banked_recalcitrance$yearPublished < 2010)) #474
length(which(iucn_banked_recalcitrance$yearPublished >= 2010 & iucn_banked_recalcitrance$yearPublished < 2020)) #1980
length(which(iucn_banked_recalcitrance$yearPublished[which(iucn_banked_recalcitrance$redlistCriteria != "prediction")] >= 2020)) #2897
# because predictions are all from 2024

length(wcvp$taxon_status == "Accepted")


# how many are in wcvp
length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) # 5595
length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) / length(unique(iucn$scientificName)) # 0.9812347
length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) # 64
length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) / length(unique(iucn$scientificName)) # 0.01122413
length(unique(iucn$scientificName)) - length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) - length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP"))
# 43 not matched to either WCVP or WFO
43 / length(unique(iucn$scientificName)) # 0.007541174

summary(as.factor(unique(paste0(iucn_banked_recalcitrance$banked, "_",iucn_banked_recalcitrance$redlistCriteria == "prediction"))))
summary(iucn_banked_recalcitrance$banked)
#            FALSE    TRUE
# logical    5480     374

########################################
# BANKED

#####################################
### % placed names     ##############

##### CR in the seed bank
length(unique(indexes$ACCESSION)) # 2351 #2348

# total names
length(unique(indexes$taxon_name)) # 374 #372
length(unique(indexes$taxon_name[which(indexes$accepted_name == T)])) # 368 #366

length(unique(indexes$SPECIES)) # 380 #379
length(unique(indexes$SPECIES[indexes$accepted_name == T])) # 367 #366


#####################
# Families
length(unique(indexes$family[!duplicated(indexes$taxon_name)])) #82
# orders
length(unique(indexes$order[!duplicated(indexes$taxon_name)])) #39 #38
# genera
summary(as.factor(indexes$higher[!duplicated(indexes$taxon_name)]))
 # Angiosperms       Ferns Gymnosperms  Lycophytes
 #         361           5           6           2
summary(as.factor(iucn_banked_recalcitrance$higher[which(!duplicated(iucn_banked_recalcitrance$taxon_name) &
                                                           iucn_banked_recalcitrance$banked)]))
# Angiosperms       Ferns Gymnosperms  Lycophytes
# 359           5           7           2

# what is the gymnosperm that is different
iucn_banked_recalcitrance[which(iucn_banked_recalcitrance$taxon_name == "Widdringtonia cedarbergensis"),]




# compare to total IUCN/PRED together
# Families
length(unique(iucn_banked_recalcitrance$family[!duplicated(iucn_banked_recalcitrance$taxon_name)])) #241 #238
# orders
length(unique(iucn_banked_recalcitrance$order[!duplicated(iucn_banked_recalcitrance$taxon_name)])) #63 #64
# genera
summary(as.factor(iucn_banked_recalcitrance$higher[!duplicated(iucn_banked_recalcitrance$taxon_name)]))
# Angiosperms       Bryophyta           Ferns     Gymnosperms      Lycophytes  Marchantiophyta
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
