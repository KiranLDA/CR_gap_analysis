library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################

#load data from previous session
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))

brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))
# make sure only consider predicted that aren't already CR
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched_full_name.csv"))

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
iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in% iucn_wcvp_matched$taxon_name)),]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$wcvp_ipni_id %in%
                                                      iucn_wcvp_matched$wcvp_ipni_id))]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in%
                                                      iucn_wcvp_matched$taxon_name))]

wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

iucn_banked_recalitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))



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

# how many species are in the bank?
length(unique(brahms_wcvp_matched$full_name)) # 46920 to start with
length(unique(brahms_unique_wcvp_matched$full_name)) # 46787 had matches
length(unique(brahms_wcvp_matched$taxon_name)) # 45780 matched
length(unique(brahms_unique_wcvp_matched$taxon_name)) # 45811 names in WCVP

# how many names have been matched from IUCN
length(unique(iucn$scientificName)) # 5702 before matching
length(unique(iucn_wcvp_matched$scientificName)) # 5667 were matched
length(unique(iucn_wcvp_matched$taxon_name)) # 5654 to this many new names
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP"))) # 5618 this many from WCVP
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WFO"))) # 49 from WFO


# how many names have been matched from IUCN predictions
# how many of the predicted IUCN species were matched
length(unique(iucn_predictions$taxon_name[which(iucn_predictions$category == "CR")])) # 4812
iucn_CR_predictions = iucn_$taxon_name[which(iucn_predictions$category == "CR")]
length(unique(iucn_predictions$taxon_name)) # 328553 before matching
length(unique(iucn_predictions_wcvp_matched$scientificName)) # 5667 were matched
length(unique(iucn_predictions_wcvp_matched$taxon_name)) # 5654 to this many new names
length(unique(which(iucn_predictions_wcvp_matched$taxonomic_backbone == "WCVP"))) # 5618 this many from WCVP
length(unique(which(iucn_predictions_wcvp_matched$taxonomic_backbone == "WFO"))) # 49 from WFO


# how many species are the exceptional species
length(unique(exceptional$Species_name)) # 23530 before matching
length(unique(exceptional_wcvp_matched$Species_name)) # 22283 were matched
length(unique(exceptional_wcvp_matched$taxon_name)) # 22298 to this many new names
length(unique(which(exceptional_wcvp_matched$taxonomic_backbone == "WCVP"))) # 22250 this many from WCVP
length(unique(which(exceptional_wcvp_matched$taxonomic_backbone == "WFO"))) # 48 from WFO


# how many accessions
length(unique(brahms_wcvp_matched$AccessionNumber))

length(which(brahms_wcvp_matched$taxonomic_backbone == "WCVP")) # 4812

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

iucn_higher_list

## What years are the IUCN assessments from
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


summary(unique(paste0(iucn_banked_recalitrance$banked, "_",iucn_banked_recalitrance$redlistCriteria == "prediction")))
summary(iucn_banked_recalitrance$banked)



