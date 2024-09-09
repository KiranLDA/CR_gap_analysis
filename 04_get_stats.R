library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

###### Find the CR species in the dataset ##################################################################

# iucn data in the bank with calcualted targets
indexes = read.csv(paste0(basepath,"iucn_brahms_indexes_targets.csv"))

# seedbank data
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))


# iucn species and their categories
iucn_banked_recalcitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))

# Access and benefits sharing data
abs <- read.csv(paste0(basepath,"ABSCH-Country-List_03_07_24.csv"))


#load data from previous session
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))

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


CR_msbp <- read.csv(paste0(basepath,"iucn_brahms_indexes_targets.csv"))
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
# iucn_CR_predictions = iucn_$taxon_name[which(iucn_predictions$category == "CR")]
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
length(unique(brahms_wcvp_matched$AccessionNumber)) # 197934

length(which(brahms_wcvp_matched$taxonomic_backbone == "WCVP")) # 197685

#how many collections?
length(unique(brahms_wcvp_matched$AccessionNumber)) # 197934

# find the MSB species that are CR endangered
length(unique(CR_msbp$taxon_name)) # 372
length(unique(CR_msbp$taxon_name[which(CR_msbp$taxon_name == "Atocion compactum")])) # 1
length(unique(CR_msbp$taxon_name[which(CR_msbp$taxon_name != "Atocion compactum")])) # 371

# total accessions
nrow(CR_msbp) #2355
# how many accessions that are not CR_pred
length(CR_msbp$taxon_name[which(CR_msbp$taxon_name != "Atocion compactum")]) # 2345
length(CR_msbp$taxon_name[which(CR_msbp$taxon_name == "Atocion compactum")]) # 10


# brahms_wcvp_matched$CR = brahms_wcvp_matched$wcvp_accepted_id %in% iucn_wcvp_matched$wcvp_accepted_id
# summary(brahms_wcvp_matched$CR)
#   Mode   FALSE    TRUE
# logical  195715    2221

# how many species in the bank are CR
# length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$CR)])) # 371

# subset the CR data
brahms_CR = CR_msbp#brahms_wcvp_matched[brahms_wcvp_matched$CR,]


# number of IUCN listed species
# length(unique(iucn_wcvp_matched$wcvp_accepted_id)) #5645
length(unique(iucn_wcvp_matched$taxon_name)) #5654

# of the CR and CRpred species which one are in the bank, and which ones not?
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria == "prediction")]))
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria == "prediction")]))
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria != "prediction")]))
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria != "prediction")]))

# make sure they all add up
total_CR_CRpred = (length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria == "prediction")])) +
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria == "prediction")])) +
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria != "prediction")])) +
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria != "prediction")]))
)
total_CR_CRpred
total_CR = (length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria != "prediction")])) +
              length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria != "prediction")])))
total_CR

# iucn_wcvp_matched$banked = iucn_wcvp_matched$wcvp_accepted_id %in% brahms_wcvp_matched$wcvp_accepted_id
# summary(iucn_wcvp_matched$banked)
# #    Mode   FALSE    TRUE
# # logical    5295     372

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
iucn_higher_list  = iucn_wcvp_matched[which(duplicated(iucn_wcvp_matched$taxon_name)==F),] %>%
  group_by(higher) %>%
  tally()

iucn_higher_list
sum(iucn_higher_list$n)

## What years are the IUCN assessments from
length(which(iucn_banked_recalcitrance$yearPublished < 2000)) #334
length(which(iucn_banked_recalcitrance$yearPublished >= 2000 & iucn_banked_recalcitrance$yearPublished < 2010)) #470
length(which(iucn_banked_recalcitrance$yearPublished >= 2010 & iucn_banked_recalcitrance$yearPublished < 2020)) #1970
length(which(iucn_banked_recalcitrance$yearPublished[which(iucn_banked_recalcitrance$redlistCriteria != "prediction")] >= 2020)) #2893
 # because predictions are all from 2024

length(wcvp$taxon_status == "Accepted")
length(wcvp$taxon_status == "Accepted")

# how many are in wcvp
length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) / 5705
length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) / 5705
5705 - length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) - length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP"))


summary(unique(paste0(iucn_banked_recalcitrance$banked, "_",iucn_banked_recalcitrance$redlistCriteria == "prediction")))
summary(iucn_banked_recalcitrance$banked)



#####################################
### % placed names     ##############

##### CR in the seed bank
length(unique(indexes$ACCESSION)) # 2351

# total names
length(unique(indexes$taxon_name)) # 372
length(unique(indexes$taxon_name[which(indexes$accepted_name ==T)])) # 366

length(unique(indexes$SPECIES)) #380
length(unique(indexes$SPECIES[indexes$accepted_name ==T])) #380

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# unplaced
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(unique(indexes$SPECIES[which(is.na(indexes$wcvp_accepted_id))]))
length(unique(indexes$SPECIES[which(is.na(indexes$wcvp_accepted_id))]))/length(unique(indexes$SPECIES[indexes$accepted_name ==T]))
# 0.002724796

length(unique(iucn_wcvp_matched$scientificName[which(is.na(iucn_wcvp_matched$wcvp_accepted_id))]))/length(unique(iucn_wcvp_matched$scientificName[iucn_wcvp_matched$accepted_name ==T]))
# 0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# non homotypic
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
above1 = length(unique(indexes$SPECIES[which(indexes$wcvp_status == "Synonym")[!(which(indexes$wcvp_status == "Synonym") %in%
                                                                                   which(indexes$wcvp_homotypic))]]))
below1 = length(unique(indexes$SPECIES[indexes$accepted_name ==T]))
above1/below1
# 0.01907357


above2 = length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$wcvp_status == "Synonym")[!(which(iucn_wcvp_matched$wcvp_status == "Synonym") %in%
                                                                                   which(iucn_wcvp_matched$wcvp_homotypic))]]))
below2 = length(unique(iucn_wcvp_matched$scientificName[iucn_wcvp_matched$accepted_name ==T]))
above2/below2
# 0.03331408


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# accepted
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
above1 = length(unique(indexes$SPECIES[which(indexes$wcvp_status == "Accepted")[!(which(indexes$wcvp_status == "Accepted") %in%
                                                                                   which(indexes$wcvp_homotypic))]]))
below1 = length(unique(indexes$SPECIES[indexes$accepted_name ==T]))
above1/below1
# 0.01907357


above2 = length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$wcvp_status == "Accepted")[!(which(iucn_wcvp_matched$wcvp_status == "Accepted") %in%
                                                                                                              which(iucn_wcvp_matched$wcvp_homotypic))]]))
below2 = length(unique(iucn_wcvp_matched$scientificName[iucn_wcvp_matched$accepted_name ==T]))
above2/below2
# 0.03331408


##################################################################
# How many of IUCN in bank are storable, etc...

####################################################################################
### orthodoxy of species in bank
####################################################################################
#
# which(is.na(iucn_banked_recalcitrance$taxon_name))
#
iucn_banked_recalcitrance$banked_category = NA
iucn_banked_recalcitrance$banked_category[iucn_banked_recalcitrance$probability.of.recalcitrance <= 0.25] = "orthodox"
iucn_banked_recalcitrance$banked_category[iucn_banked_recalcitrance$probability.of.recalcitrance >= 0.75] = "recalcitrant"
iucn_banked_recalcitrance$banked_category[iucn_banked_recalcitrance$probability.of.recalcitrance < 0.75 &
                                           iucn_banked_recalcitrance$probability.of.recalcitrance > 0.25] = "intermediate"

iucn_banked_recalcitrance$probability.of.recalcitrance[which(iucn_banked_recalcitrance$banked == T)]

# get proportions
# prop = iucn_banked_recalcitrance$category[!is.na(iucn_banked_recalcitrance$banked_category)]
prop = iucn_banked_recalcitrance$category[which(iucn_banked_recalcitrance$banked == T)]
summary(factor(prop))
summary(factor(prop))/length(prop)

#look at which species may not do well?
prop = iucn_banked_recalcitrance[,c("taxon_name", "storBehav","banked_category", "probability.of.recalcitrance")] # "banked_category","banked_recalcitrance.y")]
prop = prop[!is.na(prop$banked_category),]
prop[prop$banked_category == "intermediate",]
prop[prop$banked_category == "recalcitrant",]

prop = iucn_banked_recalcitrance[which(iucn_banked_recalcitrance$category == "banked"),
                                c("taxon_name", "banked_category", "probability.of.recalcitrance")] #"banked_recalcitrance.y")]
prop[is.na(prop$banked_category),"taxon_name"]
nrow(prop[is.na(prop$banked_category),])

#####################################################
##  accession viability
#####################################################

nrow(indexes[which(indexes$LASTTEST != "  /  /    "),])
nrow(indexes)
# proportioin of accessions tested
nrow(indexes[which(indexes$LASTTEST != "  /  /    "),])/nrow(indexes)


# of those tested, what is their quality (75% viability)
## Best ever test
length(which(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    ")] >= 75))
length(which(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    ")] < 75))
length(which(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    ")] >= 75))/nrow(indexes[which(indexes$LASTTEST != "  /  /    "),])
length(which(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    ")] < 75))/nrow(indexes[which(indexes$LASTTEST != "  /  /    "),])

length(which(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    ")] == 100))
length(which(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    ")] == 100))/nrow(indexes[which(indexes$LASTTEST != "  /  /    "),])

length(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    " & indexes$BESTEVER < 75)])

length(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    " & indexes$BESTEVER == 0 )])
summary(indexes$BESTEVER[which(indexes$LASTTEST != "  /  /    " & indexes$BESTEVER < 75 )])


## last test
length(which(indexes$BESTLAST[which(indexes$LASTTEST != "  /  /    ")] >= 75))
length(which(indexes$BESTLAST[which(indexes$LASTTEST != "  /  /    ")] < 75))
length(which(indexes$BESTLAST[which(indexes$LASTTEST != "  /  /    ")] >= 75))/nrow(indexes[which(indexes$LASTTEST != "  /  /    "),])
length(which(indexes$BESTLAST[which(indexes$LASTTEST != "  /  /    ")] < 75))/nrow(indexes[which(indexes$LASTTEST != "  /  /    "),])


###############################################
# How many countries have CR in MSBP
##########################################

length(unique(indexes$COUNTRY))
length(unique(brahms_wcvp_matched$CountryName))
length(unique(indexes$COUNTRY))/length(unique(brahms_wcvp_matched$CountryName))


summary(factor(indexes$DISTPOLICY))

# number with no policy
length(which(indexes$DISTPOLICY == ""))

##########################################################
# ABS in CR species
#######################################################

# Create a matrix of 0 and 1 for the different criteria
str(abs)
abs$st.nagoya<- ifelse(abs$status=="Party", as.numeric(1),as.numeric(0))
abs$st.nfp <- ifelse(abs$ABS_National_Focal_Point_NFP>0,as.numeric(1),as.numeric(0))
abs$st.cna <- ifelse(abs$Competent_National_Authority_CNA>0,as.numeric(1),as.numeric(0))
abs$st.ircc <- ifelse(abs$Internationally_Recognized_Certificates_Compliance_IRCC>0,as.numeric(1),as.numeric(0))

##Create ABS index
#Sum the 0-1 value of the four criteria
abs$index.abs<- rowSums(abs[,c("st.nagoya","st.nfp","st.cna","st.ircc")])

#check if country names match between databases
abs$Country<- as.character(abs$Country)

# test = left_join(indexes[,c("ACCESSION", "COUNTRY")], abs, by=c("COUNTRY"="Country"))
# unique(test$COUNTRY[is.na(test$status)])
#
# test2 = left_join(abs, indexes[,c("ACCESSION", "COUNTRY")], by=c("Country" = "COUNTRY"))
# unique(test2$Country[is.na(test2$ACCESSION)])

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
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Turkey"] = "Türkiye"
indexes$COUNTRY_ABS[indexes$COUNTRY_ABS == "Tanzania"] = "United Republic of Tanzania"

# merge with the data
indexes = left_join(indexes, abs,
                 by=c("COUNTRY_ABS"="Country"))
unique(indexes$COUNTRY_ABS[is.na(indexes$status)])

# Countries with nagoya
length(unique(indexes$COUNTRY_ABS[which(indexes$st.nagoya == 1)])) # 43
length(unique(indexes$COUNTRY_ABS[which(indexes$st.nfp == 1)]))    # 58
length(unique(indexes$COUNTRY_ABS[which(indexes$st.cna == 1)]))    # 25
length(unique(indexes$COUNTRY_ABS[which(indexes$st.ircc == 1)]))   # 12

# number of countries with specific ABS categories
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 0)]))   # 1
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 1)]))   # 14
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 2)]))   # 20
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 3)]))   # 12
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 4)]))   # 12

# Percentage countries with specific ABS categories
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 0)]))/length(unique(indexes$COUNTRY_ABS))   # 0.01639344
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 1)]))/length(unique(indexes$COUNTRY_ABS))   # 0.2295082
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 2)]))/length(unique(indexes$COUNTRY_ABS))   # 0.3278689
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 3)]))/length(unique(indexes$COUNTRY_ABS))   # 0.1967213
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 4)]))/length(unique(indexes$COUNTRY_ABS))   # 0.1967213

# number of accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$index.abs == 0)]))   # 9
length(unique(indexes$ACCESSION[which(indexes$index.abs == 1)]))   # 1003
length(unique(indexes$ACCESSION[which(indexes$index.abs == 2)]))   # 93
length(unique(indexes$ACCESSION[which(indexes$index.abs == 3)]))   # 319
length(unique(indexes$ACCESSION[which(indexes$index.abs == 4)]))   # 900

# Percentage accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$index.abs == 0)]))/length(unique(indexes$ACCESSION))   # 0.003828158
length(unique(indexes$ACCESSION[which(indexes$index.abs == 1)]))/length(unique(indexes$ACCESSION))   # 0.426627
length(unique(indexes$ACCESSION[which(indexes$index.abs == 2)]))/length(unique(indexes$ACCESSION))   # 0.03955764
length(unique(indexes$ACCESSION[which(indexes$index.abs == 3)]))/length(unique(indexes$ACCESSION))   # 0.1356869
length(unique(indexes$ACCESSION[which(indexes$index.abs == 4)]))/length(unique(indexes$ACCESSION))   # 0.3828158


# number of accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$st.nagoya == 1)]))   # 1311
length(unique(indexes$ACCESSION[which(indexes$st.nfp == 1)]))   # 2315
length(unique(indexes$ACCESSION[which(indexes$st.cna == 1)]))   # 1220
length(unique(indexes$ACCESSION[which(indexes$st.ircc == 1)]))   # 900


# Percentage accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$st.nagoya == 1)]))/length(unique(indexes$ACCESSION))   # 0.557635
length(unique(indexes$ACCESSION[which(indexes$st.nfp == 1)]))/length(unique(indexes$ACCESSION))   # 0.9846874
length(unique(indexes$ACCESSION[which(indexes$st.cna == 1)]))/length(unique(indexes$ACCESSION))   # 0.5189281
length(unique(indexes$ACCESSION[which(indexes$st.ircc == 1)]))/length(unique(indexes$ACCESSION))   # 0.3828158



##########################################################
# ABS in all MSB species
#######################################################

# Create a matrix of 0 and 1 for the different criteria
# str(abs)
# abs$st.nagoya<- ifelse(abs$status=="Party", as.numeric(1),as.numeric(0))
# abs$st.nfp <- ifelse(abs$ABS_National_Focal_Point_NFP>0,as.numeric(1),as.numeric(0))
# abs$st.cna <- ifelse(abs$Competent_National_Authority_CNA>0,as.numeric(1),as.numeric(0))
# abs$st.ircc <- ifelse(abs$Internationally_Recognized_Certificates_Compliance_IRCC>0,as.numeric(1),as.numeric(0))
#
# ##Create ABS index
# #Sum the 0-1 value of the four criteria
# abs$index.abs<- rowSums(abs[,c("st.nagoya","st.nfp","st.cna","st.ircc")])
#
# #check if country names match between databases
# abs$Country<- as.character(abs$Country)

test = left_join(brahms_wcvp_matched[,c("AccessionNumber", "CountryName")], abs, by=c("CountryName"="Country"))
unique(test$CountryName[is.na(test$status)])

test2 = left_join(abs, brahms_wcvp_matched[,c("AccessionNumber", "AccessionNumber")], by=c("Country" = "AccessionNumber"))
unique(test2$Country[is.na(test2$AccessionNumber)])

#correct the mismatches
brahms_wcvp_matched$COUNTRY_ABS = brahms_wcvp_matched$CountryName
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "UK"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Bermuda"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "St. Helena, Ascension & Tristan da Cunha"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Saint Helena, Ascension, and Tristan da Cunha"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Jersey"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Montserrat"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Cayman Islands"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Anguilla"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Turks and Caicos Islands"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Turks & Caicos Is."] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "British Virgin Islands"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "British Virgin Is."] = "United Kingdom of Great Britain and Northern Ireland"

brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "St. Lucia"] = "Saint Lucia" #"United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Falkland Is."] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Czech-Republic"] = "Czechia"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "South Korea"] = "Republic of Korea"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Bosnia & Herzegovina"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Netherlands"] = "Netherlands (Kingdom of the)"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Taiwan"] = "China"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Vietnam"] = "Viet Nam"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Syria"] = "Syrian Arab Republic"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "USA"] = "United States of America"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Turkey"] = "Türkiye"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Tanzania"] = "United Republic of Tanzania"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Puerto Rico"] = "United States of America"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Iran"] = "Iran (Islamic Republic of)"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Bolivia"] = "Bolivia (Plurinational State of)"


brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Macedonia"] = "North Macedonia"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Congo, DRC"] = "Congo"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Norfolk Island"] = "Australia"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Reunion"] = "France"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Swaziland"] = "Eswatini"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Venezuela"] = "Venezuela (Bolivarian Republic of)"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Cayman Is."] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Christmas I."] = "Australia"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Czech Republic"] = "Czechia"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Ivory Coast"] = "Côte d'Ivoire"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Jersey"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Unknown"] = NA
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Heard Isl"] = "Australia"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Antarctica"] = NA
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Bermuda"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Yugoslavia"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Anguilla"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Guernsey"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "New Caledonia"] = "France"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Palestinian Territory, Occupied"] = "State of Palestine"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Kosovo"] = "Serbia"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "FYROM"] = NA
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Hong Kong"] = "China"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Martinique"] = "France"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Isle of Man"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "French Polynesia"] = "France"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Sao Tome & Principe"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "French Guiana"] = "France"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Gibraltar"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Moldova"] = "Republic of Moldova"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "San-Marino"] = "San Marino"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Pitcairn"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Laos"] = "Lao People's Democratic Republic"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Trinidad & Tobago"] = "Trinidad and Tobago"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "British Indian Ocean Territory"] = "United Kingdom of Great Britain and Northern Ireland"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Southern America"] = NA
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Auckland Island"] = "New Zealand"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Cape Verde"] = "Cabo Verde"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "French Southern Territories"] = "France"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Bosnia-Herzegovina"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Azerbaidjan"] = "Azerbaijan"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Solomon Is."] = "Solomon Islands"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Brasil"] = "Brazil"



# merge with the data
brahms_wcvp_matched = left_join(brahms_wcvp_matched, abs,
                    by=c("COUNTRY_ABS"="Country"))
# unique(brahms_wcvp_matched[is.na(test$status)])

# Countries with nagoya
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$st.nagoya == 1)])) # 116
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$st.nfp == 1)]))    # 143
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$st.cna == 1)]))    # 67
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$st.ircc == 1)]))   # 23

# number of countries with specific ABS categories
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 0)]))   # 9
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 1)]))   # 31
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 2)]))   # 53
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 3)]))   # 40
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 4)]))   # 23

# Percentage countries with specific ABS categories
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 0)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.05660377
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 1)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.1949686
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 2)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.3333333
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 3)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.2515723
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 4)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.1446541

# number of AccessionNumbers with specific ABS categories
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 0)]))   # 2370
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 1)]))   # 72247
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 2)]))   # 27601
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 3)]))   # 38187
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 4)]))   # 57286

# Percentage AccessionNumbers with specific ABS categories
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 0)]))/length(unique(brahms_wcvp_matched$AccessionNumber))   # 0.01197369
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 1)]))/length(unique(brahms_wcvp_matched$AccessionNumber))   # 0.3650055
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 2)]))/length(unique(brahms_wcvp_matched$AccessionNumber))   # 0.1394455
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 3)]))/length(unique(brahms_wcvp_matched$AccessionNumber))   # 0.1929279
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$index.abs == 4)]))/length(unique(brahms_wcvp_matched$AccessionNumber))   # 0.2894197


# number of AccessionNumbers with specific ABS categories
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$st.nagoya == 1)]))   # 115992
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$st.nfp == 1)]))      # 195275
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$st.cna == 1)]))      # 102601
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$st.ircc == 1)]))     # 57286


# Percentage AccessionNumbers with specific ABS categories
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$st.nagoya == 1)]))/length(unique(brahms_wcvp_matched$AccessionNumber))  # 0.5860135
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$st.nfp == 1)]))/length(unique(brahms_wcvp_matched$AccessionNumber))     # 0.9865662
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$st.cna == 1)]))/length(unique(brahms_wcvp_matched$AccessionNumber))     # 0.5183597
length(unique(brahms_wcvp_matched$AccessionNumber[which(brahms_wcvp_matched$st.ircc == 1)]))/length(unique(brahms_wcvp_matched$AccessionNumber))    # 0.2894197


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  Proposed  targets per CR plant species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get the data
indexes$plants_sampled = as.numeric(indexes$PLANTSAMP)
indexes$plants_sampled[site_counts$PLANTSAMP == "11-100"] = 50
indexes$plants_sampled[site_counts$PLANTSAMP == "100-1000"] = 100
indexes$plants_sampled[site_counts$PLANTSAMP == "25-50"] = 50
indexes$plants_sampled[site_counts$PLANTSAMP == ">100"] = 100
indexes$plants_sampled[site_counts$PLANTSAMP == "200-500"] = 200
indexes$plants_sampled[is.na(indexes$plants_sampled)] = 0
indexes$ADJSTCOUNT[is.na(indexes$ADJSTCOUNT)] = 0

# per species data
rm(spp_count)
spp_count = indexes[,c("taxon_name", "ADJSTCOUNT", "plants_sampled", "redlistCriteria")] %>%
  group_by(taxon_name) %>%
  summarize(
    accessions = n(), # Number of times the species appears
    summed_count = sum(ADJSTCOUNT), # Sum of seed count per species
    summed_sampled = sum(plants_sampled) # Sum of plants sampled per species
  )

to_add = indexes[,c("taxon_name", "redlistCriteria")]
to_add = to_add[duplicated(to_add$taxon_name)==FALSE,]
spp_count = spp_count %>% left_join(to_add,
                                    by = "taxon_name")

########################################################
# Get targets
########################################################

spp_count$need_more = ifelse(spp_count$summed_count  < 250,
                            TRUE,FALSE)

length(which(spp_count$need_more))
length(which(spp_count$need_more))/nrow(spp_count)


spp_count$bankable = ifelse(spp_count$summed_count >= 250 & spp_count$summed_count < 1050,
                             TRUE,FALSE)

length(which(spp_count$bankable))
length(which(spp_count$bankable))/nrow(spp_count)



spp_count$restorable = ifelse(spp_count$summed_count >= 1050,
                            TRUE,FALSE)

length(which(spp_count$restorable))
length(which(spp_count$restorable))/nrow(spp_count)


spp_count$Target_1a = ifelse(spp_count$summed_count >= 1050,
                               TRUE,FALSE)
spp_count$Target_1a[is.na(spp_count$Target_1a)] = FALSE


# from at least 50 different plants stored ex situ
spp_count$Target_1b = ifelse((as.numeric(spp_count$summed_sampled) >= 50),
                               TRUE,
                               FALSE)
spp_count$Target_1b[is.na(spp_count$Target_1b)] = FALSE

# Combine for target 1

spp_count$Target_1 = ifelse((spp_count$Target_1a & spp_count$Target_1b),
                              TRUE,FALSE)

# find species listed based on their range criteria
iucn_dict = data.frame(cbind(spp_count$taxon_name,
                             spp_count$redlistCriteria,
                             grepl("B", spp_count$redlistCriteria, ignore.case=FALSE)))



colnames(iucn_dict) = c("taxon_name","redlistCriteria","Target_2")
iucn_dict = iucn_dict[,c("taxon_name","Target_2")]

spp_count = spp_count %>%
  left_join(iucn_dict, by= c("taxon_name"),
            relationship = "many-to-many")

head(spp_count)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Get target stats
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Add the targets to the IUCN data
test2 = iucn_banked_recalcitrance %>% left_join(indexes[,c("taxon_name","Target_1","Target_1a","Target_1b","Target_2")],
                                               by = c("taxon_name"="taxon_name"))

#  CR spp number in iucn
length(unique(test2$taxon_name)) # 5705

# CR banked
length(unique(test2$taxon_name[test2$category == "banked"])) # 373

# CR meeting target
length(unique(test2$taxon_name[which(test2$Target_1)])) # 30

# CR meeting target
length(which(test2$Target_1)) # 39


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add the targets to the IUCN data
test2 = iucn_banked_recalcitrance %>% left_join(spp_count[,c("taxon_name","Target_1","Target_2")],
                                               by = c("taxon_name"="taxon_name"))

#  CR spp number in iucn
length(unique(test2$taxon_name)) # 5705

# CR banked
length(unique(test2$taxon_name[test2$category == "banked"])) #373

# CR meeting target 1
length(unique(test2$taxon_name[which(test2$Target_1)])) # 41

# CR meeting target
length(which(test2$Target_1)) # 41

# CR meeting target 1a
length(unique(test2$taxon_name[which(test2$Target_1a)])) # 124
# without data
length(which(spp_count$summed_count == 0)) #107

# CR meeting target 1b
length(unique(test2$taxon_name[which(test2$Target_1b)])) # 49
# without data
length(which(spp_count$summed_sampled == 0)) #188

#### TARGET 2

### banked species
iucn_dict = data.frame(cbind(spp_count$taxon_name,
                             spp_count$redlistCriteria,
                             grepl("A", spp_count$redlistCriteria, ignore.case=FALSE),
                             grepl("B", spp_count$redlistCriteria, ignore.case=FALSE),
                             grepl("C", spp_count$redlistCriteria, ignore.case=FALSE),
                             grepl("D", spp_count$redlistCriteria, ignore.case=FALSE)))



colnames(iucn_dict) = c("taxon_name","redlistCriteria","A","B","C","D")
iucn_dict = iucn_dict[,c("taxon_name","A","B","C","D")]

spp_count = spp_count %>%
  left_join(iucn_dict, by= c("taxon_name"),
            relationship = "many-to-many")

head(spp_count)


length(which(spp_count$A == "TRUE")) #70
length(which(spp_count$B == "TRUE")) #247
length(which(spp_count$C == "TRUE")) #78
length(which(spp_count$D == "TRUE")) #100

length(which(indexes$Target_2)) #247

### all iucn
iucn_dict = data.frame(cbind(iucn_banked_recalcitrance$taxon_name,
                             iucn_banked_recalcitrance$redlistCriteria,
                             iucn_banked_recalcitrance$accessions,
                             iucn_banked_recalcitrance$banked,
                             grepl("A", iucn_banked_recalcitrance$redlistCriteria, ignore.case=FALSE),
                             grepl("B", iucn_banked_recalcitrance$redlistCriteria, ignore.case=FALSE),
                             grepl("C", iucn_banked_recalcitrance$redlistCriteria, ignore.case=FALSE),
                             grepl("D", iucn_banked_recalcitrance$redlistCriteria, ignore.case=FALSE)))

colnames(iucn_dict) = c("taxon_name","redlistCriteria","accessions","banked",
                        "A","B","C","D")
# iucn_dict = iucn_dict[,c("taxon_name","A","B","C","D")]

length(which(iucn_dict$A == "TRUE")) # 615
length(which(iucn_dict$B == "TRUE")) # 4581
length(which(iucn_dict$C == "TRUE")) # 763
length(which(iucn_dict$D == "TRUE")) # 1043

length(which(iucn_dict$A == "TRUE"))/nrow(iucn_dict) # 0.1075739
length(which(iucn_dict$B == "TRUE"))/nrow(iucn_dict) # 0.8012944
length(which(iucn_dict$C == "TRUE"))/nrow(iucn_dict) # 0.1334616
length(which(iucn_dict$D == "TRUE"))/nrow(iucn_dict) # 0.1824383

# species counts needed
length(which(iucn_dict$B == "FALSE" & iucn_dict$accessions >=2)) #46
(247+46)/373
(373-(247+46))/373
iucn_dict2 = iucn_dict[iucn_dict$banked==F,]

length(which(iucn_dict2$B == "TRUE")) # 4581
length(which(iucn_dict2$B == "TRUE"))/nrow(iucn_dict2) # 0.8102545
length(which(iucn_dict2$B == "FALSE")) # 1014
length(which(iucn_dict2$B == "FALSE"))/nrow(iucn_dict2) # 0.8102545

# iucn_banked_recalcitrance = iucn_banked_recalcitrance %>%
#   left_join(iucn_dict, by= c("taxon_name"),
#             relationship = "many-to-many")
