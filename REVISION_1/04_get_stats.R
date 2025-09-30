library(shiny)
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

# wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

wcvp_countries <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

###### Find the CR species in the dataset ##################################################################

# iucn data in the bank with calculated targets
indexes = read.csv(paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"), encoding = "UTF-8")
# indexes = indexes[which(!is.na(indexes$taxon_name)),]
# unique(indexes$PLANTSAMP)

# convert to dates
indexes$BESTEVERD[which(indexes$BESTEVERD == "  /  /    ")] <- NA
indexes$BESTEVERD <- as.Date(indexes$BESTEVERD , format =  "%d/%m/%Y")

indexes$FIRSTTEST[which(indexes$FIRSTTEST == "  /  /    ")] <- NA
indexes$FIRSTTEST <- as.Date(indexes$FIRSTTEST , format =  "%d/%m/%Y")

indexes$LASTTEST[which(indexes$LASTTEST == "  /  /    ")] <- NA
indexes$LASTTEST <- as.Date(indexes$LASTTEST , format =  "%d/%m/%Y")

indexes$XRAYDATE[which(indexes$XRAYDATE == "  /  /    ")] <- NA
indexes$XRAYDATE <- as.Date(indexes$XRAYDATE , format =  "%d/%m/%Y")



# seedbank data
# brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))
brahms_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_wcvp_matched_full_name_infra.csv"), encoding = "UTF-8")

# convert to dates
brahms_wcvp_matched$BESTEVERD[which(brahms_wcvp_matched$BESTEVERD == "  /  /    ")] <- NA
brahms_wcvp_matched$BESTEVERD <- as.Date(brahms_wcvp_matched$BESTEVERD , format =  "%d/%m/%Y")

brahms_wcvp_matched$FIRSTTEST[which(brahms_wcvp_matched$FIRSTTEST == "  /  /    ")] <- NA
brahms_wcvp_matched$FIRSTTEST <- as.Date(brahms_wcvp_matched$FIRSTTEST , format =  "%d/%m/%Y")

brahms_wcvp_matched$LASTTEST[which(brahms_wcvp_matched$LASTTEST == "  /  /    ")] <- NA
brahms_wcvp_matched$LASTTEST <- as.Date(brahms_wcvp_matched$LASTTEST , format =  "%d/%m/%Y")

brahms_wcvp_matched$XRAYDATE[which(brahms_wcvp_matched$XRAYDATE == "  /  /    ")] <- NA
brahms_wcvp_matched$XRAYDATE <- as.Date(brahms_wcvp_matched$XRAYDATE , format =  "%d/%m/%Y")




# iucn species and their categories
iucn_banked_recalcitrance <- read.csv(paste0(basepath, "revision_1/spp_banked_recalcitrant.csv"), encoding = "UTF-8")


# Access and benefits sharing data
abs <- read.csv(paste0(basepath,"ABSCH-Country-List_03_07_24.csv"), encoding = "UTF-8")

#load data from previous session
iucn <- read.table(paste0(basepath, "revision_1/redlist_2025-1_downloaded_17_09_2025/assessments.csv" ),
                   sep = ",", quote = "\"",
                   dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"), encoding = "UTF-8")
iucn_wcvp_matched$higher[which(iucn_wcvp_matched$higher == "A")] = "Angiosperms"

# make sure only consider predicted that aren't already CR
# brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched_full_name.csv"))
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_unique_wcvp_matched_full_name_infra.csv"), encoding = "UTF-8")


# brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched.csv"))
# brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched.csv"))
# exceptional_wcvp_matched = read.csv(paste0(basepath,"exceptional_wcvp_matched.csv"))
exceptional <- read.csv(paste0(basepath, "revision_1/list_exceptional_status_18_09_2025.csv"), encoding = "UTF-8")
exceptional_wcvp_matched = read.csv(paste0(basepath,"revision_1/exceptional_unique_wcvp_matched.csv"), encoding = "UTF-8")

iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"), encoding = "UTF-8")
iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_predictions_wcvp_matched.csv"), encoding = "UTF-8")


# keep only the CR ones
iucn_CR_predictions = iucn_predictions[which(iucn_predictions$category == "CR"),]
iucn_CR_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]

# keep only the ones that aren't already in IUCN
iucn_CR_predictions_wcvp_matched = iucn_CR_predictions_wcvp_matched[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in% iucn_wcvp_matched$taxon_name)),]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$wcvp_ipni_id %in%
                                                      iucn_wcvp_matched$wcvp_ipni_id))]
iucn_CR_predictions_wcvp_matched$taxon_name[which(!(iucn_CR_predictions_wcvp_matched$taxon_name %in%
                                                      iucn_wcvp_matched$taxon_name))]


CR_msbp <- read.csv(paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"), encoding = "UTF-8")
CR_msbp


##############################################################
###                NAME MATCHING STATS
##############################################################

# how many names have been matched from IUCN
length(unique(iucn$scientificName)) # 6520 # 5702 before matching
length(unique(iucn_wcvp_matched$scientificName)) # 6480 # 5667 were matched
length(unique(iucn$scientificName)) - length(unique(iucn_wcvp_matched$scientificName)) #40
test = iucn[which(!(iucn$scientificName %in% iucn_wcvp_matched$scientificName)),]
length(unique(iucn_wcvp_matched$taxon_name)) # 6437 #5654 to this many new names
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP"))) # 6399 #5618 this many from WCVP
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")))/ length(unique(iucn$scientificName)) # 0.9814417
length(unique(which(iucn_wcvp_matched$taxonomic_backbone == "WFO"))) # 66 # 49 from WFO

# what proportion accepted, synonym, homotypic
summary(as.factor(iucn_wcvp_matched$wcvp_status))/nrow(iucn_wcvp_matched)
#         Accepted  Artificial Hybrid      Illegitimate           Invalid           Synonym
#      0.912500000        0.000154321       0.001851852       0.000308642       0.085185185
# old 0.9163879599       0.0001760253      0.0014082028      0.0003520507      0.0816757613

# separate out the synonyms
length(which(iucn_wcvp_matched$wcvp_status == "Synonym" &
               iucn_wcvp_matched$wcvp_homotypic))/nrow(iucn_wcvp_matched)
# 0.05154321 #0.05157543

length(which(iucn_wcvp_matched$wcvp_status == "Synonym" &
               is.na(iucn_wcvp_matched$wcvp_homotypic)))/nrow(iucn_wcvp_matched)
# 0.03364198 #0.03010033

#### Synonyms for wcvp
length(which(iucn_wcvp_matched$wcvp_status == "Synonym" &
               iucn_wcvp_matched$wcvp_homotypic &
               iucn_wcvp_matched$taxonomic_backbone == "WCVP"))/nrow(iucn_wcvp_matched)
# 0.05123457 #0.05087133

length(which(iucn_wcvp_matched$wcvp_status == "Synonym" &
               is.na(iucn_wcvp_matched$wcvp_homotypic) &
                       iucn_wcvp_matched$taxonomic_backbone == "WCVP"))/nrow(iucn_wcvp_matched)
# 0.03256173 #0.02869213



# what names were doubled in the datasets
doubled <- iucn_wcvp_matched$taxon_name[which(duplicated(iucn_wcvp_matched$taxon_name))]
cbind(iucn_wcvp_matched$taxon_name[which(iucn_wcvp_matched$taxon_name %in% doubled)],
      iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$taxon_name %in% doubled)])


# how many of each order
iucn_wcvp_matched$higher[which(iucn_wcvp_matched$higher == "A")] = "Angiosperms"
iucn_wcvp_matched$higher[which(iucn_wcvp_matched$higher == "Polypodiophyta")] = "Ferns"
summary(as.factor(iucn_wcvp_matched$higher)) # 49 from WFO
# Angiosperms       Bryophyta           Ferns     Gymnosperms      Lycophytes Marchantiophyta
#        6241              27              97              89              16              10
# old    5463              27              95              72              14              10


# how many species are in the bank?
length(unique(brahms_wcvp_matched$full_name)) # 56755 #46920 to start with
length(unique(brahms_unique_wcvp_matched$full_name)) # 56107 #46744 had matches
length(unique(brahms_wcvp_matched$taxon_name)) # 52875 # 45768 matched
length(unique(brahms_unique_wcvp_matched$taxon_name)) # 52875 # 45808 names in WCVP


# proportion matched
length(unique(brahms_wcvp_matched$taxon_name))/length(unique(brahms_wcvp_matched$full_name))
# 0.931636 # 0.9785166

# how many CR collections are there?
length(unique(indexes$ACCESSION)) # 2535 #2348



###############################################################
# how many names have been matched from IUCN predictions to wcvp
###############################################################

# how many of the predicted IUCN species were matched
length(unique(iucn_predictions_wcvp_matched$OldScientificName)) # 328395 # 28498 were matched
length(unique(iucn_predictions_wcvp_matched$taxon_name)) # 326392 #326877 to this many new names
length(unique(which(iucn_predictions_wcvp_matched$taxonomic_backbone == "WCVP"))) # 328209 #327989 this many from WCVP
length(unique(which(iucn_predictions_wcvp_matched$taxonomic_backbone == "WFO"))) # 201 #537 from WFO

# length(unique(iucn_predictions$taxon_name[which(iucn_predictions$category == "CR")])) # 4812 # 4812
# length(unique(iucn_predictions$taxon_name)) # 328553 before matching
# length(unique(iucn_predictions_wcvp_matched$OldScientificName[which(iucn_predictions$category == "CR")])) # 4812 # 4812 were matched
# length(unique(iucn_predictions_wcvp_matched$taxon_name[which(iucn_predictions$category == "CR")])) # 4812 # 4810 to this many new names
# # length(unique(which(iucn_predictions_wcvp_matched$taxonomic_backbone[which(iucn_predictions$category == "CR")] == "WCVP"))) # 4809 # 4807 this many from WCVP
# # length(unique(which(iucn_predictions_wcvp_matched$taxonomic_backbone[which(iucn_predictions$category == "CR")] == "WFO"))) # 1 # 5 from WFO

# how many are CR?
length(iucn_CR_predictions_wcvp_matched$taxonomic_backbone) # 199
length(unique(which(iucn_CR_predictions_wcvp_matched$taxonomic_backbone == "WCVP"))) # 199
length(unique(which(iucn_CR_predictions_wcvp_matched$taxonomic_backbone == "WFO"))) # 0




#################################################################
# Exceptional species matched  to wcvp
#################################################################

# how many species are the exceptional species
length(unique(exceptional$Species.name)) # 23792 #23530 before matching
length(unique(exceptional_wcvp_matched$Species.name)) # 22586 #22283 were matched
length(unique(exceptional_wcvp_matched$taxon_name)) # 22588 #22298 to this many new names
length(unique(which(exceptional_wcvp_matched$taxonomic_backbone == "WCVP"))) # 22588 #22251 this many from WCVP
length(unique(which(exceptional_wcvp_matched$taxonomic_backbone == "WFO"))) #0  #47 from WFO
length(unique(which(is.na(exceptional_wcvp_matched$taxonomic_backbone ))))

###################################################################
## CR in the MSB that are name matched to wcvp
###################################################################

##### MSB #########################################################

# how many accessions
length(unique(brahms_wcvp_matched$ACCESSION)) # 223876 #197934

# how many names were matched
length(unique(brahms_wcvp_matched$SPECIES)) # 56930
length(unique(brahms_wcvp_matched$taxon_name)) # 52875

# by WCVP
length(unique(brahms_wcvp_matched$SPECIES[which(brahms_wcvp_matched$taxonomic_backbone == "WCVP")])) # 54272
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$taxonomic_backbone == "WCVP")])) # 52637

# by WFO
length(unique(brahms_wcvp_matched$SPECIES[which(brahms_wcvp_matched$taxonomic_backbone == "WFO")])) # 302
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$taxonomic_backbone == "WFO")])) # 348



##### CR in MSB ###################################################

# Species ~~~~~~~~~~~

# find the MSB species that are CR endangered
length(unique(CR_msbp$taxon_name)) # 480 # 372

# which species are predicted vs from IUCN
pred_sp = unique(CR_msbp$taxon_name)[which(unique(CR_msbp$taxon_name) %in% unique(iucn_CR_predictions_wcvp_matched$taxon_name))]

# how many species are CR or CRpred
length(unique(CR_msbp$taxon_name[which(CR_msbp$taxon_name %in% pred_sp)])) # 7 # 1
length(unique(CR_msbp$taxon_name[which(!(CR_msbp$taxon_name %in% pred_sp))])) # 473 # 371


# accessions ~~~~~~~~~~~

# total accessions
nrow(CR_msbp) # 2539 # 2348
length(unique(CR_msbp$ACCESSION)) # 2535


# how many accessions that are or not CR_pred
length(unique(CR_msbp$ACCESSION[which(CR_msbp$taxon_name %in% pred_sp)])) # 46
length(unique(CR_msbp$ACCESSION[which(!(CR_msbp$taxon_name %in% pred_sp))])) # 2489


# number of IUCN listed species
length(unique(iucn_wcvp_matched$taxon_name)) # 6437  #5654

# of the CR and CRpred species which one are in the bank, and which ones not?
# banked CRpred
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria == "prediction")]))
# 7

# unbanked CRpred
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria == "prediction")]))
# 192

# banked CR
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria != "prediction")]))
# 476

# unbanked CR
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria != "prediction")]))
# 5961


# make sure they all add up
total_CR_CRpred = (length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria == "prediction")])) +
                     length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria == "prediction")])) +
                     length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria != "prediction")])) +
                     length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria != "prediction")])))
total_CR_CRpred # 6636

total_CR = (length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T  & iucn_banked_recalcitrance$redlistCriteria != "prediction")])) +
              length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F & iucn_banked_recalcitrance$redlistCriteria != "prediction")])))
total_CR # 6437 # 5654



# find how many of each family there are in IUCN
iucn_higher_list  = iucn_wcvp_matched[which(duplicated(iucn_wcvp_matched$taxon_name)==F),] %>%
  group_by(higher) %>%
  tally()

iucn_higher_list
#   higher              n
# 1 Angiosperms      6199
# 2 Bryophyta          27
# 3 Ferns              96
# 4 Gymnosperms        89
# 5 Lycophytes         16
# 6 Marchantiophyta    10


sum(iucn_higher_list$n) # 6437

## What years are the IUCN assessments from
length(which(iucn_banked_recalcitrance$yearPublished < 2000)) #293 #334
length(which(iucn_banked_recalcitrance$yearPublished >= 2000 & iucn_banked_recalcitrance$yearPublished < 2010)) # 462 #472
length(which(iucn_banked_recalcitrance$yearPublished >= 2010 & iucn_banked_recalcitrance$yearPublished < 2020)) # 2020 #1973
length(which(iucn_banked_recalcitrance$yearPublished[which(iucn_banked_recalcitrance$redlistCriteria != "prediction")] >= 2020)) # 3701 #2895
length(iucn_banked_recalcitrance$yearPublished[which(iucn_banked_recalcitrance$redlistCriteria == "prediction")]) # 233

# how many are in wcvp
length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) # 6399 #5618
length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) / length(unique(iucn$scientificName)) # 0.9814417 # 0.9852683
length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) # 66 #49
length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) / length(unique(iucn$scientificName)) # 0.0101227 #0.008593476
length(unique(iucn$scientificName)) - length(which(iucn_wcvp_matched$taxonomic_backbone == "WFO")) - length(which(iucn_wcvp_matched$taxonomic_backbone == "WCVP")) # 55

# how many are banked
summary(iucn_banked_recalcitrance$banked)
#    Mode   FALSE    TRUE
# logical    6221     488


#####################################
### % placed names     ##############

##### CR in the seed bank
length(unique(indexes$ACCESSION)) # 2535 #2348

# total names
length(unique(indexes$taxon_name)) # 480 #372
length(unique(indexes$taxon_name[which(indexes$accepted_name == T)])) # 463 #366

length(unique(indexes$SPECIES)) # 492 #379
length(unique(indexes$SPECIES[indexes$accepted_name == T])) # 465 #366

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# unplaced
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(unique(indexes$SPECIES[which(is.na(indexes$taxonomic_backbone))])) #0
length(unique(indexes$SPECIES[which(is.na(indexes$taxonomic_backbone))]))/length(unique(indexes$SPECIES[indexes$accepted_name ==T]))
# 0 #0.002724796

length(unique(iucn_wcvp_matched$scientificName[which(is.na(iucn_wcvp_matched$taxonomic_backbone))]))/length(unique(iucn_wcvp_matched$scientificName[iucn_wcvp_matched$accepted_name ==T]))
# 0.002536783 # 0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# non homotypic
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
above1 = length(unique(indexes$SPECIES[which(indexes$wcvp_status == "Synonym")[!(which(indexes$wcvp_status == "Synonym") %in%
                                                                                   which(indexes$wcvp_homotypic))]]))
below1 = length(unique(indexes$SPECIES[indexes$accepted_name ==T]))
above1/below1
# 0.01935484 # 0.01912568


above2 = length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$wcvp_status == "Synonym")[!(which(iucn_wcvp_matched$wcvp_status == "Synonym") %in%
                                                                                                              which(iucn_wcvp_matched$wcvp_homotypic))]]))
below2 = length(unique(iucn_wcvp_matched$scientificName[iucn_wcvp_matched$accepted_name ==T]))
above2/below2
# 0.03686792 # 0.03331408


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# accepted
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
above1 = length(unique(indexes$SPECIES[which(indexes$wcvp_status == "Accepted")[!(which(indexes$wcvp_status == "Accepted") %in%
                                                                                    which(indexes$wcvp_homotypic))]]))
below1 = length(unique(indexes$SPECIES[indexes$accepted_name ==T]))
above1/below1
# 1 # 0.01907357


above2 = length(unique(iucn_wcvp_matched$scientificName[which(iucn_wcvp_matched$wcvp_status == "Accepted")[!(which(iucn_wcvp_matched$wcvp_status == "Accepted") %in%
                                                                                                               which(iucn_wcvp_matched$wcvp_homotypic))]]))
below2 = length(unique(iucn_wcvp_matched$scientificName[iucn_wcvp_matched$accepted_name ==T]))
above2/below2
# 1 # 0.03331408



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### SEED STORAGE BEHAVIOR PROPORTIONS (BANKED/UNBANKED)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### CERTAIN ############################

# get proportions in the bank
prop = iucn_banked_recalcitrance$category_certain[which(iucn_banked_recalcitrance$banked == T)]
summary(factor(prop))/length(prop)
#   exceptional intermediate     orthodox recalcitrant      unknown         NA's
#   0.014344262  0.012295082  0.526639344  0.008196721  0.004098361  0.434426230

# get proportions not in the bank
prop = iucn_banked_recalcitrance$category_certain[which(iucn_banked_recalcitrance$banked == F)]
summary(factor(prop))/length(prop)
# exceptional intermediate     orthodox recalcitrant      unknown         NA's
# 0.006751326  0.013663398  0.241922520  0.098376467  0.017199807  0.622086481


# get proportions overall
prop = iucn_banked_recalcitrance$category_certain
summary(factor(prop))/length(prop)
#   exceptional intermediate     orthodox recalcitrant      unknown         NA's
#   0.007303622  0.013563869  0.262632285  0.091816962  0.016246833  0.608436429


### UNCERTAIN ##########################

# get proportions in the bank
prop = iucn_banked_recalcitrance$category_uncertain[which(iucn_banked_recalcitrance$banked == T)]
summary(factor(prop))/length(prop)
#   exceptional intermediate     orthodox recalcitrant      unknown         NA's
#   0.014344262  0.014344262  0.881147541  0.032786885  0.004098361  0.053278689

# get proportions not in the bank
prop = iucn_banked_recalcitrance$category_uncertain[which(iucn_banked_recalcitrance$banked == F)]
summary(factor(prop))/length(prop)
#  exceptional intermediate     orthodox recalcitrant      unknown         NA's
#  0.006751326  0.014145636  0.697315544  0.154316026  0.017199807  0.110271661

# get proportions overall
prop = iucn_banked_recalcitrance$category_uncertain
summary(factor(prop))/length(prop)
#  exceptional intermediate     orthodox recalcitrant      unknown         NA's
#  0.007303622  0.014160083  0.710687137  0.145476226  0.016246833  0.106126099



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###  GERMINATION TESTING (BANKED/UNBANKED)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# # how many have been germination tested
# length(which(!is.na(indexes$LASTTEST))) # 404 #321
# length(which(!is.na(indexes$LASTTEST)))/length(unique(indexes$ACCESSION)) # 0.1574435 #0.1367121
#
# # how many have >75% germination
# length(which(indexes$BESTEVER >= 75)) # 254 #212
# # % total >75%
# length(which(indexes$BESTEVER >= 75))/length(unique(indexes$ACCESSION)) # 0.09898675 #0.09028961
# # % tested >75%
# length(which(indexes$BESTEVER >= 75))/length(which(!is.na(indexes$LASTTEST))) # 0.6287129 # 0.6604361
#
# #how many have 100% germination
# length(which(indexes$BESTEVER == 100)) # 142 # 116
#
# #remaining that have not yet been tested
# length(unique(indexes$ACCESSION)) - length(which(indexes$BESTEVER < 75)) # 242 # 212
#
# summary(indexes$BESTEVER[which(indexes$BESTEVER < 75 & !is.na(indexes$LASTTEST))])
# #      Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# #     0.00    0.00    5.50   21.49   44.00   72.00
# # old 0.00    0.00   26.00   27.92   50.00   72.00
#
# # how many were tested but didn't germinate?
# length(which(indexes$BESTEVER == 0 & !is.na(indexes$LASTTEST))) # 69 # 34

### BANKED ##################################################################


## Best test ######################

# proportion of CR accessions tested ~~~~~~~~~~~
nrow(indexes[which(!is.na(indexes$BESTEVERD)),])
# 318
nrow(indexes)
# 2539
nrow(indexes[which(!is.na(indexes$BESTEVERD)),])/nrow(indexes)
# 0.1252462

# 75% viability ~~~~~~~~~~~
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] >= 75)) # 239
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] < 75)) # 79
# of those tested
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] >= 75))/nrow(indexes[which(!is.na(indexes$BESTEVERD)),]) # 0.7515723
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] < 75))/nrow(indexes[which(!is.na(indexes$BESTEVERD)),]) # 0.2484277
# of all CR collections
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] >= 75))/nrow(indexes) # 0.09413155
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] < 75))/nrow(indexes) # 0.03111461


# 100% viability ~~~~~~~~~~~
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] == 100)) # 131
# of those tested
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] == 100))/nrow(indexes[which(!is.na(indexes$BESTEVERD)),]) # 0.4119497
# of all CR collections
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] == 100))/nrow(indexes) # 0.05159512

# 0% viability ~~~~~~~~~~~
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] == 0)) # 0
# of those tested
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] == 0))/nrow(indexes[which(!is.na(indexes$BESTEVERD)),]) # 0
# of all CR collections
length(which(indexes$BESTEVER[which(!is.na(indexes$BESTEVERD))] == 0))/nrow(indexes) # 0




## Last test ######################

# proportion of CR accessions tested ~~~~~~~~~~~
nrow(indexes[which(!is.na(indexes$LASTTEST)),])
# 385
nrow(indexes)
# 2539
nrow(indexes[which(!is.na(indexes$LASTTEST)),])/nrow(indexes)
# 0.1516345

# 75% viability ~~~~~~~~~~~
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] >= 75)) #  218
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] < 75)) # 167
# of those tested
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] >= 75))/nrow(indexes[which(!is.na(indexes$BESTEVERD)),]) #  0.6855346
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] < 75))/nrow(indexes[which(!is.na(indexes$BESTEVERD)),]) # 0.5251572
# of all CR collections
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] >= 75))/nrow(indexes) # 0.08586058
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] < 75))/nrow(indexes) # 0.06577393


# 100% viability ~~~~~~~~~~~
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] == 100)) # 96
# of those tested
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] == 100))/nrow(indexes[which(!is.na(indexes$BESTEVERD)),]) # 0.3018868
# of all CR collections
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] == 100))/nrow(indexes) # 0.03781016

# 0% viability ~~~~~~~~~~~
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] == 0)) # 75
# of those tested
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] == 0))/nrow(indexes[which(!is.na(indexes$LASTTEST)),]) # 0.1948052
# of all CR collections
length(which(indexes$BESTLAST[which(!is.na(indexes$LASTTEST))] == 0))/nrow(indexes) # 0.02953919



##################################################################
### BANKED and UNBANKED ##########################################
##################################################################


## Best test ######################

# proportion of CR accessions tested ~~~~~~~~~~~
nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),])
# 66338
nrow(brahms_wcvp_matched)
# 231478
nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),])/nrow(brahms_wcvp_matched)
# 0.2865845

# 75% viability ~~~~~~~~~~~
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] >= 75)) # 49255
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] < 75)) # 17083
# of those tested
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] >= 75))/
  nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),]) # 0.7424855
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] < 75))/
  nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),]) # 0.2575145
# of all CR collections
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] >= 75))/nrow(brahms_wcvp_matched) # 0.2127848
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] < 75))/nrow(brahms_wcvp_matched) # 0.07379967


# 100% viability ~~~~~~~~~~~
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] == 100)) # 25250
# of those tested
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] == 100))/
  nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),]) #  0.3806265
# of all CR collections
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] == 100))/nrow(brahms_wcvp_matched) # 0.1090816

# 0% viability ~~~~~~~~~~~
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] == 0)) # 0
# of those tested
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] == 0))/nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),]) # 0
# of all CR collections
length(which(brahms_wcvp_matched$BESTEVER[which(!is.na(brahms_wcvp_matched$BESTEVERD))] == 0))/nrow(brahms_wcvp_matched) # 0



## Last test ######################

# proportion of CR accessions tested ~~~~~~~~~~~
nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$LASTTEST)),])
# 73410
nrow(brahms_wcvp_matched)
# 231478
nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$LASTTEST)),])/nrow(brahms_wcvp_matched)
# 0.317136

# 75% viability ~~~~~~~~~~~
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] >= 75)) #  45491
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] < 75)) #  27919
# of those tested
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] >= 75))/nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),]) #  0.6857457
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] < 75))/nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),]) # 0.4208598
# of all CR collections
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] >= 75))/nrow(brahms_wcvp_matched) # 0.1965241
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] < 75))/nrow(brahms_wcvp_matched) # 0.1206119


# 100% viability ~~~~~~~~~~~
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] == 100)) # 19350
# of those tested
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] == 100))/nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$BESTEVERD)),]) # 0.291688
# of all CR collections
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] == 100))/nrow(brahms_wcvp_matched) # 0.08359326

# 0% viability ~~~~~~~~~~~
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] == 0)) # 8178
# of those tested
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] == 0))/nrow(brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$LASTTEST)),]) # 0.1114017
# of all CR collections
length(which(brahms_wcvp_matched$BESTLAST[which(!is.na(brahms_wcvp_matched$LASTTEST))] == 0))/nrow(brahms_wcvp_matched) # 0.03532949


###############################################
# How many countries have CR in MSBP
##########################################

length(unique(indexes$COUNTRY)) # 81
length(unique(brahms_wcvp_matched$COUNTRY)) # 213
length(unique(indexes$COUNTRY))/length(unique(brahms_wcvp_matched$COUNTRY)) # 0.3802817


summary(factor(indexes$DISTPOLICY))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ABS in CR species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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

# Countries with nagoya
length(unique(indexes$COUNTRY_ABS[which(indexes$st.nagoya == 1)])) # 50
length(unique(indexes$COUNTRY_ABS[which(indexes$st.nfp == 1)]))    # 67
length(unique(indexes$COUNTRY_ABS[which(indexes$st.cna == 1)]))    # 30
length(unique(indexes$COUNTRY_ABS[which(indexes$st.ircc == 1)]))   # 13

# number of countries with specific ABS categories
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 0)]))   # 1
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 1)]))   # 17
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 2)]))   # 23
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 3)]))   # 15
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 4)]))   # 13

# Percentage countries with specific ABS categories
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 0)]))/length(unique(indexes$COUNTRY_ABS))   # 0.01428571
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 1)]))/length(unique(indexes$COUNTRY_ABS))   # 0.2428571
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 2)]))/length(unique(indexes$COUNTRY_ABS))   # 0.3285714
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 3)]))/length(unique(indexes$COUNTRY_ABS))   # 0.2142857
length(unique(indexes$COUNTRY_ABS[which(indexes$index.abs == 4)]))/length(unique(indexes$COUNTRY_ABS))   # 0.1857143

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

# number of accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$st.nagoya == 1)]))   # 1400
length(unique(indexes$ACCESSION[which(indexes$st.nfp == 1)]))      # 2456
length(unique(indexes$ACCESSION[which(indexes$st.cna == 1)]))      # 1242
length(unique(indexes$ACCESSION[which(indexes$st.ircc == 1)]))     # 870


# Percentage accessions with specific ABS categories
length(unique(indexes$ACCESSION[which(indexes$st.nagoya == 1)]))/length(unique(indexes$ACCESSION))   # 0.5522682
length(unique(indexes$ACCESSION[which(indexes$st.nfp == 1)]))/length(unique(indexes$ACCESSION))      # 0.9688363
length(unique(indexes$ACCESSION[which(indexes$st.cna == 1)]))/length(unique(indexes$ACCESSION))      # 0.4899408
length(unique(indexes$ACCESSION[which(indexes$st.ircc == 1)]))/length(unique(indexes$ACCESSION))     # 0.3431953



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

# test = left_join(brahms_wcvp_matched[,c("ACCESSION", "COUNTRY")], abs, by=c("COUNTRY"="Country"))
# unique(test$CountryName[is.na(test$status)])

# test2 = left_join(abs, brahms_wcvp_matched[,c("ACCESSION", "COUNTRY")], by=c("Country" = "COUNTRY"))
# unique(test2$Country[is.na(test2$ACCESSION)])

#correct the mismatches
brahms_wcvp_matched$COUNTRY_ABS = brahms_wcvp_matched$COUNTRY
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
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Puerto Rico"] = "United States of America"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Cape Verde"] = "Cabo Verde"
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "Unknown"] = NA
brahms_wcvp_matched$COUNTRY_ABS[brahms_wcvp_matched$COUNTRY_ABS == "?"] = NA


# merge with the data
brahms_wcvp_matched = left_join(brahms_wcvp_matched, abs,
                                by=c("COUNTRY_ABS"="Country"))
# unique(brahms_wcvp_matched[is.na(test$status)])

# Countries with nagoya
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$st.nagoya == 1)])) # 119
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$st.nfp == 1)]))    # 148
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$st.cna == 1)]))    # 68
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$st.ircc == 1)]))   # 21

# number of countries with specific ABS categories
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 0)]))   # 9
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 1)]))   # 33
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 2)]))   # 55
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 3)]))   # 43
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 4)]))   # 21

# Percentage countries with specific ABS categories
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 0)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.04945055
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 1)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.1813187
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 2)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.3021978
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 3)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.2362637
length(unique(brahms_wcvp_matched$COUNTRY_ABS[which(brahms_wcvp_matched$index.abs == 4)]))/length(unique(brahms_wcvp_matched$COUNTRY_ABS))   # 0.1153846

# number of AccessionNumbers with specific ABS categories
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 0)]))   # 2387
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 1)]))   # 71426
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 2)]))   # 31673
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 3)]))   # 32703
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 4)]))   # 63135

# Percentage AccessionNumbers with specific ABS categories
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 0)]))/length(unique(brahms_wcvp_matched$ACCESSION))   # 0.01066215
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 1)]))/length(unique(brahms_wcvp_matched$ACCESSION))   # 0.3190427
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 2)]))/length(unique(brahms_wcvp_matched$ACCESSION))   # 0.1414756
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 3)]))/length(unique(brahms_wcvp_matched$ACCESSION))   # 0.1460764
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$index.abs == 4)]))/length(unique(brahms_wcvp_matched$ACCESSION))   # 0.2820088


# number of AccessionNumbers with specific ABS categories
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$st.nagoya == 1)]))   # 120338
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$st.nfp == 1)]))      # 198867
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$st.cna == 1)]))      # 103079
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$st.ircc == 1)]))     # 63135


# Percentage AccessionNumbers with specific ABS categories
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$st.nagoya == 1)]))/length(unique(brahms_wcvp_matched$ACCESSION))  # 0.5375208
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$st.nfp == 1)]))/length(unique(brahms_wcvp_matched$ACCESSION))     # 0.8882908
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$st.cna == 1)]))/length(unique(brahms_wcvp_matched$ACCESSION))     # 0.460429
length(unique(brahms_wcvp_matched$ACCESSION[which(brahms_wcvp_matched$st.ircc == 1)]))/length(unique(brahms_wcvp_matched$ACCESSION))    # 0.2820088

#############################################
## how many are cultivated with known lineage

table(indexes$cultivation_index)
#    0  0.5    1
# 2348  177   14

round(table(indexes$cultivation_index)/nrow(indexes),3)*100
#    0  0.5    1
# 92.5  7.0  0.6

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  Proposed  targets per CR plant species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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


########################################################
# Get targets
########################################################

# number of species
length(unique(spp_count$taxon_name))
# 480

# how many collections meet target 1 individually
length(which(indexes$Target_1))
# 43

# how many species meet target 1 individually
length(unique(indexes$taxon_name[which(indexes$Target_1)]))
# 32

# # define new comboned target 1
# spp_count$Target_1 = (spp_count$summed_count >= 1050 &
#                         spp_count$summed_sampled >= 50)
#
# # how many species meet target 1 once collections are merged?
# length(which(spp_count$Target_1))
# # 46

#~~~~~~~~~~~~~~~~~~~~
# TARGET 1A
# The ones that just have 1050 collections
spp_count$Target_1a = (spp_count$summed_count >= 1050)

# how many species meet target 1 once collections are merged?
length(which(spp_count$Target_1a))
# 168 # 137

# percentage
length(which(spp_count$Target_1a)) / length(spp_count$Target_1a)
#  0.35 # 0.3682796

# how many species have no data?
length(which(spp_count$summed_count == 0))
# 142 # 107

length(which(spp_count$summed_count == 0))/ length(spp_count$summed_count)
# 0.2958333

#~~~~~~~~~~~~~~~~~~~~
# TARGET 1B
# The ones that just have been collected from over 50 individuals
spp_count$Target_1b = (spp_count$summed_sampled >= 50)

# how many species meet target 1 once collections are merged?
length(which(spp_count$Target_1b))
# 67   # 137

# percentage
length(which(spp_count$Target_1b)) / length(spp_count$Target_1b)
# 0.1395833   # 0.155914

#~~~~~~~~~~~~~~~~~~~~
# TARGET 1 (A&B)

# combined both targets
spp_count$Target_1 = ifelse((spp_count$Target_1a & spp_count$Target_1b),TRUE,FALSE)

# how many species meet target 1 once collections are merged?
length(which(spp_count$Target_1))
# 46

# percentage
length(which(spp_count$Target_1)) / length(spp_count$Target_1)
# 0.09583333

#~~~~~~~~~~~~~~~~~~~~
# how many species have no data?
length(which(spp_count$summed_sampled == 0))
# 243  # 184

# percentage
length(which(spp_count$summed_sampled == 0))/ length(spp_count$summed_sampled)
# 0.50625  # 0.4946237



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Target 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

View(spp_count)


#~~~~~~~~~~~~~~~
# how many collections meet target 2 individually
length(which(indexes$Target_2))
# 35

# how many species meet target 2 individually
length(unique(indexes$taxon_name[which(indexes$Target_2)]))
# 14

#~~~~~~~~~~~~~~~~~~~~~~~
# Target 2A

# define new combined target 2a
spp_count$Target_2a = (spp_count$proportion_native_country == 1)


# how many species meet target 2a once collections are merged?
length(which(spp_count$Target_2a))
# 347

length(which(spp_count$Target_2a))/ length(spp_count$Target_2a)
# 0.7229167

#~~~~~~~~~~~~~~~~~~~~~~~
# Target 2B

# define new combined target 2b
spp_count$Target_2b = (spp_count$collections_all_native_country >= 5)


# how many species meet target 2b once collections are merged?
length(which(spp_count$Target_2b))
# 77

length(which(spp_count$Target_2b))/ length(spp_count$Target_2a)
# 0.1604167


#~~~~~~~~~~~~~~~~~~~~~~~
# Target 2
# define new combined target 2
spp_count$Target_2 = (spp_count$summed_sampled >= 50 &
                        spp_count$proportion_native_country == 1 &
                        spp_count$collections_all_native_country >= 5)


# how many species meet target 2 once collections are merged?
length(which(spp_count$Target_2))
# 12

length(which(spp_count$Target_2))/ length(spp_count$Target_2)
# 0.025



#####################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# storage behaviour
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  CERTAIN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# UNKNOWN prediction
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(is.na(iucn_banked_recalcitrance$category_certain) | iucn_banked_recalcitrance$category_certain == "unknown")]))
# 4153

# ORTHODOX
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain == "orthodox")]))
# 1750


length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain == "intermediate")]))
# 88

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain == "exceptional" | iucn_banked_recalcitrance$category_certain == "recalcitrant")]))
# 655


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UNCERTAIN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# UNKNOWN prediction
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(is.na(iucn_banked_recalcitrance$category_uncertain) | iucn_banked_recalcitrance$category_uncertain == "unknown")]))
# 818

# ORTHODOX
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain == "orthodox")]))
# 4747

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain == "intermediate")]))
# 92

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain == "exceptional" | iucn_banked_recalcitrance$category_uncertain == "recalcitrant")]))
# 1014

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# classification level
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Orthodox
summary(as.factor(iucn_banked_recalcitrance$tax.level[
  which(iucn_banked_recalcitrance$probability.of.recalcitrance <= 0.3)]))/
  length(which(iucn_banked_recalcitrance$probability.of.recalcitrance <= 0.3))
#     Family      Genus      Order    Species
# 0.47477867 0.32612724 0.17726992 0.02182417

# Recalcitrant / exceptional
summary(as.factor(iucn_banked_recalcitrance$tax.level[
  which(iucn_banked_recalcitrance$probability.of.recalcitrance >= 0.7)]))/
  length(which(iucn_banked_recalcitrance$probability.of.recalcitrance >= 0.7))
#      Family      Genus      Order    Species
# 0.375000000 0.600609756 0.009146341 0.015243902


# Intermediate
summary(as.factor(iucn_banked_recalcitrance$tax.level[
  which(iucn_banked_recalcitrance$probability.of.recalcitrance < 0.7 &
          iucn_banked_recalcitrance$probability.of.recalcitrance > 0.3)]))/
  length(which(iucn_banked_recalcitrance$probability.of.recalcitrance < 0.7 &
                 iucn_banked_recalcitrance$probability.of.recalcitrance > 0.3))
#     Family      Genus      Order
# 0.54368932 0.38187702 0.07443366


#
# #~~~~~~~~~~~~~~~~~~~~~
# # in the bank
# #~~~~~~~~~~~~~~~~~~~~~~
#
#
# # orth in bank
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "orthodox"&
#           iucn_banked_recalcitrance$banked == T)]))
# # 339
# 339/372 # 0.9112903
#
# # recalcitrant
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "recalcitrant"&
#           iucn_banked_recalcitrance$banked == T)]))
# # 5
# # reclcitrant names
# unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "recalcitrant"&
#           iucn_banked_recalcitrance$banked == T)])
#
#
#
# # intermediate
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "intermediate"&
#           iucn_banked_recalcitrance$banked == T)]))
# # 18
#
#
# # proportion intermediate
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "intermediate"&
#           iucn_banked_recalcitrance$banked == T)]))/ length(unique(iucn_banked_recalcitrance$taxon_name[
#             which(iucn_banked_recalcitrance$banked == T)]))
# # 0.0483871
#
#
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(is.na(iucn_banked_recalcitrance$category) &
#           iucn_banked_recalcitrance$banked == T)]))
# # 10
#


#~~~~~~~~~~~~~~~~~~
# prediction level overall: family, genus, order, sepcies
#~~~~~~~~~~~~~~~~~~

# family level
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$tax.level == "Species")]))
# 119

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$tax.level == "Order")]))
# 915

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$tax.level == "Genus")]))
# 2399

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$tax.level == "Family")]))
# 2992


# #~~~~~~~~~~~~~
# # # number of intermediate predicted at different precisions
# #~~~~~~~~~~~~~~~~~~
# # family level
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "intermediate" &
#           iucn_banked_recalcitrance$tax.level == "Species")]))
# # 0
#
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "intermediate" &
#           iucn_banked_recalcitrance$tax.level == "Order")]))
# # 95
#
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "intermediate" &
#           iucn_banked_recalcitrance$tax.level == "Genus")]))
# # 177
#
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "intermediate" &
#           iucn_banked_recalcitrance$tax.level == "Family")]))
# # 441
#
#
# #~~~~~~~~~~~~~~~~~~~~
# # exceptional
# #~~~~~~~~~~~~~~~~~~~~
#
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$category == "exceptional")]))
# # 98  82
#
# # EF1
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$EF1_seed_unavailable == "Yes" )]))
# # 37
#
# # EF2
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$EF2_desiccation_sensitive == "Yes" )]))
# # 20
#
# # EF3
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$EF3_short.lived == "Yes" )]))
# # 57
#
# # EF4
# length(unique(iucn_banked_recalcitrance$taxon_name[
#   which(iucn_banked_recalcitrance$EF4_deep_dormancy == "Yes" )]))
# # 2
#
#
#############################################################################
#### NEW ORTHODOXY PREDICTIONS
#############################################################################


####  REFERENCES    ##########################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# certain
#~~~~~~~~~~~~~~~~~~~~~~~~~~

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain_ref == "SID")]))
# 112

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain_ref == "SID")]))/
  length(unique(iucn_banked_recalcitrance$taxon_name))
# 0.0194512

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain_ref == "Pence et al. 2022")]))
# 5

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain_ref == "Pence et al. 2022")]))/
  length(unique(iucn_banked_recalcitrance$taxon_name))
# 0.0008683571

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain_ref == "Wyse and Dickie (2017)")]))
# 1790

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain_ref == "Wyse and Dickie (2017)")]))/
  length(unique(iucn_banked_recalcitrance$taxon_name))
# 0.3108718

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(is.na(iucn_banked_recalcitrance$category_certain_ref))]))
# 3438

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(is.na(iucn_banked_recalcitrance$category_certain_ref))]))/
  length(unique(iucn_banked_recalcitrance$taxon_name))
# 0.5970823

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(!(iucn_banked_recalcitrance$category_certain_ref %in% c("Pence et al. 2022",
                                                                "SID",
                                                                "Wyse and Dickie (2017)")) &
          !is.na(iucn_banked_recalcitrance$category_certain_ref))]))
# 413

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(!(iucn_banked_recalcitrance$category_certain_ref %in% c("Pence et al. 2022",
                                                                "SID",
                                                                "Wyse and Dickie (2017)")) &
          !is.na(iucn_banked_recalcitrance$category_certain_ref))]))/
  length(unique(iucn_banked_recalcitrance$taxon_name))
# 0.07172629

summary(as.factor(iucn_banked_recalcitrance$category_certain))
#  exceptional intermediate     orthodox recalcitrant      unknown         NA's
#           38           96         1520          574          110         3442

summary(as.factor(iucn_banked_recalcitrance$category_certain)) /nrow(iucn_banked_recalcitrance)
#  exceptional intermediate     orthodox recalcitrant      unknown         NA's
#  0.006574394  0.016608997  0.262975779  0.099307958  0.019031142  0.595501730
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# uncertain
#~~~~~~~~~~~~~~~~~~~~~~~~~~

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain_ref == "SID")]))

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain_ref == "Pence et al. 2022")]))

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain_ref == "Wyse and Dickie (2017)")]))
# 4647

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain_ref == "Wyse and Dickie (2017)")]))/
  length(unique(iucn_banked_recalcitrance$taxon_name))
# 0.8070511


length(unique(iucn_banked_recalcitrance$taxon_name[
  which(is.na(iucn_banked_recalcitrance$category_uncertain_ref))]))

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(!(iucn_banked_recalcitrance$category_uncertain_ref %in% c("Pence et al. 2022",
                                                                  "SID",
                                                                  "Wyse and Dickie (2017)")) &
          !is.na(iucn_banked_recalcitrance$category_uncertain_ref))]))


summary(as.factor(iucn_banked_recalcitrance$category_uncertain))

#  exceptional intermediate     orthodox recalcitrant      unknown         NA's
#           38           99         4005          953          110          575

summary(as.factor(iucn_banked_recalcitrance$category_uncertain))/nrow(iucn_banked_recalcitrance)
#  exceptional intermediate     orthodox recalcitrant      unknown         NA's
#  0.006574394  0.017128028  0.692906574  0.164878893  0.019031142  0.099480969




#### Wyse and Dickie and the level of prediction
summary(as.factor(iucn_banked_recalcitrance$tax.level[
  which(iucn_banked_recalcitrance$category_uncertain_ref == "Wyse and Dickie (2017)")]))



####  STORAGE CATEGORIES  ###########################################################


# number CR
length(unique(iucn_banked_recalcitrance$taxon_name))
# 5758

# number CR need banking
length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == F)]))
# 5386

# certain category
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unbankable
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# certain
length(unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_certain == "exceptional" &
           iucn_banked_recalcitrance$banked == F)
        | (iucn_banked_recalcitrance$category_certain == "recalcitrant" &
             iucn_banked_recalcitrance$banked == F)
        | (iucn_banked_recalcitrance$category_certain == "intermediate" &
             iucn_banked_recalcitrance$banked == F))]))
# 689

# uncertain
length(unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_uncertain == "exceptional" &
           iucn_banked_recalcitrance$banked == F)
        | (iucn_banked_recalcitrance$category_uncertain == "recalcitrant" &
             iucn_banked_recalcitrance$banked == F)
        | (iucn_banked_recalcitrance$category_uncertain == "intermediate" &
             iucn_banked_recalcitrance$banked == F))]))
# 1068



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bankable
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_certain == "orthodox" &
           iucn_banked_recalcitrance$banked == F)
        | (iucn_banked_recalcitrance$category_certain == "intermediate" &
             iucn_banked_recalcitrance$banked == F))]))
# 1394


length(unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_uncertain == "orthodox" &
           iucn_banked_recalcitrance$banked == F)
        | (iucn_banked_recalcitrance$category_uncertain == "intermediate" &
             iucn_banked_recalcitrance$banked == F))]))
# 3744


#~~~~~~~~~~~~~~~~~~~~~~~~~~
#certain
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# orthodox
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain == "orthodox" &
          iucn_banked_recalcitrance$banked == T)]))
# 205

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain == "orthodox" &
          iucn_banked_recalcitrance$banked == T)]))/
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))
# 0.5510753


# intermediate
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain == "intermediate" &
          iucn_banked_recalcitrance$banked == T)]))
# 6

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_certain == "intermediate" &
          iucn_banked_recalcitrance$banked == T)]))/
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))
# 0.01612903

# exceptional
length(unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_certain == "exceptional" &
           iucn_banked_recalcitrance$banked == T)
        | (iucn_banked_recalcitrance$category_certain == "recalcitrant" &
             iucn_banked_recalcitrance$banked == T))]))
# 5

unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_certain == "exceptional" &
           iucn_banked_recalcitrance$banked == T)
        | (iucn_banked_recalcitrance$category_certain == "recalcitrant" &
             iucn_banked_recalcitrance$banked == T))])

# "Coffea decaryana"         "Grammitis ascensionensis" "Magnolia grandis"
# [4] "Trichilia acuminata"      "Zanthoxylum mayu"

length(unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_certain == "exceptional" &
           iucn_banked_recalcitrance$banked == T)
        | (iucn_banked_recalcitrance$category_certain == "recalcitrant" &
             iucn_banked_recalcitrance$banked == T))]))/
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))
# 0.01344086

summary(as.factor(iucn_banked_recalcitrance$category_certain[which(iucn_banked_recalcitrance$banked == T)]))

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# uncertain
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# orthodox
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain == "orthodox" &
          iucn_banked_recalcitrance$banked == T)]))
# 339

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain == "orthodox" &
          iucn_banked_recalcitrance$banked == T)]))/
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))
# 0.9112903


# intermediate
length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain == "intermediate" &
          iucn_banked_recalcitrance$banked == T)]))
# 6

length(unique(iucn_banked_recalcitrance$taxon_name[
  which(iucn_banked_recalcitrance$category_uncertain == "intermediate" &
          iucn_banked_recalcitrance$banked == T)]))/
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))
# 0.001042028

# exceptional
length(unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_uncertain == "exceptional" &
           iucn_banked_recalcitrance$banked == T)
        | (iucn_banked_recalcitrance$category_uncertain == "recalcitrant" &
             iucn_banked_recalcitrance$banked == T))]))
# 5

unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_uncertain == "exceptional" &
           iucn_banked_recalcitrance$banked == T)
        | (iucn_banked_recalcitrance$category_uncertain == "recalcitrant" &
             iucn_banked_recalcitrance$banked == T))])

# [1] "Chrysophyllum oliviforme subsp. oliviforme" "Coffea decaryana"
# [3] "Dypsis robusta"                             "Grammitis ascensionensis"
# [5] "Magnolia grandis"                           "Medusagyne oppositifolia"
# [7] "Trichilia acuminata"                        "Zanthoxylum mayu"

length(unique(iucn_banked_recalcitrance$taxon_name[
  which((iucn_banked_recalcitrance$category_uncertain == "exceptional" &
           iucn_banked_recalcitrance$banked == T)
        | (iucn_banked_recalcitrance$category_uncertain == "recalcitrant" &
             iucn_banked_recalcitrance$banked == T))]))/
  length(unique(iucn_banked_recalcitrance$taxon_name[which(iucn_banked_recalcitrance$banked == T)]))
# 0.02150538

summary(as.factor(iucn_banked_recalcitrance$category_uncertain[which(iucn_banked_recalcitrance$banked == T)]))



unique(iucn_banked_recalcitrance$category_uncertain_ref)

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Add the targets to the IUCN data
# test2 = iucn_banked_recalcitrance %>% left_join(spp_count[,c("taxon_name","Target_1","Target_1a", "Target_1b","Target_2")],
#                                                 by = c("taxon_name"="taxon_name"))
#
# #  CR spp number in iucn
# length(unique(test2$taxon_name)) # 5758
#
# # CR banked
# length(unique(test2$taxon_name[test2$banked == TRUE])) #372
#
# # CR meeting target 1
# length(unique(test2$taxon_name[which(test2$Target_1)])) # 43
#
# # CR meeting target
# length(which(test2$Target_1)) # 43
#
# # CR meeting target 1a
# length(unique(test2$taxon_name[which(test2$Target_1a == T)])) # 133
# # without data
# length(which(spp_count$summed_count == 0)) #107
#
# # CR meeting target 1b
# length(unique(test2$taxon_name[which(test2$Target_1b)])) # 58
# # without data
# length(which(spp_count$summed_sampled == 0)) #188
#
#


##########################################################
# add IUCN classification critera

### IUCN information per species
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


length(which(spp_count$A == "TRUE")) #98  #70
length(which(spp_count$B == "TRUE")) #316 #247
length(which(spp_count$C == "TRUE")) #108 #78
length(which(spp_count$D == "TRUE")) #139 #100

length(which(indexes$Target_2)) #247

###  IUCN per accession
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Family data from trees
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Get number of species per family that are CR and that are banked
fam_count = iucn_banked_recalcitrance[,c("family", "banked", "taxon_name",
                                         "category_certain", "category_uncertain")] %>%
  group_by(family) %>%
  summarize(
    CR_species = length(unique(taxon_name[!is.na(taxon_name)])),
    banked_species = length(which(banked)),
    total_accessisions = length(taxon_name[!is.na(taxon_name)]),
    total_orthodox_certain = sum(category_certain == "orthodox"),
    total_orthodox_uncertain = sum(category_uncertain == "orthodox")
  )

# What families have nothing banked?
fam_count$family[which(fam_count$banked_species == 0)]

# What CR families have nothing banked?
fam_count$family[which(fam_count$banked_species == 0 & fam_count$CR_species > 0)]

# What CR families have nothing banked?
fam_count$family[which(fam_count$CR_species > 0)]

# what proportion of families with CR species are banked
length(fam_count$family[which(fam_count$banked_species == 0 & fam_count$CR_species > 0)])/length(fam_count$family[which(fam_count$CR_species > 0)])
#0.6065574

# what families have lots of bankable CR species and no collections
fam_count$family[which(fam_count$banked_species == 0 &
                         fam_count$total_orthodox_certain > 30)]
fam_count$family[which(fam_count$banked_species == 0 &
                         fam_count$total_orthodox_uncertain > 30)]



