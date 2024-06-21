
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
length(unique(iucn_predictions$taxon_name)) # 328553 all names before matching
length(unique(iucn_CR_predictions$taxon_name)) # 4812 CR before matching
length(unique(iucn_CR_predictions_wcvp_matched$scientificName)) # 104 were matched
length(unique(iucn_CR_predictions_wcvp_matched$taxon_name)) # 104 to this many new names
length(unique(which(iucn_CR_predictions_wcvp_matched$taxonomic_backbone == "WCVP"))) # 96 this many from WCVP
length(unique(which(iucn_CR_predictions_wcvp_matched$taxonomic_backbone == "WFO"))) # 8 from WFO

# how many species are the exceptional species
length(unique(exceptional$Species_name)) # 23530 before matching
length(unique(exceptional_wcvp_matched$Species_name)) # 22283 were matched
length(unique(exceptional_wcvp_matched$taxon_name)) # 22298 to this many new names
length(unique(which(exceptional_wcvp_matched$taxonomic_backbone == "WCVP"))) # 22250 this many from WCVP
length(unique(which(exceptional_wcvp_matched$taxonomic_backbone == "WFO"))) # 48 from WFO

##########################################################
## CR species in the Bank
##########################################################

#how many collections?
length(unique(brahms_wcvp_matched$AccessionNumber)) # 197934

# find the MSB species that are CR endangered
brahms_wcvp_matched$CR = brahms_wcvp_matched$wcvp_accepted_id %in% iucn_wcvp_matched$wcvp_accepted_id
summary(brahms_wcvp_matched$CR)
#   Mode   FALSE    TRUE
# logical  195715    2221

# how many species in the bank are CR
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$CR)])) # 371

# how many families in the bank are CR
length(unique(brahms_wcvp_matched$family[which(brahms_wcvp_matched$CR)])) # 89

# how many orders in the bank are CR
length(unique(brahms_wcvp_matched$order[which(brahms_wcvp_matched$CR)])) # 40

# how many orders in the bank are CR
summary(as.factor(brahms_wcvp_matched$higher[which(brahms_wcvp_matched$CR)])) # 40
# Angiosperms       Ferns Gymnosperms  Lycophytes
#        2163          17          39           2

# percentage of CR species that are banked
summary(as.factor(iucn_wcvp_matched$higher)) # 40
#  A     Angiosperms       Bryophyta           Ferns     Gymnosperms      Lycophytes Marchantiophyta
# 10            5442              27              92              72              14              10

# 2163 / (5442+10)
# 17 /92

percentage_fam = data.frame(matrix(NA, nrow = length(unique(iucn_wcvp_matched$family)),
                                   ncol = 4))
colnames(percentage_fam) = c("family","banked","iucn","percentage")

brahms_CR_wcvp_matched = brahms_wcvp_matched[which(brahms_wcvp_matched$CR == T),]

iter = 0
for (fam in unique(iucn_wcvp_matched$family)){
  percentage_fam[iter,1] = fam
  percentage_fam[iter,2] = length(unique(brahms_CR_wcvp_matched$taxon_name[brahms_CR_wcvp_matched$family == fam ]))
  percentage_fam[iter,3] = length(unique(iucn_wcvp_matched$taxon_name[iucn_wcvp_matched$family == fam]))
  percentage_fam[iter,4] = (percentage_fam[iter,2] / percentage_fam[iter,3]) * 100
  iter = iter + 1
}

percentage_fam[order(percentage_fam$percentage),]

#########################################################
## predicted CR species in the Bank
##########################################################

# how many collections?
length(unique(brahms_wcvp_matched$AccessionNumber)) # 197934

# what predictions are not already listed:
pred_ID = iucn_CR_predictions_wcvp_matched$wcvp_accepted_id[!(iucn_CR_predictions_wcvp_matched$wcvp_accepted_id %in% iucn_wcvp_matched$wcvp_accepted_id)]

# find the MSB species that are CR endangered
brahms_wcvp_matched$CR_pred = brahms_wcvp_matched$wcvp_accepted_id %in% pred_ID
# brahms_wcvp_matched$wcvp_accepted_id %in% iucn_CR_predictions_wcvp_matched$wcvp_accepted_id
# brahms_wcvp_matched$CR_pred[brahms_wcvp_matched$CR & brahms_wcvp_matched$CR_pred] = FALSE

summary(brahms_wcvp_matched$CR_pred)
#    Mode   FALSE    TRUE
# logical  197928       8


# how many species in the bank are CR
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$CR_pred)])) # 313 # 1

# how many families in the bank are CR
length(unique(brahms_wcvp_matched$family[which(brahms_wcvp_matched$CR_pred)])) # 64 # 1

# how many orders in the bank are CR
length(unique(brahms_wcvp_matched$order[which(brahms_wcvp_matched$CR_pred)])) # 29 # 1


# summary
summary(as.factor(brahms_wcvp_matched$higher[which(brahms_wcvp_matched$CR_pred)])) # 40
# Angiosperms
# 8

percentage_fam = data.frame(matrix(NA, nrow = length(unique(iucn_CR_predictions_wcvp_matched$family)),
                                   ncol = 4))
colnames(percentage_fam) = c("family","banked","iucn","percentage")

brahms_CR_wcvp_matched = brahms_wcvp_matched[which(brahms_wcvp_matched$CR_pred == T),]



iter = 0
for (fam in unique(iucn_CR_predictions_wcvp_matched$family)){
  percentage_fam[iter,1] = fam
  percentage_fam[iter,2] = length(unique(brahms_CR_wcvp_matched$taxon_name[brahms_CR_wcvp_matched$family == fam ]))
  percentage_fam[iter,3] = length(unique(iucn_CR_predictions_wcvp_matched$taxon_name[iucn_CR_predictions_wcvp_matched$family == fam]))
  percentage_fam[iter,4] = (percentage_fam[iter,2] / percentage_fam[iter,3]) * 100
  iter = iter + 1
}

percentage_fam[order(percentage_fam$percentage),]

##############

percentage_fam = data.frame(matrix(NA, nrow = length(unique(iucn_CR_predictions_wcvp_matched$family[iucn_CR_predictions_wcvp_matched$wcvp_accepted_id %in% pred_ID])),
                                   ncol = 4))
colnames(percentage_fam) = c("family","banked","iucn","percentage")

brahms_CR_wcvp_matched = brahms_wcvp_matched[which(brahms_wcvp_matched$CR_pred == T),]



iter = 0
for (fam in unique(iucn_CR_predictions_wcvp_matched$family)){
  percentage_fam[iter,1] = fam
  percentage_fam[iter,2] = length(unique(brahms_CR_wcvp_matched$taxon_name[brahms_CR_wcvp_matched$family == fam ]))
  percentage_fam[iter,3] = length(unique(iucn_CR_predictions_wcvp_matched$taxon_name[iucn_CR_predictions_wcvp_matched$family == fam]))
  percentage_fam[iter,4] = (percentage_fam[iter,2] / percentage_fam[iter,3]) * 100
  iter = iter + 1
}

percentage_fam[order(percentage_fam$percentage),]

########################################

# TARGET 1: At least 1 collection of 1050 seeds representative of of the genetic diversity of samples pop

########################################

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


any(iucn_wcvp_matched$family ==  "Bryophyta")
# IUCN years
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


#########################################
#number of taxa in the bank
length(unique(brahms_wcvp_matched$taxon_name)) # 45811
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$taxonomic_backbone == "WCVP")]) )# 45779
length(unique(brahms_wcvp_matched$taxon_name[which(brahms_wcvp_matched$taxonomic_backbone == "WFO")]) )# 45779

length(unique(MSB_wcvp_matched$taxon_name[which(MSB_wcvp_matched$taxonomic_backbone == "WFO")])) # 43

