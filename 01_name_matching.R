library(dplyr)
library(stringr)
# library(fuzzyjoin)
library(stringdist)
library(rWCVP)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

###### FUNCTIONS ###############################################################

# Function to perform fuzzy matching
fuzzy_match <- function(str1, str2) {
  distance <- stringdist(str1, str2, method = "jw")  # Using Jaro-Winkler distance
  return(distance < 0.1)  # Adjust threshold as needed
}


###### LOAD DATA ###############################################################

# Load POWO/wcvp dowloaded from POWO directly
wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ),
                             sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

###################################################################################
#         MSBP data
###################################################################################

###### PREP MSBP DATA FOR NAME MATCHING #########################################

### Note: the MSBP data has subspecies, variety and author name in addition to
###       species name in one string. To facilitate rWCVP name matching, these
###       are separated out in this bit of code.


# load brahms data
brahms <- read.csv(paste0(basepath,"2024-03-21_164953044-BRAHMSOnlineData.csv"))

# remove duplicates
brahms <- brahms[duplicated(brahms$AccessionNumber)==FALSE,] # removes 441 duplicates

# extract species
brahms$species <- gsub("^(\\S+ \\S+).*", "\\1", brahms$Taxon) #gsub("^(\\w+ \\w+).*", "\\1", brahms$Taxon)
brahms$species <- trimws(brahms$species)

# extract subspecies
subspecies_match <- regexpr("subsp\\. \\w+", brahms$Taxon)
brahms$subspecies <- substring(brahms$Taxon, subspecies_match,
                               subspecies_match + attr(subspecies_match, "match.length") - 1)

# extract variety
variety_match <- regexpr("var\\. \\w+", brahms$Taxon)
brahms$var <- substring(brahms$Taxon, variety_match,
                        variety_match + attr(variety_match, "match.length") - 1)

# now save the rest of the string as the author name
# Remove species and subspecies information to get author
brahms$author <- gsub("^(\\S+ \\S+)", "", brahms$Taxon) # gsub("^(\\w+ \\w+)", "", brahms$Taxon)  # Remove species
brahms$author <- gsub("subsp\\. \\w+", "", brahms$author)      # Remove subspecies
brahms$author <- gsub("var\\. \\w+", "", brahms$author)
brahms$author <- trimws(brahms$author)
# brahms$subspecies_name = trimws(paste(brahms$species,brahms$subspecies))

# edit code to include var and other if needed
brahms$full_name = trimws(paste(brahms$species,brahms$subspecies,brahms$var))
brahms$full_name = gsub("\\s+"," ",brahms$full_name)


# now count adjusted seeds per species
# spp_count = brahms %>%
#   group_by(subspecies_name,author,CountryName,AdjustedSeedQuantity) %>%
#   tally() %>%
#   mutate(summed_count = sum(AdjustedSeedQuantity),
#          count = n()) %>%
#   ungroup()
# spp_count = spp_count[, c("subspecies_name","author", "CountryName","count", "summed_count")]
# spp_count =spp_count %>% distinct()
# # spp_count = spp_count[duplicated(spp_count$subspecies_name)==FALSE,]
# spp_count$CR = ifelse(spp_count$subspecies_name %in% iucn$scientificName,1,0)
# spp_country_count = spp_count


# create summary table where adjusted seed counts per species are added together
spp_count = brahms %>%
  group_by(full_name,species,author,AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(summed_count = sum(AdjustedSeedQuantity),
         accessions = n()) %>%
  ungroup()
spp_count = spp_count[, c("full_name","species","author", "accessions", "summed_count")]
spp_count = spp_count[duplicated(spp_count$full_name)==FALSE,]
# spp_count$CR = ifelse(spp_count$full_name %in% iucn$scientificName,1,0)


###### NAME MATCHING ###########################################################

### Note: rWCVP sometimes returns multiple matches for one name. This bit of code
###       follows protocol from https://nph.onlinelibrary.wiley.com/doi/full/10.1002/ppp3.10146
###       whereby if there are multiple names, 1st we use accepted name,
###       if no accepted name, we use synonym, and if no synonym, we use
###       homotypic synonym.



# This bit of code dates 30 minutes to run, and is therefore saved to avoid rerun
# MSB species
MSB_wcvp = wcvp_match_names(spp_count, wcvp,
                 name_col = "full_name",
                 author_col = "author")
MSB_wcvp = MSB_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
                           by=c("wcvp_accepted_id" = "plant_name_id"))

write.csv(MSB_wcvp, paste0(basepath,"MSB_unique_wcvp_full_name.csv"))
MSB_wcvp = read.csv(paste0(basepath,"MSB_unique_wcvp_full_name.csv"))

# Put the data in test to avoid overwriting for time being...
test = MSB_wcvp
test$duplicated = test$full_name %in% unique(test$full_name[ duplicated(test$full_name)])
test$accepted_name = test$wcvp_status == "Accepted"
test$names_match = apply(test, 1, function(row) fuzzy_match(row["full_name"], row["taxon_name"]))#test$species == test$taxon_name
test$keep = ifelse(test$duplicated,0,1)
test$keep = ifelse(test$accepted_name & test$names_match & test$duplicated, 1, test$keep)

obvious = test$full_name[test$accepted_name & test$names_match & test$duplicated]


# find duplicated names that don't have any accepted name
dupl = test$full_name %in% unique(test$full_name[ duplicated(test$full_name)])
dupl_nam = unique(test$full_name[dupl])
# create some empty variables to fill during analysis
problematic = c()
accepted = c()
synonym = c()
homotypic = c()
diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$full_name == du,])
  if (!(any(temp$keep))) {

    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      test$keep[which(test$full_name == du)[which(temp$accepted_name == T)]] = 1
      accepted = c(accepted, du)
    }

    # if not accepted, keep the synonym
    else if (length(which(temp$wcvp_status == "Synonym"))==1){
      test$keep[which(test$full_name == du)[which(temp$wcvp_status == "Synonym")]] = 1
      synonym = c(synonym, du)
    }

    # if not a synonym, keep the homotypic synonym
    else if (length(which(temp$wcvp_homotypic == "TRUE"))==1){
      test$keep[which(test$full_name == du)[which(temp$wcvp_homotypic == "TRUE")]] = 1
      homotypic = c(homotypic, du)
    }

    # if the names are the same, keep the one with the smallest author distance
    else if (length(unique(temp$taxon_name)) == 1){
      test$keep[which(test$full_name == du)[which(temp$wcvp_author_edit_distance == min(temp$wcvp_author_edit_distance))]] = 1
      diff_author = c(diff_author, du)
    }

    # class everything else as problematic
    else {
      problematic = c(problematic, du)
    }
  }
}



# some subspecies names are not recognised - use the species name and rerun rWCVP
subset = data.frame(test[test$full_name %in% problematic,])
subset$subsp = ifelse(subset$full_name != subset$species, T, F)
subset = subset[subset$subsp == T,]
species_match = unique(subset$full_name)

for (nami in species_match){
  print(nami)
  temp = test[test$full_name == nami,]
  temp2 = wcvp_match_names(temp[1,1:5], wcvp,
                           name_col = "species")
  temp2 = temp2 %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
                              by=c("wcvp_accepted_id" = "plant_name_id"))
  i=1
  if (nrow(temp2)>1){
    i= which(temp2$wcvp_status == "Accepted")
  }
  test[which(test$full_name == nami)[1],colnames(temp2)] = temp2[i, ]
  test$keep[which(test$full_name == nami)[1]] = 1
}

# the remaining species are species that have been split,
# use the country they were collected  to assign the sub species
problematic = problematic[!(problematic %in% unique(subset$full_name))]
problematic
# there are some species that were not duplicated, but that didn't find a match
#

species_match = c(species_match, test$full_name[is.na(test$taxon_name) & test$duplicated==F & test$species != test$full_name])

# go through the process again pf selecting the accepted name and synonym
for (nami in test$full_name[is.na(test$taxon_name) & test$duplicated==F & test$species != test$full_name]){
  print(nami)
  tryCatch({
    temp = test[test$full_name == nami,]
    temp2 = wcvp_match_names(temp[1,1:5], wcvp,
                             name_col = "species")
    temp2 = temp2 %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
                                by=c("wcvp_accepted_id" = "plant_name_id"))
    i=1
    if (nrow(temp2)>1){
      i = which(temp2$wcvp_status == "Accepted")
      if (length(i) == 0) i = which(temp2$wcvp_status == "Synonym")
    }
    test[which(test$full_name == nami)[1],colnames(temp2)] = temp2[i, ]
    test$keep[which(test$full_name == nami)[1]] = 1
  }, error = function(e) {
    cat("Error occurred for", nami, ":", conditionMessage(e), "\n")
  })
}



problematic = c(problematic, test$full_name[test$duplicated==F & test$keep == 1 & is.na(test$taxon_name)])

test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)


# see how many species were matched to different categories
length(obvious) # 1608
length(accepted) # 17
length(synonym) # 56
length(homotypic) # 9
length(diff_author) # 6
length(species_match) # 70
length(problematic) # 126


# subset = data.frame(test[test$subspecies_name %in% problematic,])
# for (nami in unique(subset$subspecies_name)){
#   temp = data.frame(test[test$subspecies_name == nami,])
#   if (any(temp$accepted_name == "Accepted")){
#   # find the countries the species is collected from in Brahms
#   countries = brahms[brahms$subspecies_name == nami, "CountryName"]
#   spp_codes = subset$wcvp_accepted_id[subset$subspecies_name ==  nami]
#   wcvp_geo = wcvp_countries[wcvp_countries$plant_name_id %in% spp_codes,]
#   }
# }

##### NOW JOIN THE NAMES TO THE BRAHMS DATA EXTRACT ###############################

MSB_wcvp_matched = test[test$keep == 1,]
write.csv(MSB_wcvp_matched, paste0(basepath, "brahms_unique_wcvp_matched_full_name.csv"))

brahms_wcvp_matched = brahms %>% left_join(MSB_wcvp_matched, by = "full_name")
write.csv(brahms_wcvp_matched, paste0(basepath, "brahms_wcvp_matched_full_name.csv"))

###################################################################################
#         IUCN
###################################################################################

### Now do the same for the IUCN data. Note that

# load IUCN
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
length(iucn$scientificName) # 5702

# slow to run 10 mins therefore saved and can be loaded
# iucn_wcvp = wcvp_match_names(iucn, wcvp_names,
#                  name_col = "scientificName",
#                  id_col = "assessmentId",
#                  author_col = "authority")
# iucn_wcvp = iucn_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
#                            by=c("wcvp_accepted_id" = "plant_name_id"))
# write.csv(iucn_wcvp, paste0(basepath,"iucn_wcvp.csv"))
iucn_wcvp = read.csv(paste0(basepath,"iucn_wcvp.csv"))


# Put the data in test to avoid overwriting for time being...
test = iucn_wcvp
test$duplicated = test$scientificName %in% unique(test$scientificName[ duplicated(test$scientificName)])
test$accepted_name = test$wcvp_status == "Accepted"
test$names_match = apply(test, 1, function(row) fuzzy_match(row["scientificName"], row["taxon_name"]))#test$species == test$taxon_name
test$keep = ifelse(test$duplicated,0,1)
test$keep = ifelse(test$accepted_name & test$names_match & test$duplicated, 1, test$keep)

obvious = test$scientificName[test$accepted_name & test$names_match & test$duplicated]

# find duplicated names that don't have any accepted name
dupl = test$scientificName %in% unique(test$scientificName[ duplicated(test$scientificName)])
dupl_nam = unique(test$scientificName[dupl])
#create some empty variables to fill during analysis
problematic = c()
accepted = c()
synonym = c()
homotypic = c()
diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$scientificName == du,])
  if (!(any(temp$keep))) {

    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      test$keep[which(test$scientificName == du)[which(temp$accepted_name == T)]] = 1
      accepted = c(accepted, du)
    }

    # if not accepted, keep the synonym
    else if (length(which(temp$wcvp_status == "Synonym"))==1){
      test$keep[which(test$scientificName == du)[which(temp$wcvp_status == "Synonym")]] = 1
      synonym = c(synonym, du)
    }

    # if not a synonym, keep the homotypic synonym
    else if (length(which(temp$wcvp_homotypic == "TRUE"))==1){
      test$keep[which(test$scientificName == du)[which(temp$wcvp_homotypic == "TRUE")]] = 1
      homotypic = c(homotypic, du)
    }

    # if the names are the same, keep the one with the smallest author distance
    else if (length(unique(temp$taxon_name)) == 1){
      test$keep[which(test$scientificName == du)[which(temp$wcvp_author_edit_distance == min(temp$wcvp_author_edit_distance))]] = 1
      diff_author = c(diff_author, du)
    }

    # class everything else as problematic
    else {
      problematic = c(problematic, du)
    }
  }
}

problematic = c(problematic,test$scientificName[test$duplicated==F & test$keep == 1 & is.na(test$taxon_name)])
test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)


# see how many species were matched to different categories
length(obvious) # 51
length(accepted) # 2
length(synonym) # 5
length(homotypic) # 1
length(diff_author) # 0
length(problematic) # 81

##### NOW GET RID OF DUPLICATED NAMES #################################################

iucn_wcvp_matched = test[test$keep == 1,]
length(unique(iucn_wcvp_matched$scientificName))-length(unique(iucn_wcvp$scientificName))
write.csv(iucn_wcvp_matched, paste0(basepath, "iucn_wcvp_matched.csv"))




###################################################################################
#         EXCEPTIONAL  SPECIES
###################################################################################
exceptional <- read.csv(paste0(basepath, "pence_appendix1.csv"))

# exceptional_wcvp = wcvp_match_names(exceptional, wcvp,
#                                     name_col = "Species_name")
# exceptional_wcvp = exceptional_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
#                            by=c("wcvp_accepted_id" = "plant_name_id"))
# write.csv(exceptional_wcvp, paste0(basepath,"exceptional_wcvp.csv"))
exceptional_wcvp = read.csv(paste0(basepath,"exceptional_wcvp.csv"))


# Put the data in test to avoid overwriting for time being...
test = exceptional_wcvp
test$duplicated = test$Species_name %in% unique(test$Species_name[ duplicated(test$Species_name)])
test$accepted_name = test$wcvp_status == "Accepted"
test$names_match = apply(test, 1, function(row) fuzzy_match(row["Species_name"], row["taxon_name"]))#test$species == test$taxon_name
test$keep = ifelse(test$duplicated,0,1)
test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)
test$keep = ifelse(test$accepted_name & test$names_match & test$duplicated, 1, test$keep)

obvious = test$Species_name[test$accepted_name & test$names_match & test$duplicated]

# find duplicated names that don't have any accepted name
dupl = test$Species_name %in% unique(test$Species_name[ duplicated(test$Species_name)])
dupl_nam = unique(test$Species_name[dupl])
#create some empty variables to fill during analysis
problematic = c()
accepted = c()
synonym = c()
homotypic = c()
# diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$Species_name == du,])
  if (!(any(temp$keep))) {

    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      test$keep[which(test$Species_name == du)[which(temp$accepted_name == T)]] = 1
      accepted = c(accepted, du)
    }

    # if not accepted, keep the synonym
    else if (length(which(temp$wcvp_status == "Synonym"))==1){
      test$keep[which(test$Species_name == du)[which(temp$wcvp_status == "Synonym")]] = 1
      synonym = c(synonym, du)
    }

    # if not a synonym, keep the homotypic synonym
    else if (length(which(temp$wcvp_homotypic == "TRUE"))==1){
      test$keep[which(test$Species_name == du)[which(temp$wcvp_homotypic == "TRUE")]] = 1
      homotypic = c(homotypic, du)
    }

    # if the names are the same, keep the one with the smallest author distance
    # else if (length(unique(temp$taxon_name)) == 1){
    #   test$keep[which(test$Species_name == du)[which(temp$wcvp_author_edit_distance == min(temp$wcvp_author_edit_distance))]] = 1
    #   diff_author = c(diff_author, du)
    # }

    # class everything else as problematic
    else {
      problematic = c(problematic, du)
    }
  }
}

problematic = c(problematic, test$Species_name[test$duplicated==F & is.na(test$taxon_name)])


# see how many species were matched to different categories
length(obvious) # 1976
length(accepted) # 9
length(synonym) # 355
length(homotypic) # 29
# length(diff_author) # 5
length(problematic) # 141 (was 136 when names were included)

##### NOW GET RID OF DUPLICATED NAMES #################################################

exceptional_wcvp_matched = test[test$keep == 1,]
# not_matched = unique(exceptional_wcvp$Species_name)[!(unique(exceptional_wcvp$Species_name) %in% unique(exceptional_wcvp_matched$Species_name))]
# not_matched = not_matched[!(not_matched %in% problematic)]
# test[test$Species_name == not_matched[2],]
# length differs from problematic because the author difference cannot be used
length(unique(exceptional_wcvp_matched$Species_name))-length(unique(exceptional_wcvp$Species_name))
write.csv(exceptional_wcvp_matched, paste0(basepath, "exceptional_wcvp_matched.csv"))

exceptional_wcvp_matched = read.csv(paste0(basepath, "exceptional_wcvp_matched.csv"))
########
# find the duplicated
dupl = exceptional_wcvp_matched$taxon_name %in%
  unique(exceptional_wcvp_matched$taxon_name[duplicated(exceptional_wcvp_matched$taxon_name)])
dupl_nam = unique(exceptional_wcvp_matched$taxon_name[dupl])

exceptional_wcvp_matched$keep2 = 0
exceptional_wcvp_matched$keep2[dupl == F] = 1

#create some empty variables to fill during analysis
for (du in dupl_nam){
  id = which(exceptional_wcvp_matched$taxon_name == du)
  temp = data.frame(exceptional_wcvp_matched[id,])

  # temp$EF3_short.lived == "<NA>"
  rowi = which(apply(temp, 1, function(x) sum(x == "<NA>")) == max(apply(temp, 1, function(x) sum(x == "<NA>")),na.rm=T))
  if (length(rowi) == 1){
    exceptional_wcvp_matched$keep2[id[rowi]] = 1
  } else if (length(which(temp$accepted_name == T))==1 & sum(exceptional_wcvp_matched$keep2[id[rowi]]) ==  0){
    # keep the accepted name
    exceptional_wcvp_matched$keep2[id[which(temp$accepted_name == T)]] = 1
  } else if (length(which(temp$Exceptional_status == "Insufficient data"))==1 & sum(exceptional_wcvp_matched$keep2[id[rowi]]) ==  0){
    # otherwise keep the one that doesn't have sufficient data
    exceptional_wcvp_matched$keep2[id[which(temp$Exceptional_status != "Insufficient data")][1]] = 1
  } else if (length(which(temp$wcvp_status == "Synonym"))==1 & sum(exceptional_wcvp_matched$keep2[id[rowi]]) ==  0){
    # otherwise keep the synonym
    exceptional_wcvp_matched$keep2[id[which(temp$wcvp_status == "Synonym")]] = 1
  } else if (length(which(temp$match_similarity == max(temp$match_similarity))) == 1 & sum(exceptional_wcvp_matched$keep2[id[rowi]]) ==  0){
    # keep the one with the closest name
    exceptional_wcvp_matched$keep2[id[which(temp$match_similarity == max(temp$match_similarity))]] = 1
  }
  else {
    exceptional_wcvp_matched$keep2[id[sample(1:nrow(temp),1)]] = 1

  }

  #   if (sum(exceptional_wcvp_matched$keep2[which(exceptional_wcvp_matched$taxon_name == du)]) != 1)
  #     exceptional_wcvp_matched$keep2[which(exceptional_wcvp_matched$taxon_name == du)] = 0
  #
  # }

  if (sum(exceptional_wcvp_matched$keep2[which(exceptional_wcvp_matched$taxon_name == du)]) != 1){
    print(paste0(du, " : ", sum(exceptional_wcvp_matched$keep2[which(exceptional_wcvp_matched$taxon_name == du)] )))
  }
}

exceptional_wcvp_matched = exceptional_wcvp_matched[exceptional_wcvp_matched$keep2 ==1,]
write.csv(exceptional_wcvp_matched, paste0(basepath,"exceptional_unique_wcvp_matched.csv"))
