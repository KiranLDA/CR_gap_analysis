library(dplyr)
library(stringr)
# library(fuzzyjoin)
library(stringdist)
library(rWCVP)
library(WorldFlora)

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

# wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ),
#                              sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

# WFO.download(WFO.url =
#                paste0("https://files.worldfloraonline.org/files/WFO_Backbone/",
#                       "_WFOCompleteBackbone/WFO_Backbone.zip"),
#              save.dir = getwd(), WFO.remember = TRUE,
#              timeout = 500)

# WFO.remember(WFO.file = NULL, WFO.data = "WFO.data", WFO.pos = 1)
WFO.remember(WFO.file = paste0(basepath,"WFO_Backbone/classification_v_2023_12.csv"),
             WFO.data = "WFO.data", WFO.pos = 1)

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
# MSB_wcvp = wcvp_match_names(spp_count, wcvp,
#                  name_col = "full_name",
#                  author_col = "author")
# MSB_wcvp = MSB_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
#                            by=c("wcvp_accepted_id" = "plant_name_id"))
#
# write.csv(MSB_wcvp, paste0(basepath,"MSB_unique_wcvp_full_name.csv"))
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
split= c()
synonym = c()
homotypic = c()
diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$full_name == du,])
  #if the names don't match
  if (!(any(temp$keep == 1))) {

    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      if (temp$match_similarity[which(temp$accepted_name == T)] >= 0.9){
        test$keep[which(test$full_name == du)[which(temp$accepted_name == T)]] = 1
        accepted = c(accepted, du)
      } else {
        problematic = c(problematic, du)
      }
    }

    # if there are multiple accepted names the species has been split
    else if (length(which(temp$accepted_name == T))>1){
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$full_name == du)[which(temp$accepted_name == T & temp$match_similarity >= 0.9)]] = 1
        split = c(split, du)
      } else {
        problematic = c(problematic, du)
      }
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



# some subspecies and variety names are not recognised - use the species name instead and rerun rWCVP

# # find names that didn't match and associate them with the author
subset = data.frame(test[test$full_name %in% problematic,])
subset$diff_name = ifelse(subset$full_name != subset$species, T, F)
subset = subset[which(subset$diff_name == T),]
species_match = unique(subset$full_name)
#
# # Find them on wcvp
# for (nami in species_match){
#   print(nami)
#   temp = test[test$full_name == nami,]
#   temp2 = wcvp_match_names(temp[1,1:5], wcvp,
#                            name_col = "species")
#   temp2 = temp2 %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
#                               by=c("wcvp_accepted_id" = "plant_name_id"))
#   i=1
#   if (nrow(temp2)>1){
#     i= which(temp2$wcvp_status == "Accepted")
#   }
#   test[which(test$full_name == nami)[1],colnames(temp2)] = temp2[i, ]
#   test$keep[which(test$full_name == nami)[1]] = 1
#   # KD
#   test$wcvp_status[which(test$full_name == nami)[1]] = "Accepted Species Level"
# }
# test$species[which(test$wcvp_status == "Accepted Species Level")]
# test$full_name[which(test$wcvp_status == "Accepted Species Level")]


# the remaining species are species that have been split,
# use the country they were collected  to assign the sub species
# problematic = problematic[!(problematic %in% unique(subset$full_name))]
# problematic

# there are some species that were not duplicated, but that didn't find a match
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

# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$scientificName %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$scientificName %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$scientificName %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$scientificName %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$scientificName %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
# test$taxonomic_backbone[test$scientificName %in% problematic] = "WFO"

#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# now check them out on WFO
match = test[test$full_name %in% problematic,]

pb_sp = WorldFlora::WFO.match(spec.data = match$full_name, WFO.data=WFO.data,
                              counter=1, verbose=TRUE)
# write.csv(pb_sp, paste0(basepath, "brahms_wfo_matched.csv"))
# pb_sp = read.csv(paste0(basepath, "brahms_wfo_matched.csv"))


wfo_match = c()

for(problem in problematic){
  print(problem)
  # test[test$scientificName %in% problem,]
  sp = pb_sp[pb_sp$spec.name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  if(any(sp$taxonRank == "species" & !is.na(sp$taxonRank))){

    to_add = test[test$scientificName %in% problem,][1,]

    if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$duplicated = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$accepted_name = TRUE
        to_add$match_logic = "accepted"
        to_add$family = sp$family[i]
        to_add$genus = sp$genus[i]
        to_add$taxonomic_backbone = "WFO"
        to_add %>% left_join(rWCVP::taxonomic_mapping, by=c("family" = "family"))
        if (is.na( to_add$higher)) {
          to_add$higher = sp$majorGroup[i]
          to_add$order = sp$majorGroup[i]
        }

        # accepted = c(accepted, problematic)

        test = rbind(test, to_add)
      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = NA
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$duplicated = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$accepted_name = TRUE
        to_add$match_logic = "name_split"
        to_add$taxonomic_backbone = "WFO"
        to_add$family = sp$family[i]
        to_add$genus = sp$genus[i]
        to_add %>% left_join(rWCVP::taxonomic_mapping, by=c("family" = "family"))
        if (any(is.na(to_add$higher))) {
          to_add$higher = sp$majorGroup[i]
          to_add$order = sp$majorGroup[i]
        }

        # accepted = c(accepted, problematic)

        test = rbind(test, to_add)
      }

    }
  }
}

problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$scientificName %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)

# see how many species were matched to different categories
length(obvious) # 1567
length(accepted) # 1
length(split) # 0
length(synonym) # 54
length(homotypic) # 8
length(diff_author) # 9
length(wfo_match) # 62
length(problematic) # 115



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
write.csv(MSB_wcvp_matched, paste0(basepath, "brahms_unique_wcvp_matched_full_name_infra.csv"))

brahms_wcvp_matched = brahms %>% left_join(MSB_wcvp_matched, by = "full_name")
write.csv(brahms_wcvp_matched, paste0(basepath, "brahms_wcvp_matched_full_name_infra.csv"))

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
split= c()
synonym = c()
homotypic = c()
diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$scientificName == du,])
  #if the names don't match
  if (!(any(temp$keep == 1))) {
    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      if (temp$match_similarity[which(temp$accepted_name == T)] >= 0.9){
        test$keep[which(test$scientificName == du)[which(temp$accepted_name == T)]] = 1
        accepted = c(accepted, du)
      } else {
        problematic = c(problematic, du)
      }
    }
    # if there are multiple accepted names the species has been split
    else if (length(which(temp$accepted_name == T))>1){
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$scientificName == du)[which(temp$accepted_name == T & temp$match_similarity >= 0.9)]] = 1
        split = c(split, du)
      } else {
        problematic = c(problematic, du)
      }
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



# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$scientificName %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$scientificName %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$scientificName %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$scientificName %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$scientificName %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
# test$taxonomic_backbone[test$scientificName %in% problematic] = "WFO"



#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# now check them out on WFO
match = test[test$scientificName %in% problematic,]

pb_sp = WorldFlora::WFO.match(spec.data = match$scientificName, WFO.data=WFO.data, counter=1, verbose=TRUE)
# write.csv(pb_sp, paste0(basepath, "iucn_wfo_matched.csv"))



wfo_match = c()

for(problem in problematic){
  print(problem)
  # test[test$scientificName %in% problem,]
  sp = pb_sp[pb_sp$spec.name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  if(any(sp$taxonRank == "species" & !is.na(sp$taxonRank))){

    to_add = test[test$scientificName %in% problem,][1,]

    if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18] sp$New.accepted
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$duplicated = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$accepted_name = TRUE
        to_add$match_logic = "accepted"
        to_add$family = sp$family[i]
        to_add$genus = sp$genus[i]
        to_add$taxonomic_backbone = "WFO"
        to_add %>% left_join(rWCVP::taxonomic_mapping, by=c("family" = "family"))
        if (is.na( to_add$higher)) {
          to_add$higher = sp$majorGroup[i]
          to_add$order = sp$majorGroup[i]
        }

        # accepted = c(accepted, problematic)

        test = rbind(test, to_add)
      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = NA
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$duplicated = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$accepted_name = TRUE
        to_add$match_logic = "name_split"
        to_add$taxonomic_backbone = "WFO"
        to_add$family = sp$family[i]
        to_add$genus = sp$genus[i]
        to_add %>% left_join(rWCVP::taxonomic_mapping, by=c("family" = "family"))
        if (any(is.na(to_add$higher))) {
          to_add$higher = sp$majorGroup[i]
          to_add$order = sp$majorGroup[i]
        }

        # accepted = c(accepted, problematic)

        test = rbind(test, to_add)
      }

    }
  }
}

problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$scientificName %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)


# see how many species were matched to different categories
length(obvious) # 51
length(accepted) # 0
length(split) # 0
length(synonym) # 5
length(homotypic) # 0
length(diff_author) # 0
length(wfo_match) # 51
length(problematic) # 35



##### NOW GET RID OF DUPLICATED NAMES #################################################

iucn_wcvp_matched = test[test$keep == 1,]
length(unique(iucn_wcvp_matched$scientificName))-length(unique(iucn_wcvp$scientificName))
write.csv(iucn_wcvp_matched, paste0(basepath, "iucn_wcvp_matched.csv"))




###################################################################################
#         EXCEPTIONAL  SPECIES
###################################################################################
exceptional <- read.csv(paste0(basepath, "pence_appendix1.csv"))
length(exceptional)

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
test$keep = ifelse(test$accepted_name & test$names_match & test$duplicated, 1, test$keep)

obvious = test$Species_name[test$accepted_name & test$names_match & test$duplicated]

# find duplicated names that don't have any accepted name
dupl = test$Species_name %in% unique(test$Species_name[ duplicated(test$Species_name)])
dupl_nam = unique(test$Species_name[dupl])
#create some empty variables to fill during analysis
problematic = c()
accepted = c()
split= c()
synonym = c()
homotypic = c()
diff_author = c()
# diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$Species_name == du,])
  #if the names don't match
  if (!(any(temp$keep == 1))) {

    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      if (temp$match_similarity[which(temp$accepted_name == T)] >= 0.9){
        test$keep[which(test$Species_name == du)[which(temp$accepted_name == T)]] = 1
        accepted = c(accepted, du)
      } else {
        problematic = c(problematic, du)
      }
    }

    # if there are multiple accepted names the species has been split
    else if (length(which(temp$accepted_name == T))>1){
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$Species_name == du)[which(temp$accepted_name == T & temp$match_similarity >= 0.9)]] = 1
        split = c(split, du)
      } else {
        problematic = c(problematic, du)
      }
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
test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)

# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$Species_name %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$Species_name %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$Species_name %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$Species_name %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$Species_name %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
# test$taxonomic_backbone[test$Species_name %in% problematic] = "WFO"



#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# now check them out on WFO
match = test[test$Species_name %in% problematic,]

pb_sp = WorldFlora::WFO.match(spec.data = match$Species_name, WFO.data=WFO.data, counter=1, verbose=TRUE)
write.csv(pb_sp, paste0(basepath, "exceptional_wfo_matched.csv"))


wfo_match = c()

for(problem in problematic){
  print(problem)
  # test[test$scientificName %in% problem,]
  sp = pb_sp[pb_sp$spec.name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  if(any(sp$taxonRank == "species" & !is.na(sp$taxonRank))){

    to_add = test[test$Species_name %in% problem,][1,]

    if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$duplicated = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$accepted_name = TRUE
        to_add$match_logic = "accepted"
        to_add$family = sp$family[i]
        to_add$genus = sp$genus[i]
        to_add$taxonomic_backbone = "WFO"
        to_add %>% left_join(rWCVP::taxonomic_mapping, by=c("family" = "family"))
        if (is.na( to_add$higher)) {
          to_add$higher = sp$majorGroup[i]
          to_add$order = sp$majorGroup[i]
        }

        # accepted = c(accepted, problematic)

        test = rbind(test, to_add)
      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = NA
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$duplicated = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$accepted_name = TRUE
        to_add$match_logic = "name_split"
        to_add$taxonomic_backbone = "WFO"
        to_add$family = sp$family[i]
        to_add$genus = sp$genus[i]
        to_add %>% left_join(rWCVP::taxonomic_mapping, by=c("family" = "family"))
        if (any(is.na(to_add$higher))) {
          to_add$higher = sp$majorGroup[i]
          to_add$order = sp$majorGroup[i]
        }

        # accepted = c(accepted, problematic)

        test = rbind(test, to_add)
      }

    }
  }
}

problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$scientificName %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)

# see how many species were matched to different categories
length(obvious) # 1976
length(accepted) # 0
length(split) # 0
length(synonym) # 354
length(homotypic) # 29
length(diff_author) # 0
length(wfo_match) # 42
length(problematic) # 109


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



#############################################################################
### IUCN PREDICTIONS
##############################################################################

iucn_predictions = read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
colnames(iucn_predictions)[which(colnames(iucn_predictions) == "taxon_name")] = "scientificName"
length(iucn_predictions$scientificName) # 328565

# slow to run 10 mins therefore saved and can be loaded
# iucn_predictions_wcvp = wcvp_match_names(iucn_predictions, wcvp,
#                                          name_col = "scientificName"#,
#                                         # id_col = "plant_name_id"#,
#                                          # author_col = "authority"
# )
# iucn_predictions_wcvp = iucn_predictions_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
#                                                 by=c("wcvp_accepted_id" = "plant_name_id"))
# write.csv(iucn_predictions_wcvp, paste0(basepath,"iucn_predictions_wcvp.csv"))
iucn_predictions_wcvp = read.csv(paste0(basepath,"iucn_predictions_wcvp.csv"))




# Put the data in test to avoid overwriting for time being...
test = iucn_predictions_wcvp
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
split= c()
synonym = c()
homotypic = c()
diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$scientificName == du,])
  #if the names don't match
  if (!(any(temp$keep == 1))) {

    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      if (temp$match_similarity[which(temp$accepted_name == T)] >= 0.9){
        test$keep[which(test$scientificName == du)[which(temp$accepted_name == T)]] = 1
        accepted = c(accepted, du)
      } else {
        problematic = c(problematic, du)
      }
    }

    # if there are multiple accepted names the species has been split
    else if (length(which(temp$accepted_name == T))>1){
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$scientificName == du)[which(temp$accepted_name == T & temp$match_similarity >= 0.9)]] = 1
        split = c(split, du)
      } else {
        problematic = c(problematic, du)
      }
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



# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$scientificName %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$scientificName %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$scientificName %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$scientificName %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$scientificName %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
# test$taxonomic_backbone[test$scientificName %in% problematic] = "WFO"

#add family
# test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
#                           by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# now check them out on WFO
match = test[test$scientificName %in% problematic,]

pb_sp = WorldFlora::WFO.match(spec.data = match$scientificName, WFO.data=WFO.data, counter=1, verbose=TRUE)
write.csv(pb_sp, paste0(basepath, "iucn_predictions_wfo_matched.csv"))
# pb_sp =read.csv(paste0(basepath, "iucn_predictions_wfo_matched.csv"))

wfo_match = c()

for(problem in problematic){
  print(problem)
  # test[test$scientificName %in% problem,]
  sp = pb_sp[pb_sp$spec.name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  if(any(sp$taxonRank == "species" & !is.na(sp$taxonRank))){

    to_add = test[test$scientificName %in% problem,][1,]

    if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$duplicated = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$accepted_name = TRUE
        to_add$match_logic = "accepted"
        to_add$family = sp$family[i]
        to_add$genus = sp$genus[i]
        to_add$taxonomic_backbone = "WFO"
        to_add %>% left_join(rWCVP::taxonomic_mapping, by=c("family" = "family"))
        if (is.na( to_add$higher)) {
          to_add$higher = sp$majorGroup[i]
          to_add$order = sp$majorGroup[i]
        }

        # accepted = c(accepted, problematic)

        test = rbind(test, to_add)
      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = NA
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$duplicated = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$accepted_name = TRUE
        to_add$match_logic = "name_split"
        to_add$taxonomic_backbone = "WFO"
        to_add$family = sp$family[i]
        to_add$genus = sp$genus[i]
        to_add %>% left_join(rWCVP::taxonomic_mapping, by=c("family" = "family"))
        if (any(is.na(to_add$higher))) {
          to_add$higher = sp$majorGroup[i]
          to_add$order = sp$majorGroup[i]
        }

        # accepted = c(accepted, problematic)

        test = rbind(test, to_add)
      }

    }
  }
}

problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$scientificName %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)

# see how many species were matched to different categories
length(obvious) # 10238
length(accepted) # 0
length(split) # 0
length(synonym) # 136
length(homotypic) # 16
length(diff_author) # 4
length(wfo_match) # 0
length(problematic) # 573



##### NOW GET RID OF DUPLICATED NAMES #################################################

iucn_predictions_wcvp_matched = test[test$keep == 1,]
length(unique(iucn_predictions_wcvp_matched$scientificName))-length(unique(iucn_predictions_wcvp$scientificName))
write.csv(iucn_predictions_wcvp_matched, paste0(basepath, "iucn_predictions_wcvp_matched.csv"))

##########################################################################################
##############           IUCN BRAHMS                        ##############################
##########################################################################################

# NAME MATCH THE DATA NAOMI SENT
iucn_brahms = read.csv(paste0(basepath,"IUCN_seedsampling_info.csv"))

# remove duplicates
iucn_brahms <- iucn_brahms[duplicated(iucn_brahms$ACCESSION)==FALSE,] # removes 441 duplicates

# extract species
iucn_brahms$species <- gsub("^(\\S+ \\S+).*", "\\1", iucn_brahms$SPECIES) #gsub("^(\\w+ \\w+).*", "\\1", brahms$Taxon)
iucn_brahms$species <- trimws(iucn_brahms$species)

# extract subspecies
subspecies_match <- regexpr("subsp\\. \\w+", iucn_brahms$SPECIES)
iucn_brahms$subspecies <- substring(iucn_brahms$SPECIES, subspecies_match,
                               subspecies_match + attr(subspecies_match, "match.length") - 1)

# extract variety
variety_match <- regexpr("var\\. \\w+", iucn_brahms$SPECIES)
iucn_brahms$var <- substring(iucn_brahms$SPECIES, variety_match,
                        variety_match + attr(variety_match, "match.length") - 1)

# now save the rest of the string as the author name
# Remove species and subspecies information to get author
iucn_brahms$author <- gsub("^(\\S+ \\S+)", "", iucn_brahms$SPECIES) # gsub("^(\\w+ \\w+)", "", brahms$Taxon)  # Remove species
iucn_brahms$author <- gsub("subsp\\. \\w+", "", iucn_brahms$author)      # Remove subspecies
iucn_brahms$author <- gsub("var\\. \\w+", "", iucn_brahms$author)
iucn_brahms$author <- trimws(iucn_brahms$author)
# brahms$subspecies_name = trimws(paste(brahms$species,brahms$subspecies))

# edit code to include var and other if needed
iucn_brahms$full_name = trimws(paste(iucn_brahms$species,iucn_brahms$subspecies,iucn_brahms$var))
iucn_brahms$full_name = gsub("\\s+"," ",iucn_brahms$full_name)


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
spp_count = iucn_brahms %>%
  group_by(full_name, species, author, ADJSTCOUNT) %>%
  tally() %>%
  mutate(summed_count = sum(ADJSTCOUNT),
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
# iucn_MSB_wcvp = wcvp_match_names(spp_count, wcvp,
#                  name_col = "full_name",
#                  author_col = "author")
# iucn_MSB_wcvp = iucn_MSB_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
#                            by=c("wcvp_accepted_id" = "plant_name_id"))
#
# write.csv(iucn_MSB_wcvp, paste0(basepath,"iucn_MSB_unique_wcvp_full_name.csv"))
iucn_MSB_wcvp = read.csv(paste0(basepath,"iucn_MSB_unique_wcvp_full_name.csv"))

# Put the data in test to avoid overwriting for time being...
test = iucn_MSB_wcvp
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
split= c()
synonym = c()
homotypic = c()
diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$full_name == du,])
  #if the names don't match
  if (!(any(temp$keep == 1))) {

    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      if (temp$match_similarity[which(temp$accepted_name == T)] >= 0.9){
        test$keep[which(test$full_name == du)[which(temp$accepted_name == T)]] = 1
        accepted = c(accepted, du)
      } else {
        problematic = c(problematic, du)
      }
    }

    # if there are multiple accepted names the species has been split
    else if (length(which(temp$accepted_name == T))>1){
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$full_name == du)[which(temp$accepted_name == T & temp$match_similarity >= 0.9)]] = 1
        split = c(split, du)
      } else {
        problematic = c(problematic, du)
      }
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


test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)

# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$scientificName %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$scientificName %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$scientificName %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$scientificName %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$scientificName %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
# test$taxonomic_backbone[test$scientificName %in% problematic] = "WFO"

#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# see how many species were matched to different categories
length(obvious) # 1
length(accepted) # 0
length(split) # 0
length(synonym) # 0
length(homotypic) # 0
length(diff_author) # 0
length(wfo_match) # 0
length(problematic) # 0



##### NOW JOIN THE NAMES TO THE BRAHMS DATA EXTRACT ###############################

iucn_MSB_wcvp_matched = test[test$keep == 1,]
write.csv(iucn_MSB_wcvp_matched, paste0(basepath, "iucn_brahms_unique_wcvp_matched_full_name.csv"))

iucn_brahms_wcvp_matched = iucn_brahms %>% left_join(iucn_MSB_wcvp_matched, by = "full_name")
write.csv(iucn_brahms_wcvp_matched, paste0(basepath, "iucn_brahms_wcvp_matched_full_name.csv"))
