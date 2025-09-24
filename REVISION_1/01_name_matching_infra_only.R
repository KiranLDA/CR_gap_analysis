library(dplyr)
library(stringr)
# library(fuzzyjoin)
library(stringdist)
library(rWCVP)
library(WorldFlora)
# install.packages("rWCVPdata", repos=c("https://matildabrown.github.io/drat", "https://cloud.r-project.org"))


basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

###### FUNCTIONS ###############################################################

# Function to perform fuzzy matching
fuzzy_match <- function(str1, str2) {
  distance <- stringdist(str1, str2, method = "jw")  # Using Jaro-Winkler distance
  return(distance < 0.1)  # Adjust threshold as needed
}


###### LOAD DATA ###############################################################

# Load POWO/wcvp dowloaded from POWO directly
# wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),
#                    sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

wcvp <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

wcvp_names <- rWCVPdata::wcvp_names

# WFO.download(WFO.url =
#                paste0("https://files.worldfloraonline.org/files/WFO_Backbone/",
#                       "_WFOCompleteBackbone/WFO_Backbone.zip"),
#              save.dir = getwd(), WFO.remember = TRUE,
#              timeout = 500)

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
# brahms <- read.csv(paste0(basepath,"2024-03-21_164953044-BRAHMSOnlineData.csv"))
# brahms <- read.csv(paste0(basepath,"revision_1/2025-09-23_105316747-BRAHMSOnlineData.csv"))
brahms <- read.table(paste0(basepath, "revision_1/2025-09-23_105316747-BRAHMSOnlineData.csv" ),
                     sep = ",", quote = "\"",
                     dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")


# keep only the ones in active use
brahms <- brahms[which(brahms$TransferToHistory == "False"),]

# remove duplicates
brahms <- brahms[duplicated(brahms$AccessionNumber)==FALSE,] # removes 614 duplicates

# extract species
brahms$species <- gsub("^([A-Z][a-z]+(?:\\s+×)?\\s+\\w+).*", "\\1", brahms$Taxon) #gsub("^(\\S+ \\S+).*", "\\1", brahms$Taxon) #gsub("^(\\w+ \\w+).*", "\\1", brahms$Taxon)
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

# create summary table where adjusted seed counts per species are added together
spp_count = brahms %>%
  group_by(full_name,species,author,AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(summed_count = sum(AdjustedSeedQuantity),
         accessions = n()) %>%
  ungroup()
spp_count = spp_count[, c("full_name","species","author", "accessions", "summed_count")]
spp_count = spp_count[duplicated(spp_count$full_name)==FALSE,]


###### NAME MATCHING ###########################################################

### Note: rWCVP sometimes returns multiple matches for one name. This bit of code
###       follows protocol from https://nph.onlinelibrary.wiley.com/doi/full/10.1002/ppp3.10146
###       whereby if there are multiple names, 1st we use accepted name,
###       if no accepted name, we use synonym, and if no synonym, we use
###       homotypic synonym.


# # This bit of code dates 30 minutes to run, and is therefore saved to avoid rerun
# # MSB species
# MSB_wcvp = wcvp_match_names(spp_count, wcvp,
#                  name_col = "full_name",
#                  author_col = "author")
# MSB_wcvp = MSB_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name", "taxon_authors")],
#                            by=c("wcvp_accepted_id" = "plant_name_id"))
#
# write.csv(MSB_wcvp, paste0(basepath,"revision_1/MSB_unique_wcvp_full_name.csv"), row.names = F)
# MSB_wcvp = read.csv(paste0(basepath,"revision_1/MSB_unique_wcvp_full_name.csv"))
MSB_wcvp = read.table(paste0(basepath, "revision_1/MSB_unique_wcvp_full_name.csv" ),
           sep = ",", quote = "\"",
           dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")


# Put the data in test to avoid overwriting for time being...
test = MSB_wcvp
test$duplicated = test$full_name %in% unique(test$full_name[ duplicated(test$full_name)])
test$accepted_name = test$wcvp_status == "Accepted"
test$names_match = apply(test, 1, function(row) fuzzy_match(row["full_name"], row["taxon_name"])) # test$species == test$taxon_name
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
  # du = "Abutilon pritzelianum"#"Acacia acnidermis"
  # du = "Lepidium sativum"
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
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$full_name == du)[which(temp$wcvp_status == "Synonym")]] = 1
        synonym = c(synonym, du)
      } else {
        problematic = c(problematic, du)
      }
    }

    # if not a synonym, keep the homotypic synonym
    else if (length(which(temp$wcvp_homotypic == "TRUE"))==1){
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$full_name == du)[which(temp$wcvp_homotypic == "TRUE")]] = 1
        homotypic = c(homotypic, du)
      } else {
        problematic = c(problematic, du)
      }
    }

    # if the names are the same, keep the one with the smallest author distance
    else if (length(unique(temp$taxon_name)) == 1){
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$full_name == du)[which(temp$wcvp_author_edit_distance == min(temp$wcvp_author_edit_distance))]] = 1
        diff_author = c(diff_author, du)
      } else {
        problematic = c(problematic, du)
      }
    }

    # class everything else as problematic
    else {
      problematic = c(problematic, du)
    }
  }
}

# upddate the problematic speciees
problematic = c(problematic, test$full_name[test$duplicated==F & test$keep == 1 & is.na(test$taxon_name)])

# add species that were to fuzzily matched (less than 0.9) as problematic and check WFO
problematic = c(problematic, unique(test$full_name[test$match_similarity < 0.9]))
problematic <- unique(problematic)
problematic <- problematic[which(!is.na(problematic))]

test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)

# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$full_name %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$full_name %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$full_name %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$full_name %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$full_name %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
test$taxonomic_backbone[test$full_name %in% problematic] = NA

#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


#### now check them out on WFO #####################
match = test[test$full_name %in% problematic,]

data.frame(match[,c("full_name","author")])

# pb_sp = WorldFlora::WFO.match(spec.data = data.frame(match[,c("full_name","author")]),#match,
#                               WFO.data = WFO.data,
#                               spec.name = "full_name",
#                               Authorship = "author",
#                               counter=1, verbose=TRUE)
# write.csv(pb_sp, paste0(basepath, "revision_1/brahms_wfo_matched.csv"), row.names = F)
# # pb_sp = read.csv(paste0(basepath, "revision_1/brahms_wfo_matched.csv"))
pb_sp = read.table(paste0(basepath, "revision_1/brahms_wfo_matched.csv" ),
           sep = ",", quote = "\"",
           dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")


wfo_match = c()

# add in the ones with no distance
pb_sp$Fuzzy.dist[is.na(pb_sp$Fuzzy.dist)] <- 0


### Loop over the species the WCVP didn't name match and see if WFO did

for(problem in problematic){
  print(problem)
  # problem = "Abies nordmanniana subsp. equi"
  sp = pb_sp[pb_sp$full_name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  #if the matched name is that of a species
  if(any(sp$taxonRank %in% c("species", "subspecies", "variety") & !is.na(sp$taxonRank))){

    # to_add = test[test$scientificName %in% problem,][1,]
    to_add = test[test$full_name %in% problem,][1,]

    if(any(sp$taxonomicStatus == "Accepted" & sp$Fuzzy.dist <3)){ #& !is.na(sp$taxonomicStatus))){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))
      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))
      }


    }
    print("no match")
  } # if it was not matched
}


problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$full_name %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)

# see how many species were matched to different categories
length(obvious) # 1486
length(accepted) # 1
length(split) # 0
length(synonym) # 0
length(homotypic) # 0
length(diff_author) # 0
length(wfo_match) # 352
length(problematic) # 591

##### NOW JOIN THE NAMES TO THE BRAHMS DATA EXTRACT ###############################

MSB_wcvp_matched = test[test$keep == 1,]
write.csv(MSB_wcvp_matched, paste0(basepath, "revision_1/brahms_unique_wcvp_matched_full_name_infra.csv"), row.names = F)

brahms_wcvp_matched = brahms %>% left_join(MSB_wcvp_matched, by = "full_name")
write.csv(brahms_wcvp_matched, paste0(basepath, "revision_1/brahms_wcvp_matched_full_name_infra.csv"), row.names = F)

###################################################################################################################
###################################################################################################################
###################################################################################################################
#         IUCN
###################################################################################################################

### Now do the same for the IUCN data. Note that

# load IUCN
# iucn <- read.csv(paste0(basepath, "revision_1/redlist_2025-1_downloaded_17_09_2025/assessments.csv" ))
iucn <-read.table(paste0(basepath, "revision_1/redlist_2025-1_downloaded_17_09_2025/assessments.csv" ),
                  sep = ",", quote = "\"",
                  dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

length(iucn$scientificName) #6520 #5702
iucn_taxonomy <- read.csv(paste0(basepath, "redlist/taxonomy_fixed.csv" ))
# iucn_taxonomy <- read.table(paste0(basepath, "revision_1/redlist_2025-1_downloaded_17_09_2025/taxonomy.csv" ),
#                             sep = ",", quote = "\"",
#                             dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

iucn <- iucn %>% left_join(iucn_taxonomy[,c("internalTaxonId","authority")],
                           by = "internalTaxonId")


# slow to run 10 mins therefore saved and can be loaded
# iucn_wcvp = wcvp_match_names(iucn, wcvp,
#                  name_col = "scientificName",
#                  id_col = "assessmentId",
#                  author_col = "authority")
# iucn_wcvp = iucn_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name", "taxon_authors")],
#                            by=c("wcvp_accepted_id" = "plant_name_id"))
# write.csv(iucn_wcvp, paste0(basepath,"revision_1/iucn_wcvp.csv"), row.names = F)
#iucn_wcvp = read.csv(paste0(basepath,"revision_1/iucn_wcvp.csv"))
iucn_wcvp <- read.table(paste0(basepath, "revision_1/iucn_wcvp.csv" ),
                        sep = ",", quote = "\"",
                        dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

# Put the data in test to avoid overwriting for time being...
test <- iucn_wcvp
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

problematic = c(problematic, unique(test$scientificName[test$match_similarity < 0.9]))
problematic <- unique(problematic)
problematic <- problematic[which(!is.na(problematic))]

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
test$taxonomic_backbone[test$scientificName %in% problematic] = NA

#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))

rWCVP::taxonomic_mapping[which(rWCVP::taxonomic_mapping$order == "Cycadales"),]
rWCVP::taxonomic_mapping[which(rWCVP::taxonomic_mapping$family == "Cycadaceae"),]

# now check them out on WFO
match = test[which(test$scientificName %in% problematic),]
# match$family = NA
# match$genus = NA

match$full_name = match$scientificName

# pb_sp <- WorldFlora::WFO.match(spec.data = data.frame(match[,c("full_name","authority")]),
#                                WFO.data = WFO.data,
#                                spec.name = "full_name",
#                                Authorship = "authority",
#                                counter=1, verbose=TRUE)
# write.csv(pb_sp, paste0(basepath, "revision_1/iucn_wfo_matched.csv"))#, row_names=F)

# #pb_sp = read.csv(paste0(basepath, "revision_1/iucn_wfo_matched.csv"))
pb_sp = read.table(paste0(basepath, "revision_1/iucn_wfo_matched.csv" ),
           sep = ",", quote = "\"",
           dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")


wfo_match = c()

pb_sp$Fuzzy.dist[is.na(pb_sp$Fuzzy.dist)] <- 0

for(problem in problematic){
  print(problem)
  # test[test$scientificName %in% problem,]
  sp = pb_sp[pb_sp$full_name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  #if(any(sp$taxonRank == "species" & !is.na(sp$taxonRank))){
  if(any(sp$taxonRank %in% c("species", "subspecies", "variety") & !is.na(sp$taxonRank))){

    to_add = test[test$scientificName %in% problem,][1,]

    #if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
    if(any(sp$taxonomicStatus == "Accepted" & sp$Fuzzy.dist <3)){

      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18] sp$New.accepted
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))

      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))
      }
    }
    print("no match")
  }
}

problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$scientificName %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)


# see how many species were matched to different categories
length(obvious) # 9
length(accepted) # 0
length(split) # 0
length(synonym) # 0
length(homotypic) # 0
length(diff_author) # 0
length(wfo_match) # 66
length(problematic) # 55



##### NOW GET RID OF DUPLICATED NAMES #################################################

iucn_wcvp_matched = test[test$keep == 1,]
length(unique(iucn_wcvp_matched$scientificName))-length(unique(iucn_wcvp$scientificName))
write.csv(iucn_wcvp_matched, paste0(basepath, "revision_1/iucn_wcvp_matched.csv"), row.names = F)


###################################################################################################################
###################################################################################################################
###################################################################################################################
#         EXCEPTIONAL  SPECIES
###################################################################################################################
###################################################################################################################
###################################################################################################################


# exceptional <- read.csv(paste0(basepath, "revision_1/list_exceptional_status_18_09_2025.csv"))#pence_appendix1.csv"))
exceptional <- read.table(paste0(basepath, "revision_1/list_exceptional_status_18_09_2025.csv" ),
           sep = ",", quote = "\"",
           dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")
length(exceptional)

# 20 minutes
# exceptional_wcvp = wcvp_match_names(exceptional, wcvp,
#                                     name_col = "Species.name")
# exceptional_wcvp = exceptional_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name", "taxon_authors")],
#                            by=c("wcvp_accepted_id" = "plant_name_id"))
# write.csv(exceptional_wcvp, paste0(basepath,"revision_1/exceptional_wcvp.csv"), row.names = F)
#exceptional_wcvp = read.csv(paste0(basepath,"revision_1/exceptional_wcvp.csv"))
exceptional_wcvp <- read.table(paste0(basepath, "revision_1/exceptional_wcvp.csv" ),
                               sep = ",", quote = "\"",
                               dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")


# Put the data in test to avoid overwriting for time being...
test = exceptional_wcvp
test$duplicated = test$Species.name %in% unique(test$Species.name[ duplicated(test$Species.name)])
test$accepted_name = test$wcvp_status == "Accepted"
test$names_match = apply(test, 1, function(row) fuzzy_match(row["Species.name"], row["taxon_name"]))#test$species == test$taxon_name
test$keep = ifelse(test$duplicated,0,1)
test$keep = ifelse(test$accepted_name & test$names_match & test$duplicated, 1, test$keep)

obvious = test$Species.name[test$accepted_name & test$names_match & test$duplicated]

# find duplicated names that don't have any accepted name
dupl = test$Species.name %in% unique(test$Species.name[ duplicated(test$Species.name)])
dupl_nam = unique(test$Species.name[dupl])
#create some empty variables to fill during analysis
problematic = c()
accepted = c()
split= c()
synonym = c()
homotypic = c()
diff_author = c()
# diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[test$Species.name == du,])
  #if the names don't match
  if (!(any(temp$keep == 1))) {

    # keep the accepted name
    if (length(which(temp$accepted_name == T))==1){
      if (temp$match_similarity[which(temp$accepted_name == T)] >= 0.9){
        test$keep[which(test$Species.name == du)[which(temp$accepted_name == T)]] = 1
        accepted = c(accepted, du)
      } else {
        problematic = c(problematic, du)
      }
    }

    # if there are multiple accepted names the species has been split
    else if (length(which(temp$accepted_name == T))>1){
      if (any(temp$match_similarity[ which(temp$accepted_name == T)] >= 0.9)){
        test$keep[which(test$Species.name == du)[which(temp$accepted_name == T & temp$match_similarity >= 0.9)]] = 1
        split = c(split, du)
      } else {
        problematic = c(problematic, du)
      }
    }


    # if not accepted, keep the synonym
    else if (length(which(temp$wcvp_status == "Synonym"))==1){
      test$keep[which(test$Species.name == du)[which(temp$wcvp_status == "Synonym")]] = 1
      synonym = c(synonym, du)
    }

    # if not a synonym, keep the homotypic synonym
    else if (length(which(temp$wcvp_homotypic == "TRUE"))==1){
      test$keep[which(test$Species.name == du)[which(temp$wcvp_homotypic == "TRUE")]] = 1
      homotypic = c(homotypic, du)
    }

    # if the names are the same, keep the one with the smallest author distance
    # else if (length(unique(temp$taxon_name)) == 1){
    #   test$keep[which(test$Species.name == du)[which(temp$wcvp_author_edit_distance == min(temp$wcvp_author_edit_distance))]] = 1
    #   diff_author = c(diff_author, du)
    # }

    # class everything else as problematic
    else {
      problematic = c(problematic, du)
    }
  }
}

problematic = c(problematic, test$Species.name[test$duplicated==F & is.na(test$taxon_name)])
problematic = c(problematic, unique(test$full_name[test$match_similarity < 0.9]))
problematic <- unique(problematic)
problematic <- problematic[which(!is.na(problematic))]

test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)

# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$Species.name %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$Species.name %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$Species.name %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$Species.name %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$Species.name %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
test$taxonomic_backbone[test$Species.name %in% problematic] = NA

# test$taxonomic_backbone[test$Species.name %in% problematic] = "WFO"



#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# now check them out on WFO
match <- test[which(test$Species.name %in% problematic),]
match$full_name <- match$Species.name


pb_sp <- WorldFlora::WFO.match(spec.data = match[,c("full_name")],
                              WFO.data = WFO.data,
                              spec.name = "full_name",
                              counter=1, verbose=TRUE)
write.csv(pb_sp, paste0(basepath, "revision_1/exceptional_wfo_matched.csv"), row.names = F)
# #pb_sp = read.csv(paste0(basepath, "revision_1/exceptional_wfo_matched.csv"))
pb_sp <- read.table(paste0(basepath, "revision_1/exceptional_wfo_matched.csv" ),
           sep = ",", quote = "\"",
           dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")



wfo_match = c()

pb_sp$Fuzzy.dist[is.na(pb_sp$Fuzzy.dist)] <- 0

for(problem in problematic){
  print(problem)
  # test[test$scientificName %in% problem,]
  sp = pb_sp[pb_sp$spec.name %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  # if(any(sp$taxonRank == "species" & !is.na(sp$taxonRank))){
  if(any(sp$taxonRank %in% c("species", "subspecies", "variety") & !is.na(sp$taxonRank))){

    to_add = test[test$spec.name %in% problem,][1,]

    # if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
    if(any(sp$taxonomicStatus == "Accepted" & sp$Fuzzy.dist <3)){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))

      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))
      }
    }
    print("no match")
  }
}

problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$scientificName %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)

# see how many species were matched to different categories
length(obvious) # 1973
length(accepted) # 0
length(split) # 0
length(synonym) # 358
length(homotypic) # 33
length(diff_author) # 0
length(wfo_match) # 0
length(problematic) # 94


##### NOW GET RID OF DUPLICATED NAMES #################################################

exceptional_wcvp_matched = test[test$keep == 1,]
# not_matched = unique(exceptional_wcvp$Species.name)[!(unique(exceptional_wcvp$Species.name) %in% unique(exceptional_wcvp_matched$Species.name))]
# not_matched = not_matched[!(not_matched %in% problematic)]
# test[test$Species.name == not_matched[2],]
# length differs from problematic because the author difference cannot be used
length(unique(exceptional_wcvp_matched$Species.name))-length(unique(exceptional_wcvp$Species.name))
write.csv(exceptional_wcvp_matched, paste0(basepath, "revision_1/exceptional_wcvp_matched.csv"), row.names = F)
# #exceptional_wcvp_matched = read.csv(paste0(basepath, "revision_1/exceptional_wcvp_matched.csv"))
exceptional_wcvp_matched = read.table(paste0(basepath, "revision_1/exceptional_wcvp_matched.csv" ),
                                      sep = ",", quote = "\"",
                                      dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

########
# find the duplicated
dupl = exceptional_wcvp_matched$taxon_name %in%
  unique(exceptional_wcvp_matched$taxon_name[duplicated(exceptional_wcvp_matched$taxon_name)])
dupl_nam = unique(exceptional_wcvp_matched$taxon_name[dupl])

exceptional_wcvp_matched$keep2 = 0
exceptional_wcvp_matched$keep2[dupl == F] = 1

# create some empty variables to fill during analysis
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
write.csv(exceptional_wcvp_matched, paste0(basepath,"revision_1/exceptional_unique_wcvp_matched.csv"), row.names = F)




###################################################################################################################
###################################################################################################################
###################################################################################################################
### IUCN PREDICTIONS
###################################################################################################################
###################################################################################################################



# iucn_predictions <- read.csv(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv"))
iucn_predictions <- read.table(paste0(basepath, "Angiosperm_extinction_risk_predictions_v1.csv" ),
           sep = ",", quote = "\"",
           dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

colnames(iucn_predictions)[which(colnames(iucn_predictions) == "taxon_name")] <- "OldScientificName"
length(iucn_predictions$OldScientificName) # 328565
# add the name and author based on WCVP
iucn_predictions <-  iucn_predictions %>% left_join(wcvp[,c("ipni_id","taxon_name", "taxon_authors")],
                                                 by="ipni_id")
colnames(iucn_predictions)[which(colnames(iucn_predictions) == "taxon_name")] <- "full_name"
colnames(iucn_predictions)[which(colnames(iucn_predictions) == "taxon_authors")] <- "author_name"
colnames(iucn_predictions)[which(colnames(iucn_predictions) == "plant_name_id")] <- "old_plant_name_id"
iucn_predictions$X <- as.character(1:nrow(iucn_predictions))

match_data <- iucn_predictions[,c("X","threatened",".lower",".upper",".width",
                                  ".point",".interval",
                                  "predicted_threat","confidence","category",
                                  "observed_threat","combined",
                                  "full_name","author_name")]

match_data <- match_data[which(!duplicated(match_data$full_name)),]


# slow to run 10 mins therefore saved and can be loaded
# iucn_predictions_wcvp <- wcvp_match_names(match_data,
#                                           wcvp_names = wcvp_names,
#                                           name_col = "full_name",
#                                           id_col = "X",
#                                           author_col = "author_name")
#
# iucn_predictions_wcvp = iucn_predictions_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name", "taxon_authors")],
#                                                 by=c("wcvp_accepted_id" = "plant_name_id"))
#
# # add in the old data that was lost
# iucn_predictions_wcvp <- iucn_predictions_wcvp %>% left_join(iucn_predictions[,c("X","OldScientificName",
#                                          #"full_name","author_name",
#                                          "old_plant_name_id")], by = "X")
#
#
# write.csv(iucn_predictions_wcvp, paste0(basepath,"revision_1/iucn_predictions_wcvp.csv"), row.names = F)
# #iucn_predictions_wcvp <- read.csv(paste0(basepath,"revision_1/iucn_predictions_wcvp.csv"))
iucn_predictions_wcvp <- read.table(paste0(basepath, "revision_1/iucn_predictions_wcvp.csv" ),
                                    sep = ",", quote = "\"",
                                    dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")



# Put the data in test to avoid overwriting for time being...
test = iucn_predictions_wcvp
test$duplicated = test$full_name %in% unique(test$full_name[ duplicated(test$full_name)])
test$accepted_name = test$wcvp_status == "Accepted"
test$names_match = apply(test, 1, function(row) fuzzy_match(row["full_name"], row["taxon_name"]))#test$species == test$taxon_name
test$keep = ifelse(test$duplicated,0,1)
test$keep = ifelse(test$accepted_name & test$names_match & test$duplicated, 1, test$keep)


obvious = test$full_name[test$accepted_name & test$names_match & test$duplicated]

# find duplicated names that don't have any accepted name
dupl = test$full_name %in% unique(test$full_name[ duplicated(test$full_name)])
dupl_nam = unique(test$full_name[dupl])
#create some empty variables to fill during analysis
problematic = c()
accepted = c()
split= c()
synonym = c()
homotypic = c()
diff_author = c()
for (du in dupl_nam){
  temp = data.frame(test[which(test$full_name == du),])
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

problematic = c(problematic,test$full_name[test$duplicated==F & test$keep == 1 & is.na(test$taxon_name)])
problematic = c(problematic, unique(test$full_name[test$match_similarity < 0.9]))
problematic <- unique(problematic)
problematic <- problematic[which(!is.na(problematic))]

test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)



# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$full_name %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$full_name %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$full_name %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$full_name %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$full_name %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
# test$taxonomic_backbone[test$full_name %in% problematic] = "WFO"
test$taxonomic_backbone[test$full_name %in% problematic] = NA

#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# now check them out on WFO
match = test[test$full_name %in% problematic,]
colnames(match)


# pb_sp = WorldFlora::WFO.match(spec.data = data.frame(match[, c("full_name","author_name")]),
#                               WFO.data = WFO.data,
#                               spec.name = "full_name",
#                               Authorship = "author_name",
#                               counter=1, verbose=TRUE)
#
# write.csv(pb_sp, paste0(basepath, "revision_1/iucn_predictions_wfo_matched.csv"), row.names = F)
# #pb_sp =read.csv(paste0(basepath, "revision_1/iucn_predictions_wfo_matched.csv"))
pb_sp <- read.table(paste0(basepath, "revision_1/iucn_predictions_wfo_matched.csv" ),
                                    sep = ",", quote = "\"",
                                    dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")
wfo_match = c()

pb_sp$Fuzzy.dist[is.na(pb_sp$Fuzzy.dist)] <- 0

for(problem in problematic){
  print(problem)
  # test[test$full_name %in% problem,]
  sp = pb_sp[pb_sp$full_name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  #if(any(sp$taxonRank == "species" & !is.na(sp$taxonRank))){
  if(any(sp$taxonRank %in% c("species", "subspecies", "variety") & !is.na(sp$taxonRank))){

    to_add = test[test$full_name %in% problem,][1,]

    # if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
    if(any(sp$taxonomicStatus == "Accepted" & sp$Fuzzy.dist <3)){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))

      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))

      }
    }
    print("no match")
  }
}

problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$full_name %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)

# see how many species were matched to different categories
length(obvious) # 306
length(accepted) # 0
length(split) # 0
length(synonym) # 10
length(homotypic) # 1
length(diff_author) # 1
length(wfo_match) # 174
length(problematic) # 38



##### NOW GET RID OF DUPLICATED NAMES #################################################

iucn_predictions_wcvp_matched = test[test$keep == 1,]
length(unique(iucn_predictions_wcvp_matched$full_name))-length(unique(iucn_predictions_wcvp$full_name))
write.csv(iucn_predictions_wcvp, paste0(basepath, "revision_1/iucn_predictions_wcvp_matched.csv"), row.names = F)




###################################################################################################################
###################################################################################################################
###################################################################################################################
##############           IUCN BRAHMS
###################################################################################################################
###################################################################################################################




# NAME MATCH THE DATA NAOMI SENT
# iucn_brahms = read.csv(paste0(basepath,"IUCN_seedsampling_info.csv"))
iucn_brahms <- read.table(paste0(basepath, "IUCN_seedsampling_info.csv" ),
                          sep = ",", quote = "\"",
                          dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

# remove duplicates
iucn_brahms <- iucn_brahms[duplicated(iucn_brahms$ACCESSION)==FALSE,] # removes 441 duplicates

# extract species
iucn_brahms$species <- gsub("^([A-Z][a-z]+(?:\\s+×)?\\s+\\w+).*", "\\1", iucn_brahms$SPECIES) #gsub("^(\\S+ \\S+).*", "\\1", iucn_brahms$SPECIES) #gsub("^(\\w+ \\w+).*", "\\1", brahms$Taxon)
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
iucn_MSB_wcvp = wcvp_match_names(spp_count, wcvp,
                 name_col = "full_name",
                 author_col = "author")
iucn_MSB_wcvp = iucn_MSB_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name", "taxon_authors")],
                           by=c("wcvp_accepted_id" = "plant_name_id"))

write.csv(iucn_MSB_wcvp, paste0(basepath,"revision_1/iucn_MSB_unique_wcvp_full_name.csv"), row.names = F)
# #iucn_MSB_wcvp <- read.csv(paste0(basepath,"revision_1/iucn_MSB_unique_wcvp_full_name.csv"))
iucn_MSB_wcvp <- read.table(paste0(basepath, "revision_1/iucn_MSB_unique_wcvp_full_name.csv" ),
                            sep = ",", quote = "\"",
                            dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")


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

problematic = c(problematic, test$full_name[test$duplicated==F & test$keep == 1 & is.na(test$taxon_name)])

problematic = c(problematic, unique(test$full_name[test$match_similarity < 0.9]))
problematic <- unique(problematic)
problematic <- problematic[which(!is.na(problematic))]

test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)


# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$full_name %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$full_name %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$full_name %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$full_name %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$full_name %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
test$taxonomic_backbone[test$full_name %in% problematic] = NA
# test$taxonomic_backbone[test$scientificName %in% problematic] = "WFO"

#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# now check them out on WFO
match = test[test$full_name %in% problematic,]

pb_sp = WorldFlora::WFO.match(spec.data = data.frame(match[,c("full_name","author")]), #match,
                              WFO.data = WFO.data,
                              spec.name = "full_name",
                              Authorship = "author",
                              counter=1, verbose=TRUE)
write.csv(pb_sp, paste0(basepath, "revision_1/iucn_MSB_wfo_matched.csv"), row.names = F)
# #pb_sp <- read.csv(paste0(basepath, "revision_1/iucn_MSB_wfo_matched.csv"))
pb_sp <- read.table(paste0(basepath, "revision_1/iucn_MSB_wfo_matched.csv" ),
          sep = ",", quote = "\"",
          dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

wfo_match = c()

pb_sp$Fuzzy.dist[is.na(pb_sp$Fuzzy.dist)] <- 0

for(problem in problematic){
  print(problem)
  # problem = "Aconitum nasutum"
  # problem = "Acacia elongata var. elongata"
  # problem = "Xanthoselinum alsaticum subsp. alsaticum"
  # test[test$scientificName %in% problem,]
  sp = pb_sp[pb_sp$full_name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  #--------------------
  # # if it's a species
  # if (sapply(gregexpr("[[:alpha:]]+", problem), function(x) sum(x > 0)) == 2){
  #
  #   #if the matched name is that of a species
  if(any(sp$taxonRank %in% c("species", "subspecies", "variety") & !is.na(sp$taxonRank))){

    # to_add = test[test$scientificName %in% problem,][1,]
    to_add = test[test$full_name %in% problem,][1,]

    # if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
    if(any(sp$taxonomicStatus == "Accepted" & sp$Fuzzy.dist <3)){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))

      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))
      }
    }
    print("no match")
  } # if it was not matched
}


problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$full_name %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)

# see how many species were matched to different categories
length(obvious) # 1
length(accepted) # 0
length(split) # 0
length(synonym) # 0
length(homotypic) # 0
length(diff_author) # 0
length(wfo_match) # 3
length(problematic) # 3




##### NOW JOIN THE NAMES TO THE BRAHMS DATA EXTRACT ###############################

iucn_MSB_wcvp_matched = test[test$keep == 1,]
write.csv(iucn_MSB_wcvp_matched, paste0(basepath, "revision_1/iucn_brahms_unique_wcvp_matched_full_name.csv"), row.names = F)

iucn_brahms_wcvp_matched = iucn_brahms %>% left_join(iucn_MSB_wcvp_matched, by = "full_name")
write.csv(iucn_brahms_wcvp_matched, paste0(basepath, "revision_1/iucn_brahms_wcvp_matched_full_name.csv"), row.names = F)




###################################################################################################################
###################################################################################################################
###################################################################################################################
##############          HAWAII/BRYO/LYCO/FERN
###################################################################################################################
###################################################################################################################
###################################################################################################################



# NAME MATCH THE DATA NAOMI SENT
# extras = extras = read.csv(paste0(basepath,"CR_hawaii_bryo_lyco_fern.csv"))
extras = read.table(paste0(basepath, "CR_hawaii_bryo_lyco_fern.csv" ),
           sep = ",", quote = "\"",
           dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

# remove duplicates
extras <- extras[duplicated(extras$species_name_IUCN)==FALSE,] # removes 7 duplicates

# extract species
extras$species <- gsub("^([A-Z][a-z]+(?:\\s+×)?\\s+\\w+).*", "\\1", extras$species_name_IUCN) # gsub("^(\\S+ \\S+).*", "\\1", extras$species_name_IUCN) #gsub("^(\\w+ \\w+).*", "\\1", brahms$Taxon)
extras$species <- trimws(extras$species)

# extract subspecies
subspecies_match <- regexpr("subsp\\. \\w+", extras$species_name_IUCN)
extras$subspecies <- substring(extras$species_name_IUCN, subspecies_match,
                               subspecies_match + attr(subspecies_match, "match.length") - 1)

# extract variety
variety_match <- regexpr("var\\. \\w+", extras$species_name_IUCN)
extras$var <- substring(extras$species_name_IUCN, variety_match,
                        variety_match + attr(variety_match, "match.length") - 1)


# now save the rest of the string as the author name
# Remove species and subspecies information to get author
# extras$author <- gsub("^(\\S+ \\S+)", "", extras$species_name_IUCN) # gsub("^(\\w+ \\w+)", "", brahms$Taxon)  # Remove species
# extras$author <- gsub("subsp\\. \\w+", "", extras$author)      # Remove subspecies
# extras$author <- gsub("var\\. \\w+", "", extras$author)
# extras$author <- trimws(extras$author)
# # brahms$subspecies_name = trimws(paste(brahms$species,brahms$subspecies))


# edit code to include var and other if needed
extras$full_name = trimws(paste(extras$species,extras$subspecies,extras$var))
extras$full_name = gsub("\\s+"," ",extras$full_name)


###### NAME MATCHING ###########################################################

### Note: rWCVP sometimes returns multiple matches for one name. This bit of code
###       follows protocol from https://nph.onlinelibrary.wiley.com/doi/full/10.1002/ppp3.10146
###       whereby if there are multiple names, 1st we use accepted name,
###       if no accepted name, we use synonym, and if no synonym, we use
###       homotypic synonym.



# This bit of code dates 5 minutes to run, and is therefore saved to avoid rerun
# MSB species
extras_wcvp = wcvp_match_names(extras, wcvp,
                 name_col = "full_name")
extras_wcvp = extras_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name", "taxon_authors")],
                           by=c("wcvp_accepted_id" = "plant_name_id"))

write.csv(extras_wcvp, paste0(basepath,"revision_1/extras_wcvp_full_name.csv"), row.names = F)
# #extras_wcvp = read.csv(paste0(basepath,"extras_wcvp_full_name.csv"))
extras_wcvp = read.table(paste0(basepath, "revision_1/extras_wcvp_full_name.csv" ),
           sep = ",", quote = "\"",
           dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

# Put the data in test to avoid overwriting for time being...
test = extras_wcvp
test$duplicated = test$full_name %in% unique(test$full_name[ duplicated(test$full_name)])
test$accepted_name = test$wcvp_status == "Accepted"
test$names_match = apply(test, 1, function(row) fuzzy_match(row["full_name"], row["taxon_name"]))#test$species == test$taxon_name
test$keep = ifelse(test$duplicated,0,1)
test$keep = ifelse(test$accepted_name & test$names_match & test$duplicated, 1, test$keep)


obvious = test$full_name[test$accepted_name & test$names_match & test$duplicated]

# find duplicated names that don't have any accepted name
dupl = test$full_name %in% unique(test$full_name[ duplicated(test$full_name)])
dupl_nam = unique(test$full_name[dupl])
#create some empty variables to fill during analysis
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

problematic = c(problematic,test$full_name[test$duplicated==F & test$keep == 1 & is.na(test$taxon_name)])
problematic = c(problematic, unique(test$full_name[test$match_similarity < 0.9]))
problematic <- unique(problematic)
problematic <- problematic[which(!is.na(problematic))]
test$keep = ifelse(test$duplicated==F & test$keep == 1 & is.na(test$taxon_name), 0, test$keep)



# now keep track how the matching was done and why
test$match_logic = ifelse(test$duplicated,"unmatched","unique")
test$match_logic = ifelse(test$accepted_name & test$names_match & test$duplicated, "unique", test$match_logic)
test$match_logic[test$full_name %in% obvious] = "matched"# had duplicates, but one name reads the same and and is accepted
test$match_logic[test$full_name %in% accepted] = "accepted" # had duplicates, but the name isn't the same but is accepted
test$match_logic[test$full_name %in% synonym] = "synonym" # had duplicates, but the name isn't the same but is a synonym
test$match_logic[test$full_name %in% homotypic] = "homotypic"  #had duplicates, but the name isn't the same but is a homotypic synonym
test$match_logic[test$full_name %in% diff_author] = "diff_author" #find closest author name (sometimes there are additional parentheses and dots)

test$taxonomic_backbone = "WCVP"
test$taxonomic_backbone[test$full_name %in% problematic] = NA
# test$taxonomic_backbone[test$scientificName %in% problematic] = "WFO"

#add family
test = test %>% left_join(wcvp[, c("plant_name_id", "family", "genus")],
                          by=c("wcvp_accepted_id" = "plant_name_id"))

test = test %>% left_join(rWCVP::taxonomic_mapping,
                          by=c("family" = "family"))


# now check them out on WFO
match = test[which(test$full_name %in% problematic),]

# pb_sp = WorldFlora::WFO.match(spec.data = match$scientificName, WFO.data=WFO.data, counter=1, verbose=TRUE)

pb_sp = WorldFlora::WFO.match(spec.data = match$full_name,
                              WFO.data = WFO.data,
                              # spec.name = "full_name",
                              # Authorship = "author",
                              counter=1, verbose=TRUE)

write.csv(pb_sp, paste0(basepath, "revision_1/extras_wfo_matched.csv"), row.names = F)
# #pb_sp =read.csv(paste0(basepath, "revision_1/extras_wfo_matched.csv"))
pb_sp <- read.table(paste0(basepath, "revision_1/extras_wfo_matched.csv" ),
                         sep = ",", quote = "\"",
                         dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")

wfo_match = c()
pb_sp$Fuzzy.dist[is.na(pb_sp$Fuzzy.dist)] <- 0

for(problem in problematic){
  print(problem)
  # test[test$scientificName %in% problem,]
  sp = pb_sp[pb_sp$spec.name.ORIG %in% problem,]
  sp = sp[which(!duplicated(sp$scientificNameID)),]

  # if(any(sp$taxonRank == "species" & !is.na(sp$taxonRank))){
  if(any(sp$taxonRank %in% c("species", "subspecies", "variety") & !is.na(sp$taxonRank))){

    to_add = test[test$scientificName %in% problem,][1,]

    # if(any(sp$taxonomicStatus == "Accepted" & !is.na(sp$taxonomicStatus))){
    if(any(sp$taxonomicStatus == "Accepted" & sp$Fuzzy.dist <3)){
      if (length(which(sp$taxonomicStatus == "Accepted")) == 1){


        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))

      } else { # if there is more than 1

        to_add = to_add[rep(seq_len(nrow(to_add)), each = length(which(sp$taxonomicStatus == "Accepted"))), ]

        wfo_match = c(wfo_match, problem)

        i = which(sp$taxonomicStatus== "Accepted")
        to_add$keep = 1
        to_add$multiple_matches = ifelse(length(sp$taxonomicStatus== "Accepted"),TRUE,FALSE)
        to_add$match_similarity = NA
        to_add$match_edit_distance = sp$Fuzzy.dist[i]
        to_add$wcvp_id = sp$taxonID[i]
        to_add$wcvp_name = sp$scientificName[i] # sp[i,18]
        to_add$wcvp_authors = sp$scientificNameAuthorship[i]
        to_add$wcvp_rank = sp$taxonRank[i]
        to_add$wcvp_status = sp$taxonomicStatus[i]
        to_add$wcvp_homotypic = NA
        to_add$wcvp_ipni_id = sp$scientificNameID[i]
        to_add$wcvp_accepted_id = sp$parentNameUsageID[i]
        to_add$taxon_name  = sp$scientificName[i] # sp[i,18]
        to_add$taxon_authors  = sp$scientificNameAuthorship[i]
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
        print(paste("added", sp$scientificName[i]))

      }
    }
    print("no match")
  }
}

problematic = problematic[!(problematic %in% wfo_match)]
test$match_logic[test$scientificName %in% problematic] = "unmatched" #find closest author name (sometimes there are additional parentheses and dots)

# see how many species were matched to different categories
length(obvious) # 37
length(accepted) # 0
length(split) # 0
length(synonym) # 6
length(homotypic) # 0
length(diff_author) # 0
length(wfo_match) # 54
length(problematic) # 13


##### NOW JOIN THE NAMES TO THE BRAHMS DATA EXTRACT ###############################

extras_wcvp_matched = test[test$keep == 1,]
# extras_wcvp_matched = extras %>% left_join(extras_wcvp_matched, by = "full_name")
write.csv(extras_wcvp_matched, paste0(basepath, "revision_1/extras_wcvp_matched_full_name.csv"), row.names = F)

