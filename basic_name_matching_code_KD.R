library(dplyr)
library(stringr)
# library(fuzzyjoin)
library(stringdist)
library(rWCVP)


# Function to perform fuzzy matching
fuzzy_match <- function(str1, str2) {
  distance <- stringdist(str1, str2, method = "jw")  # Using Jaro-Winkler distance
  return(distance < 0.1)  # Adjust threshold as needed
}




# load IUCN
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
length(iucn$scientificName) # 5702

# run in rWCVP

# slow to run therefore have commented out to prevent accidentally re-running
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

#Go over all the species where wcvp has returned multiple outputs

for (du in dupl_nam){
  temp = data.frame(test[test$scientificName == du,]) # temporary file that gets overwritten for each name with duplicates
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

    # if the names are the same, keep the one with the smallest author distance because there might be a typo in the author name
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

# add the data where there are no duplicates, but no taxon name to the problematic count
problematic = c(problematic,test$scientificName[test$duplicated==F & test$keep == 1 & is.na(test$taxon_name)])

# Also don't keep them if the taxon name is na
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


