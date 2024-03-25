library(dplyr)
library(stringr)
# library(fuzzyjoin)
library(stringdist)
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

###### FUNCTIONS ###############################################################

# Function to perform fuzzy matching
fuzzy_match <- function(str1, str2) {
  distance <- stringdist(str1, str2, method = "jw")  # Using Jaro-Winkler distance
  return(distance < 0.1)  # Adjust threshold as needed
}


###### LOAD DATA ###############################################################

# load brahms data
brahms <- read.csv(paste0(basepath,"2024-03-21_164953044-BRAHMSOnlineData.csv"))

# load IUCN
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
length(iucn$scientificName) # 5702


# remove duplicates
brahms <- brahms[duplicated(brahms$AccessionNumber)==FALSE,] # removes 441 duplicates
#Get rid of end of species names so that they are easier to match
# brahms$species <- word(brahms$Taxon, 1,2)
# word(brahms$Taxon[5], 3, str_count(brahms$Taxon[5], '\\w+'))
# brahms$author <- word(brahms$Taxon, 3,
#                       lengths(strsplit(brahms$Taxon, "\\s+")))

# extract species
brahms$species <- gsub("^(\\w+ \\w+).*", "\\1", brahms$Taxon)

# extract subspecies
subspecies_match <- regexpr("subsp\\. \\w+", brahms$Taxon)
brahms$subspecies <- substring(brahms$Taxon, subspecies_match,
                               subspecies_match + attr(subspecies_match, "match.length") - 1)

# extract variety
variety_match <- regexpr("var\\. \\w+", brahms$Taxon)
brahms$var <- substring(brahms$Taxon, variety_match,
                        variety_match + attr(variety_match, "match.length") - 1)

# now save the rest as author
# Remove species and subspecies information to get author
brahms$author <- gsub("^(\\w+ \\w+)", "", brahms$Taxon)  # Remove species
brahms$author <- gsub("subsp\\. \\w+", "", brahms$author)      # Remove subspecies
brahms$author <- gsub("var\\. \\w+", brahms$Taxon)
brahms$author <- trimws(brahms$author)
brahms$subspecies_name = paste(brahms$species,brahms$subspecies)



# create summary table where adjusted seed counts per species are added together
spp_count = brahms %>%
  group_by(species,AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(summed_count = sum(AdjustedSeedQuantity),
         count = n()) %>%
  ungroup()

# bit hacky, but get rid of adjusted count and duplicates
spp_count = spp_count[, c("species", "count", "summed_count")]
spp_count = spp_count[duplicated(spp_count$species)==FALSE,]
spp_count$CR = ifelse(spp_count$species %in% iucn$scientificName,1,0)

# get ipni IDs

# quick crossref
length(which(unique(brahms$species) %in% iucn$scientificName)) #293 species

###### NAME MATCHING ###########################################################
library(rWCVP)
library(rWCVPdata)
# Load POWO/wcvp
wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

######## CODE to name match ####################################################
# slow to run 10 mins therefore saved and can be loaded
# iucn_wcvp = wcvp_match_names(iucn, wcvp_names,
#                  name_col = "scientificName",
#                  id_col = "assessmentId")
# iucn_wcvp = iucn_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
#                            by=c("wcvp_accepted_id" = "plant_name_id"))
# write.csv(iucn_wcvp, paste0(basepath,"iucn_wcvp.csv"))
# slow to run 35 mins therefore saved and can be loaded
# brahms_wcvp = wcvp_match_names(brahms, wcvp_names,
#                                name_col = "subspecies_name",
#                                id_col = "AccessionNumber",
#                                author_col = "author")
brahms_wcvp = brahms_wcvp %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
                                        by=c("wcvp_accepted_id" = "plant_name_id"))

# find duplicated accessions with multiple names found in WCVP
# and ch
test = brahms_wcvp
test$duplicated = brahms_wcvp$AccessionNumber %in% unique(brahms_wcvp$AccessionNumber[ duplicated(brahms_wcvp$AccessionNumber)])
test$accepted_name = test$wcvp_status == "Accepted"
test$names_match = test$species == test$taxon_name
test$keep = ifelse(test$duplicated,0,1)
test$keep = ifelse(test$accepted_name & test$names_match & test$duplicated, 1, test$keep)


dupl = brahms_wcvp$AccessionNumber %in% unique(brahms_wcvp$AccessionNumber[ duplicated(brahms_wcvp$AccessionNumber)])
dupl_nam = unique(brahms_wcvp$AccessionNumber[dupl])
problematic = c()
for (du in dupl_nam){
  temp = brahms_wcvp[brahms_wcvp$AccessionNumber == du,]
  acc = temp$wcvp_status == "Accepted"
  name = apply(temp, 1, function(row) fuzzy_match(row["species"], row["taxon_name"]))#temp$species == temp$taxon_name
  if (!(any(acc) & any(name))) problematic = c(problematic, du)
}
problematic

for (pb in problematic){
  temp = brahms_wcvp[brahms_wcvp$AccessionNumber == du,]
  acc = temp$wcvp_status == "Accepted"
  name = apply(temp, 1, function(row) fuzzy_match(row["species"], row["taxon_name"]))#temp$species == temp$taxon_name
  if (!(any(acc) & any(name))) problematic = c(problematic, du)
}
temp = data.frame(brahms_wcvp[brahms_wcvp$AccessionNumber == problematic[8],])
temp2 = data.frame(brahms_wcvp[brahms_wcvp$AccessionNumber == problematic[9],1:10])

# extract species
temp2$Species <- gsub("^(\\w+ \\w+).*", "\\1", temp2$Taxon)

# extract subspecies
subspecies_match <- regexpr("subsp\\. \\w+", temp2$Taxon)
temp2$subspecies <- substring(temp2$Taxon, subspecies_match, subspecies_match + attr(subspecies_match, "match.length") - 1)

# extract variety
variety_match <- regexpr("var\\. \\w+", temp2$Taxon)
temp2$var <- substring(temp2$Taxon, variety_match, variety_match + attr(variety_match, "match.length") - 1)

# now save the rest as author
# Remove species and subspecies information to get author
temp2$author <- gsub("^(\\w+ \\w+)", "", temp2$Taxon)  # Remove species
temp2$author <- gsub("subsp\\. \\w+", "", temp2$author)      # Remove subspecies
temp2$author <- gsub("var\\. \\w+", temp2$Taxon)

temp2$author <- trimws(temp2$author)

temp2$subspecies_name = paste(temp2$Species,temp2$subspecies)


temp3 = wcvp_match_names(temp2, wcvp,
                 name_col = "subspecies_name",
                 id_col = "AccessionNumber",
                 author_col = "author")
data.frame(temp3)


temp3 = temp3 %>% left_join(wcvp[,c("plant_name_id","taxon_name")],
              by=c("wcvp_id" = "plant_name_id"))
data.frame(temp3)




temp = brahms_wcvp[brahms_wcvp$AccessionNumber == problematic[2],]
acc = temp$wcvp_status == "Accepted"
name = temp$species == temp$taxon_name


# Apply the function to each row
temp$fuzzy_match_result <- apply(temp, 1, function(row) fuzzy_match(row["species"], row["taxon_name"]))
temp$fuzzy_match_result

data.frame(stringdist_join(temp, temp, by = c("species", "taxon_name")))



# write.csv(brahms_wcvp, paste0(basepath,"brahms_wcvp2.csv"))


iucn_wcvp = read.csv(paste0(basepath,"iucn_wcvp.csv"))
brahms_wcvp = read.csv(paste0(basepath,"brahms_wcvp.csv"))
