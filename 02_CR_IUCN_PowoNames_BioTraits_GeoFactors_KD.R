#### libraries #####
#install.packages("rlang")
#install.packages("assertthat")
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("writexl")
#install.packages("vioplot")
#install.packages("ggpubr")
#install.packages("tsibble")
#install.packages(c("rgeos", "rnaturalearth", "sp", "sf", "rnaturalearthdata"))
#install.packages(c("cowplot", "googleway", "ggrepel","ggspatial", "libwgeom"))

install.packages(c("dplyr",
                   "ggplot2",
                   "stringr",
                   "writexl",
                   "plyr",
                   "purrr",
                   "gtools",
                   "vioplot",
                   "utils",
                   "ggpubr",
                   "data.table",
                   "tsibble",
                   "GGally"))



library(rlang)
library(assertthat)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
library(writexl)
library(plyr)
library(purrr)
library(gtools)
library(vioplot)
library(utils)
library(ggpubr)
require(data.table)
library(tsibble)
library(GGally)

library(sp)
library(sf)
require(rnaturalearth)
library(rnaturalearthdata)
require(rgeos)

######## Explore IUCN data Aug 2021 #####
#
# ## Import IUCN data --
# iucn_assess <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\CR_IUCN_Aug21\\IUCN data species & subspecies/assessments.csv")
# iucn_taxo <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\CR_IUCN_Aug21\\IUCN data species & subspecies/taxonomy.csv")
#
# length(unique(iucn_assess$scientificName)) #4878 unique values
# length(unique(iucn_taxo$scientificName)) #4878 unique values
#
# iucn_taxo$species2 <- word(iucn_taxo$scientificName, 1,2)
# length(unique(iucn_taxo$scientificName)) #4878
#
# ## Assessment by year
# table(iucn_assess$yearPublished)
#
# ### IUCN data by taxonomic groups --
# table(iucn_taxo$orderName)
# table(iucn_taxo$phylumName)
#
# #Create groups: Bryophytes, Lycophytes, Ferns, Gymnosperms, Angiosperms
# iucn_taxo$groups <- ifelse(iucn_taxo$orderName=="DICRANALES"|iucn_taxo$orderName=="GRIMMIALES"|iucn_taxo$orderName=="HOOKERIALES"|iucn_taxo$orderName=="HYPNALES"|
#                               iucn_taxo$orderName=="JUNGERMANNIALES"|iucn_taxo$orderName=="MARCHANTIALES"|iucn_taxo$orderName=="METZGERIALES"|iucn_taxo$orderName=="ORTHOTRICHALES"|
#                               iucn_taxo$orderName=="PORELLALES"|iucn_taxo$orderName=="POTTIALES"|iucn_taxo$orderName=="SPHAEROCARPALES"|iucn_taxo$orderName=="SPHAGNALES", "Bryophytes",
#                             ifelse(iucn_taxo$orderName=="ISOETALES"|iucn_taxo$orderName=="LYCOPODIALES", "Lycophytes",
#                                    ifelse(iucn_taxo$orderName=="HYMENOPHYLLALES"|iucn_taxo$orderName=="MARATTIALES"|
#                                             iucn_taxo$orderName=="OPHIOGLOSSALES"| iucn_taxo$orderName=="POLYPODIALES" | iucn_taxo$orderName=="SALVINIALES", "Ferns",
#                                           ifelse(iucn_taxo$orderName=="CYCADALES"|iucn_taxo$orderName=="PINALES", "Gymnosperms", "Angiosperms"))))
#
# table(iucn_taxo$phylumName, iucn_taxo$groups)
# mytable<- table(iucn_taxo$groups)
# lbls<- paste(names(mytable))
# pie(mytable, lbls)
#
# #KD has out to avoid overwriting existing tables
# # # write_xlsx(iucn_taxo, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Databases\\CR_IUCN_Aug21\\IUCN data species & subspecies\\iucn_taxo_groups.xlsx")


####### Explore EX species IUCN Sep 21 #####
extint<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\EX_IUCN_Sep21/assessments.csv")
length(unique(extint$scientificName)) #176

semi_join(extint, data, by=c("scientificName"="species3"))
semi_join(data, extint, by=c("species3"="scientificName"))
######## Explore IUCN data September 2021 #####
iucn_assess_sep <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/CR_IUCN_Sep21/assessments.csv")
iucn_taxo_sep <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/CR_IUCN_Sep21/taxonomy.csv")
iucn_syn_sep <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/CR_IUCN_Sep21/synonyms.csv")

length(unique(iucn_assess_sep$scientificName)) #5026 unique values
length(unique(iucn_taxo_sep$scientificName)) #5026 unique values
length(unique(iucn_syn_sep$scientificName)) #1084 unique values

## Species by taxonomic group ------
table(iucn_taxo_sep$orderName)

#Create groups: Bryophytes, Lycophytes, Ferns, Gymnosperms, Angiosperms
iucn_taxo_sep$groups <- ifelse(iucn_taxo_sep$orderName=="DICRANALES"|iucn_taxo_sep$orderName=="GRIMMIALES"|iucn_taxo_sep$orderName=="HOOKERIALES"|iucn_taxo_sep$orderName=="HYPNALES"|
                                 iucn_taxo_sep$orderName=="JUNGERMANNIALES"|iucn_taxo_sep$orderName=="MARCHANTIALES"|iucn_taxo_sep$orderName=="METZGERIALES"|iucn_taxo_sep$orderName=="ORTHOTRICHALES"|
                                 iucn_taxo_sep$orderName=="PORELLALES"|iucn_taxo_sep$orderName=="POTTIALES"|iucn_taxo_sep$orderName=="SPHAEROCARPALES"|iucn_taxo_sep$orderName=="SPHAGNALES", "Bryophytes",
                               ifelse(iucn_taxo_sep$orderName=="ISOETALES"|iucn_taxo_sep$orderName=="LYCOPODIALES", "Lycophytes",
                                      ifelse(iucn_taxo_sep$orderName=="HYMENOPHYLLALES"|iucn_taxo_sep$orderName=="MARATTIALES"|
                                               iucn_taxo_sep$orderName=="OPHIOGLOSSALES" |iucn_taxo_sep$orderName=="POLYPODIALES" | iucn_taxo_sep$orderName=="SALVINIALES", "Ferns",
                                             ifelse(iucn_taxo_sep$orderName=="CYCADALES"|iucn_taxo_sep$orderName=="PINALES", "Gymnosperms", "Angiosperms"))))

table(iucn_taxo_sep$groups)


#Create variable for sp. name (no subsp. or vr.)
iucn_taxo_sep$species_name<- paste(iucn_taxo_sep$genusName, iucn_taxo_sep$speciesName, sep=" ")
length(unique(iucn_taxo_sep$species_name))

#Number os species by taxonomic group
df.group= iucn_taxo_sep[!duplicated(iucn_taxo_sep$species_name),]
table(df.group$group)

## Compare species with IUCN data Aug 2021 ------
new_CR <- anti_join(iucn_taxo_sep, iucn_taxo, by="scientificName")
length(unique(new_CR$scientificName))
old_CR <- anti_join(iucn_taxo, iucn_taxo_sep, by="scientificName")
length(unique(old_CR$scientificName))

new_CR_IUCN<- as.data.frame(unique(new_CR$scientificName))
old_CR_IUCN <- as.data.frame(unique(old_CR$scientificName))

# # write_xlsx(new_CR_IUCN, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Databases\\CR_IUCN_Sep21/new_CR_IUCN.xlsx")
# # write_xlsx(old_CR_IUCN, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Databases\\CR_IUCN_Sep21/old_CR_IUCN.xlsx")

#Check is some of the old CR species are in the data warehouse
semi_join(data, old_CR, by=c("species3"="scientificName"))
semi_join(old_CR, data, by=c("scientificName"="species3"))
# No CR species from the data warehouse are now outside the IUCN CR species list


## Check synonyms in IUCN -----
head(iucn_syn_sep)

#Create variable for synonym sp. name
iucn_syn_sep$synonym<- paste(iucn_syn_sep$genusName, iucn_syn_sep$speciesName, sep=" ")
#Create database with synonyms and taxonomic information
anti_join(iucn_syn_sep, iucn_taxo_sep, by="scientificName") # All sp. from synonym dataframe are in iucn dataframe
iucn_syn_sep <- merge(iucn_syn_sep, iucn_taxo_sep[, c(2,17)], by="scientificName")


######## POWO: Check IUCN names with POWO database-----------

### Import POWO databases -----
#Naomi data
powo_feb<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/world_checklist_names_and_distribution_feb_21/checklist_names.txt", header=T, sep="|")
powo_jul<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/world_checklist_names_and_distribution_JUL_21/checklist_names.txt", header=T, sep="|")
#Rafael Govaerts data
powo_jun<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/POWO/wcvp_v5_jun_2021/wcvp_v5_jun_2021.txt", header=T, sep="|")

head(powo_jun)
length(unique(powo_jun$taxon_name)) # 1148374
length(unique(powo_jun$accepted_name)) # 206117
length(unique(powo_feb$taxon_name)) # 448453
length(unique(powo_jul$taxon_name)) # 526233

#Create variable for sp. name (no subsp. or var.)
powo_jun$species_name <- paste(powo_jun$genus,powo_jun$species, sep=" ")
powo_feb$species_name <- paste(powo_feb$genus, powo_feb$species, sep=" ")
powo_jul$species_name <- paste(powo_jul$genus, powo_jul$species, sep=" ")

length(unique(powo_jun$species_name)) #978886
length(unique(powo_feb$species_name)) #340484
length(unique(powo_jul$species_name)) #404569



### IUCN names present in POWO ----
iucn_names_powo<- merge(iucn_taxo_sep, powo_jun[,c(19,6,7,9,2,11,12)], by="species_name")  # names present in POWO
names(iucn_names_powo)[2:16] <- paste(names(iucn_names_powo)[2:16],"IUCN", sep="_")
names(iucn_names_powo)[18:23] <- paste(names(iucn_names_powo)[18:23],"POWO", sep="_")
length(unique(iucn_names_powo$species_name)) # 4795
length(unique(iucn_names_powo$scientificName_IUCN)) #4857

# duplicated names in general (species name)-----
iucn_names_powo_dup<- iucn_names_powo[duplicated(iucn_names_powo$species_name),] #names with duplicates
iucn_names_powo_dup2<- semi_join(iucn_names_powo, iucn_names_powo_dup, by="species_name") #select all rows that have duplicated names
length(unique(iucn_names_powo_dup2$species_name)) # 300
length(unique(iucn_names_powo_dup2$scientificName_IUCN)) #362

#For duplicated sp. names, check in the subsp. or var. + author are present in POWO  (those will be the definitive names in POWO)
iucn_names_powo_dup3 <- iucn_names_powo_dup2[(as.character(iucn_names_powo_dup2$scientificName_IUCN) == as.character(iucn_names_powo_dup2$taxon_name_POWO))
                                             & as.character(iucn_names_powo_dup2$authority_IUCN) == as.character(iucn_names_powo_dup2$authors_POWO),]
iucn_names_powo_dup3$accepted_name_POWO <- ifelse(iucn_names_powo_dup3$taxonomic_status_POWO=="Accepted",
                                                  as.character(iucn_names_powo_dup3$taxon_name_POWO), as.character(iucn_names_powo_dup3$accepted_name_POWO))
iucn_names_powo_dup3$accepted_authors_POWO <- ifelse(iucn_names_powo_dup3$taxonomic_status_POWO=="Accepted",
                                                  as.character(iucn_names_powo_dup3$authors_POWO), as.character(iucn_names_powo_dup3$accepted_authors_POWOO))
iucn_names_powo_dup3$reason<- "complete match"
# write_xlsx(iucn_names_powo_dup3,"C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/\\POWO/iucn_names_powo_dup3.xlsx")
dim(iucn_names_powo_dup3)
length(unique(iucn_names_powo_dup3$scientificName_IUCN)) #214
length(unique(iucn_names_powo_dup3$species_name)) #188

#Duplicated full names (subsp. and var.) with no matching author in POWO
iucn_names_powo_dup4 <- anti_join(iucn_names_powo_dup2, iucn_names_powo_dup3, by="scientificName_IUCN")
iucn_names_powo_dup4$accepted_name_POWO <- ifelse(iucn_names_powo_dup4$taxonomic_status_POWO=="Accepted",
                                                  as.character(iucn_names_powo_dup4$taxon_name_POWO), as.character(iucn_names_powo_dup4$accepted_name_POWO))
iucn_names_powo_dup4$accepted_authors_POWO <- ifelse(iucn_names_powo_dup4$taxonomic_status_POWO=="Accepted",
                                                     as.character(iucn_names_powo_dup4$authors_POWO), as.character(iucn_names_powo_dup4$accepted_authors_POWOO))
# write_xlsx(iucn_names_powo_dup4,"C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Databases\\POWO/iucn_names_powo_dup4.xlsx")
dim(iucn_names_powo_dup4) # 544
length(unique(iucn_names_powo_dup4$scientificName_IUCN)) #148
length(unique(iucn_names_powo_dup4$species_name)) #130

#Import the duplicated full names with the manually selected names
iucn_names_powo_dup4_filtered<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_names_powo_dup4_filtered_selected.csv", sep=";", head=T)
dim(iucn_names_powo_dup4_filtered)
length(unique(iucn_names_powo_dup4_filtered$scientificName_IUCN)) #148

# non-duplicated names -----
iucn_names_powo_nondup<- anti_join(iucn_names_powo, iucn_names_powo_dup, by="species_name") #names that are not duplicated
length(unique(iucn_names_powo_nondup$species_name)) #4495
length(unique(iucn_names_powo_nondup$scientificName_IUCN)) #4495
#For non-duplicated names, check in the subsp. or var. + author are present in POWO
iucn_names_powo_nondup2 <- iucn_names_powo_nondup[(as.character(iucn_names_powo_nondup$scientificName_IUCN) == as.character(iucn_names_powo_nondup$taxon_name_POWO))
                                             & as.character(iucn_names_powo_nondup$authority_IUCN) == as.character(iucn_names_powo_nondup$authors_POWO),]
length(unique(iucn_names_powo_nondup2$species_name)) #2920
length(unique(iucn_names_powo_nondup2$scientificName_IUCN)) #2920
iucn_names_powo_nondup2$accepted_name_POWO<- ifelse(iucn_names_powo_nondup2$taxonomic_status_POWO=="Accepted",
                                                   as.character(iucn_names_powo_nondup2$taxon_name_POWO),as.character(iucn_names_powo_nondup2$accepted_name_POWO))
iucn_names_powo_nondup2$accepted_authors_POWO<- ifelse(iucn_names_powo_nondup2$taxonomic_status_POWO=="Accepted",
                                                      as.character(iucn_names_powo_nondup2$authors_POWO),as.character(iucn_names_powo_nondup2$accepted_authors_POWO))
iucn_names_powo_nondup2$reason<- "complete match"
#For non-duplicated names, check in the subsp. or var. are present in POWO
iucn_names_powo_nondup3 <- iucn_names_powo_nondup[(as.character(iucn_names_powo_nondup$scientificName_IUCN) == as.character(iucn_names_powo_nondup$taxon_name_POWO))
                                                  & as.character(iucn_names_powo_nondup$authority_IUCN) != as.character(iucn_names_powo_nondup$authors_POWO),]
length(unique(iucn_names_powo_nondup3$species_name)) #1553
length(unique(iucn_names_powo_nondup3$scientificName_IUCN)) #1553
iucn_names_powo_nondup3$accepted_name_POWO<- ifelse(iucn_names_powo_nondup3$taxonomic_status_POWO=="Accepted",
                                                    as.character(iucn_names_powo_nondup3$taxon_name_POWO),as.character(iucn_names_powo_nondup3$accepted_name_POWO))
iucn_names_powo_nondup3$accepted_authors_POWO<- ifelse(iucn_names_powo_nondup3$taxonomic_status_POWO=="Accepted",
                                                       as.character(iucn_names_powo_nondup3$authors_POWO),as.character(iucn_names_powo_nondup3$accepted_authors_POWO))
iucn_names_powo_nondup3$reason<- "author missmatch"
#For non-duplicated names, cases when subs. and var. level were not matching in POWO
iucn_names_powo_nondup4 <- iucn_names_powo_nondup[(as.character(iucn_names_powo_nondup$scientificName_IUCN) != as.character(iucn_names_powo_nondup$taxon_name_POWO)),]
length(unique(iucn_names_powo_nondup4$species_name)) #22
length(unique(iucn_names_powo_nondup4$scientificName_IUCN)) #22
iucn_names_powo_nondup4$accepted_name_POWO<- ifelse(iucn_names_powo_nondup4$taxonomic_status_POWO=="Accepted",
                                                    as.character(iucn_names_powo_nondup4$taxon_name_POWO),as.character(iucn_names_powo_nondup4$accepted_name_POWO))
iucn_names_powo_nondup4$accepted_authors_POWO<- ifelse(iucn_names_powo_nondup4$taxonomic_status_POWO=="Accepted",
                                                       as.character(iucn_names_powo_nondup4$authors_POWO),as.character(iucn_names_powo_nondup4$accepted_authors_POWO))
#some of them are hybrids
iucn_names_powo_nondup4$reason<- ifelse(grepl("?",iucn_names_powo_nondup4$taxon_name_POWO),"hybrid","species level")

# Join different subset of dataframe -> Dataframe with the status in POWO------
#Rbind all database with non duplicated names
iucn_names_powo_nondup5<- rbind(iucn_names_powo_nondup2,iucn_names_powo_nondup3,iucn_names_powo_nondup4)
# write_xlsx(iucn_names_powo_nondup5,"C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_names_powo_nondup.xlsx")
dim(iucn_names_powo_nondup5)
length(unique(iucn_names_powo_nondup5$species_name)) #4495
length(unique(iucn_names_powo_nondup5$scientificName_IUCN)) #4495

#Rbind databases that had dulplicated names and database with no duplicated names
iucn_in_powo <- rbind(iucn_names_powo_dup3,iucn_names_powo_dup4_filtered, iucn_names_powo_nondup5)
dim(iucn_in_powo) #4857
length(unique(iucn_in_powo$scientificName_IUCN))#4857

### IUCN names not present in POWO ------
no_match_iucn<- anti_join(iucn_taxo_sep, powo_jun, by="species_name")
length(unique(no_match_iucn$species_name)) # 169
length(unique(no_match_iucn$scientificName)) #169

# Check match with synonyms given by IUCN -------
length(unique(iucn_syn_sep$species_name)) #1070
length(unique(iucn_syn_sep$scientificName)) #1084
maybe.syn<- semi_join(iucn_syn_sep, no_match_iucn, by="scientificName")  # select the synonyms for names not present in POWO
length(unique(maybe.syn$species_name)) #42
length(unique(maybe.syn$scientificName)) #42

#database with IUCN synonyms and matching names in POWO
iucn_synonym_powo<- merge(maybe.syn, powo_jun[,c(19,6,7,9,2,11,12)], by.x="synonym",by.y="species_name")
colnames(iucn_synonym_powo)[1:10]<- paste(colnames(iucn_synonym_powo)[1:10],"IUCN", sep="_")
colnames(iucn_synonym_powo)[11:16] <- paste(colnames(iucn_synonym_powo)[11:16],"POWO", sep="_")

iucn_synonym_powo$accepted_name_POWO<- ifelse(iucn_synonym_powo$taxonomic_status_POWO=="Accepted",
                                                   as.character(iucn_synonym_powo$taxon_name_POWO),as.character(iucn_synonym_powo$accepted_name_POWO))
iucn_synonym_powo$accepted_authors_POWO<- ifelse(iucn_synonym_powo$taxonomic_status_POWO=="Accepted",
                                                      as.character(iucn_synonym_powo$authors_POWO),as.character(iucn_synonym_powo$accepted_authors_POWO))
# write_xlsx(iucn_synonym_powo, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/POWO/iucn_synonym_powo.xlsx")
length(unique(iucn_synonym_powo$scientificName_IUCN)) #35
length(unique(iucn_synonym_powo$species_name_IUCN)) #35
length(unique(iucn_synonym_powo$synonym_IUCN)) # 43
length(unique(iucn_synonym_powo$taxon_name_POWO)) # 49

#Import database with selected names with synonyms (after manually selecting them in excel)
iucn_synonym_powo_filtered <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_synonym_powo_filtered_selected.csv", sep=";")
#merge with iucn database
colnames(iucn_synonym_powo_filtered)[c(4,7)]<- paste("synonym", colnames(iucn_synonym_powo_filtered)[c(4,7)], sep="_")
iucn_synonym_powo_merged<- merge(iucn_synonym_powo_filtered[,c(1:4,7,10:16)], iucn_taxo_sep[,c(17,3:16)], by.y="species_name", by.x="species_name_IUCN")
colnames(iucn_synonym_powo_merged)[13:25] <- paste(colnames(iucn_synonym_powo_merged)[13:25], "IUCN", sep="_")
iucn_synonym_powo_merged$reason <- "synonym match"

# IUCN names not present in POWO (not even the synonyms) ----
iucn_nomatch_powo<- anti_join(no_match_iucn, match_syn, by="scientificName")
colnames(iucn_nomatch_powo) <- paste(colnames(iucn_nomatch_powo),"IUCN", sep="_")
dim(iucn_nomatch_powo) #134
length(unique(iucn_nomatch_powo$scientificName))#134
length(unique(iucn_nomatch_powo$species_name))#134

#Check them manually in POWO and The Plant List
# write_xlsx(iucn_nomatch_powo,"C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_nomatch_powo.xlsx")

#Upload database after manual check in POWO and Plant List
iucn_nomatch_powo_PlantList_check<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/POWO/iucn_nomatch_powo_manuallyCheck_POWO_PlantList.csv", sep=";")

#names that matched POWO manually
iucn_nomatch_powo_manual<- iucn_nomatch_powo_PlantList_check[iucn_nomatch_powo_PlantList_check$status_POWO_manually!="not found",]

# Check if names corrected with The Plant List match POWO
iucn_match_powo_PlantList_corrected<- semi_join(iucn_nomatch_powo_PlantList_check[,-c(18:22)], powo_jun, by=c("accepted_name_PlantList"="species_name"))
#add powo information
iucn_match_powo_PlantList_corrected2<- merge(iucn_match_powo_PlantList_corrected, powo_jun[,c(19,6,7,9,2,11,12)], by.x="accepted_name_PlantList", by.y="species_name", all.x=T)
colnames(iucn_match_powo_PlantList_corrected2)[21:26]<- paste(colnames(iucn_match_powo_PlantList_corrected2)[21:26], "POWO", sep="_")
iucn_match_powo_PlantList_corrected2$accepted_name_POWO <- ifelse(iucn_match_powo_PlantList_corrected2$taxonomic_status_POWO=="Accepted",
                                                  as.character(iucn_match_powo_PlantList_corrected2$taxon_name_POWO), as.character(iucn_match_powo_PlantList_corrected2$accepted_name_POWO))
iucn_match_powo_PlantList_corrected2$accepted_authors_POWO <- ifelse(iucn_match_powo_PlantList_corrected2$taxonomic_status_POWO=="Accepted",
                                                     as.character(iucn_match_powo_PlantList_corrected2$authors_POWO), as.character(iucn_match_powo_PlantList_corrected2$accepted_authors_POWOO))
iucn_match_powo_PlantList_corrected2$reason<- ifelse(iucn_match_powo_PlantList_corrected2$status_PlantList=="synonym","synonym match Plantlist", "misspelt")
#there are duplicates, filter them in excel manually
iucn_match_powo_PlantList_corrected2[duplicated(iucn_match_powo_PlantList_corrected2$scientificName_IUCN),]
# write_xlsx(iucn_match_powo_PlantList_corrected2,"C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_match_powo_PlantList_corrected2.xlsx")
#upload with filtered and selected species that were duplicated
iucn_match_powo_PlantList_corrected2_selected<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_match_powo_PlantList_corrected2_filtered_selected.csv", sep=";")

# NO match after checking manually in POWO and corrected names in Plant List ------
#Species still no matching after Plant List corrections
iucn_nomatch_plantlist <- anti_join(iucn_nomatch_powo_PlantList_check, iucn_match_powo_PlantList_corrected, by="species_name_IUCN")
#Still no matching after manual checking in POWO
iucn_nomatch_powo2 <- iucn_nomatch_plantlist[iucn_nomatch_plantlist$status_POWO_manually=="not found",]

length(unique(iucn_nomatch_powo2$scientificName))#95
length(unique(iucn_nomatch_powo2$species_name))#95

# write_xlsx(iucn_nomatch_powo2, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_nomatch_powo2.xlsx")

### Create database with all species after checking in POWO --------
#names in powo
dim(iucn_in_powo) #4857
length(iucn_in_powo$scientificName_IUCN) #4857
#synonyms in powo
dim(iucn_synonym_powo_merged) #35
length(iucn_synonym_powo_merged$scientificName_IUCN) #35
#manual match in POWO
dim(iucn_nomatch_powo_manual) #19
length(iucn_nomatch_powo_manual$scientificName_IUCN)#19
#corrected names in powo (after plant list)
dim(iucn_match_powo_PlantList_corrected2_selected) #20
length(iucn_match_powo_PlantList_corrected2_selected$scientificName_IUCN) #20
#no match in powo
dim(iucn_nomatch_powo2) #95
length(iucn_nomatch_powo2$scientificName_IUCN) #95

colnames(iucn_in_powo)
colnames(iucn_in_powo)[1] <- "species_name_IUCN"
colnames(iucn_synonym_powo_merged)
colnames(iucn_match_powo_PlantList_corrected2_selected)
colnames(iucn_match_powo_PlantList_corrected2_selected)[17] <- "groups"
colnames(iucn_nomatch_powo_manual)
colnames(iucn_nomatch_powo_manual)[16] <- "groups"
colnames(iucn_nomatch_powo_manual)[18]<- "reason" #the POWO status I gave by checking the species manually in POWO will be the reason
colnames(iucn_nomatch_powo2)
colnames(iucn_nomatch_powo2)[16] <- "groups"
colnames(iucn_nomatch_powo2)[18]<- "reason"

iucn_CR_powo_names<- rbind.fill(iucn_in_powo, iucn_synonym_powo_merged, iucn_nomatch_powo_manual, iucn_match_powo_PlantList_corrected2_selected, iucn_nomatch_powo2)
dim(iucn_CR_powo_names) #5026
length(unique(iucn_CR_powo_names$species_name_IUCN)) #4964
length(unique(iucn_CR_powo_names$scientificName_IUCN)) #5026

#Give the new names: POWO accepted names, if not the POWO name that is not classified as accepted, is not POWO name then Plant List name, if the name not in POWO or plant List then the name by IUCN
iucn_CR_powo_names <- iucn_CR_powo_names %>% mutate_all(na_if,'')
iucn_CR_powo_names$new_names <- ifelse(!is.na(iucn_CR_powo_names$accepted_name_POWO), as.character(iucn_CR_powo_names$accepted_name_POWO),
                                       ifelse(is.na(iucn_CR_powo_names$accepted_name_POWO) & !is.na(iucn_CR_powo_names$taxon_name_POWO), as.character(iucn_CR_powo_names$taxon_name_POWO),
                                              ifelse(is.na(iucn_CR_powo_names$accepted_name_POWO) & is.na(iucn_CR_powo_names$taxon_name_POWO) & !is.na(iucn_CR_powo_names$accepted_name_PlantList),
                                                     as.character(iucn_CR_powo_names$accepted_name_PlantList), as.character(iucn_CR_powo_names$scientificName_IUCN))))


# write_xlsx(iucn_CR_powo_names, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_CR_powo_names.xlsx")

######## Create species list for Predictor tool -----

# Names in IUCN
CR_iucn<- data.frame(species_name= iucn_taxo_sep$species_name,
                     scientific_name= iucn_taxo_sep$scientificName,
                     group= iucn_taxo_sep$groups,
                     internalTaxonId= iucn_taxo_sep$internalTaxonId)
dim(CR_iucn) #5026
length(unique(CR_iucn$species_name)) #4964

#Not subsp. or var. level: Only species level (genus + ephitet)
CR_iucn2<- CR_iucn[!duplicated(CR_iucn$species_name),]
dim(CR_iucn2) #4964
length(unique(CR_iucn2$species_name)) #4964

#Select only species with seeds (Angiosperms and Gymnosperms)
CR_seed_iucn <- CR_iucn2[CR_iucn2$group=="Angiosperms" | CR_iucn2$group=="Gymnosperms",]
dim(CR_seed_iucn) #4843
length(unique(CR_seed_iucn$species_name)) #4843

# # # write.csv(CR_seed_iucn[,1], "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/Seed storage predictor tool/CR_seed_iucn.csv", row.names = F)


# Names after checking in POWO
iucn_CR_powo_names <- iucn_CR_powo_names %>% mutate_all(na_if,'') #convert white space in NA

CR_powo <- data.frame(scientific_name=  iucn_CR_powo_names$new_names,
                      group= iucn_CR_powo_names$groups,
                      internalTaxonId= iucn_CR_powo_names$internalTaxonId_IUCN)
dim(CR_powo)

#Not subsp. or var. level: Only species level (genus + ephitet)
CR_powo$scientific_name<- as.character(CR_powo$scientific_name)
CR_powo$species_name<- word(CR_powo$scientific_name, 1,2)

dim(CR_powo)
length(unique(CR_powo$species_name))
# Remove species names duplicates
CR_powo2<- CR_powo[!duplicated(CR_powo$species_name),]
dim(CR_powo2)
length(unique(CR_powo2$species_name)) #4948

#Select only species with seeds (Angiosperms and Gymnosperms)
CR_seed_powo<- CR_powo2[CR_powo2$group=="Angiosperms" | CR_powo2$group=="Gymnosperms",]
dim(CR_seed_powo)
length(unique(CR_seed_powo$species_name)) #4827 species

# write.csv(CR_seed_powo$species_name, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/Seed storage predictor tool/CR_seed_powo.csv", row.names = F)

### Difference in names between IUCN and POWO

new_names_powo<- anti_join(iucn_CR_powo_names, iucn_taxo_sep, by=c("new_names"="scientificName"))
dim(new_names_powo)
colnames(new_names_powo)

# write_xlsx(new_names_powo[,c(33,17,2)], "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Databases/POWO/new_names_powo.xlsx")

######## Seed storage predictor tool Results----------------

seed.pred_powo <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/Seed storage predictor tool/Model-results-2021-09-21_CR_seed_powo.csv", sep=";")
head(seed.pred_powo)
seed.pred_iucn <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/Seed storage predictor tool/Model-results-2021-09-20_CR_seed_iucn.csv", sep=";")
head(seed.pred_iucn)

length(seed.pred_powo$Accepted_name) # 4727
length(seed.pred_iucn$Accepted_name) #4797

#Join seed predictor tool info with taxonomic ID given by IUCN (unique ID for each species)
CR_seed_iucn2<- CR_seed_iucn
CR_seed_iucn2$species_name<- word(CR_seed_iucn2$species_name,1,2)
seed.pred_iucn_ID<- merge(seed.pred_iucn, CR_seed_iucn2, by.x="Accepted_name", by.y="species_name", all.x=T, all.y=T)
seed.pred_iucn_ID<- seed.pred_iucn_ID[!duplicated(seed.pred_iucn_ID),] #not sure why but some duplicated were generated. Remove them
seed.pred_powo_ID<- merge(seed.pred_powo, CR_seed_powo, by.x="Accepted_name", by.y="species_name", all.x=T, all.y=T)

#Merge iucn and powo names results by the taxonomic ID, with the names database
seed.pred_iucn_CR_names <- merge(iucn_CR_powo_names, seed.pred_iucn_ID[,c(1,5:8,10)], by.x="internalTaxonId_IUCN",by.y="internalTaxonId", all.x=T, all.y=T)
seed.pred_overall_CR_names<- merge(seed.pred_iucn_CR_names, seed.pred_powo_ID[,c(1,5:8,10)], by.x="internalTaxonId_IUCN",by.y="internalTaxonId", all.x=T, all.y=T)

#replace the x in columns name with iucn, and y with powo
# colnames(seed.pred_overall_CR_names)[34:43]<- gsub('.{2}$', '', colnames(seed.pred_overall_CR_names)[34:43])
colnames(seed.pred_overall_CR_names)[34:41]<- gsub('.{2}$', '', colnames(seed.pred_overall_CR_names)[34:41]) #KD
colnames(seed.pred_overall_CR_names)[34:38]<- paste(colnames(seed.pred_overall_CR_names)[34:38], "iucn.names", sep="_")
# colnames(seed.pred_overall_CR_names)[39:43]<- paste(colnames(seed.pred_overall_CR_names)[39:43], "powo.names", sep="_")
colnames(seed.pred_overall_CR_names)[39:41]<- paste(colnames(seed.pred_overall_CR_names)[39:41], "powo.names", sep="_")

## How to deal with different values of taxonomic level, probability of recalcitrant and storage behavior, for iucn and powo names
  #If the taxonomic level differs, select the probability value with the lowest taxonomic level
  #If the taxonomic level is the same, then calculate the mean of the  probability values

#1. give taxonomic level a numeric value
seed.pred_overall_CR_names$num_tax.level_iucn.names <- ifelse(seed.pred_overall_CR_names$tax.level_iucn.names=="Species",1,
                                                              ifelse(seed.pred_overall_CR_names$tax.level_iucn.names=="Genus",2,
                                                                     ifelse(seed.pred_overall_CR_names$tax.level_iucn.names=="Family",3,
                                                                            ifelse(seed.pred_overall_CR_names$tax.level_iucn.names=="Order",4,NA))))
seed.pred_overall_CR_names$num_tax.level_iucn.names[is.na(seed.pred_overall_CR_names$num_tax.level_iucn.names)] <-999

seed.pred_overall_CR_names$num_tax.level_powo.names <-  ifelse(seed.pred_overall_CR_names$tax.level_powo.names=="Species",1,
                                                               ifelse(seed.pred_overall_CR_names$tax.level_powo.names=="Genus",2,
                                                                      ifelse(seed.pred_overall_CR_names$tax.level_powo.names=="Family",3,
                                                                             ifelse(seed.pred_overall_CR_names$tax.level_powo.names=="Order",4,NA))))
seed.pred_overall_CR_names$num_tax.level_powo.names[is.na(seed.pred_overall_CR_names$num_tax.level_powo.names)] <-999

str(seed.pred_overall_CR_names)

#2. unique value for probability of recalcitrance and taxonomic level
seed.pred_overall_CR_names$probability.of.recalcitrance <- ifelse(seed.pred_overall_CR_names$num_tax.level_iucn.names==seed.pred_overall_CR_names$num_tax.level_powo.names &
                                                                  !is.na(seed.pred_overall_CR_names$probability.of.recalcitrance_iucn.names) & !is.na(seed.pred_overall_CR_names$probability.of.recalcitrance_powo.names),
                                                                  as.numeric((seed.pred_overall_CR_names$probability.of.recalcitrance_iucn.names + seed.pred_overall_CR_names$probability.of.recalcitrance_powo.names) /2),
                                                                  ifelse(seed.pred_overall_CR_names$num_tax.level_iucn.names==seed.pred_overall_CR_names$num_tax.level_powo.names &
                                                                           is.na(seed.pred_overall_CR_names$probability.of.recalcitrance_iucn.names) & !is.na(seed.pred_overall_CR_names$probability.of.recalcitrance_powo.names),
                                                                         as.numeric(seed.pred_overall_CR_names$probability.of.recalcitrance_powo.names),
                                                                         ifelse(seed.pred_overall_CR_names$num_tax.level_iucn.names==seed.pred_overall_CR_names$num_tax.level_powo.names &
                                                                                  !is.na(seed.pred_overall_CR_names$probability.of.recalcitrance_iucn.names) & is.na(seed.pred_overall_CR_names$probability.of.recalcitrance_powo.names),
                                                                                as.numeric(seed.pred_overall_CR_names$probability.of.recalcitrance_iucn.names),
                                                                                ifelse(seed.pred_overall_CR_names$num_tax.level_iucn.names< seed.pred_overall_CR_names$num_tax.level_powo.names,
                                                                                       as.numeric(seed.pred_overall_CR_names$probability.of.recalcitrance_iucn.names),
                                                                                       ifelse(seed.pred_overall_CR_names$num_tax.level_powo.names< seed.pred_overall_CR_names$num_tax.level_iucn.names,
                                                                                              as.numeric(seed.pred_overall_CR_names$probability.of.recalcitrance_powo.names),NA)))))



seed.pred_overall_CR_names$tax.level <- ifelse(seed.pred_overall_CR_names$num_tax.level_iucn.names==seed.pred_overall_CR_names$num_tax.level_powo.names,
                                               as.character(seed.pred_overall_CR_names$tax.level_iucn.names),
                                               ifelse(seed.pred_overall_CR_names$num_tax.level_iucn.names< seed.pred_overall_CR_names$num_tax.level_powo.names,
                                                      as.character(seed.pred_overall_CR_names$tax.level_iucn.names),
                                                      ifelse(seed.pred_overall_CR_names$num_tax.level_powo.names< seed.pred_overall_CR_names$num_tax.level_iucn.names,
                                                             as.character(seed.pred_overall_CR_names$tax.level_powo.names),NA)))

seed.pred_overall_CR_names$storBehav<- ifelse(seed.pred_overall_CR_names$probability.of.recalcitrance>0.5,"Likely recalcitrant","Likely ortodox")
seed.pred_overall_CR_names$storBehav[is.na(seed.pred_overall_CR_names$storBehav)] <- "InsufficientInfo"

#Calculate uncertainty
seed.pred_overall_CR_names$uncertainty<- ifelse(seed.pred_overall_CR_names$storBehav=="Likely ortodox", seed.pred_overall_CR_names$probability.of.recalcitrance-0, 1-seed.pred_overall_CR_names$probability.of.recalcitrance)
hist(seed.pred_overall_CR_names$uncertainty)

boxplot(uncertainty~tax.level, data=seed.pred_overall_CR_names)

boxplot(uncertainty~storBehav, data=seed.pred_overall_CR_names, col=c("lightblue", "darkorange"))
par(mfrow=c(1,4))
boxplot(uncertainty~storBehav, data=seed.pred_overall_CR_names[seed.pred_overall_CR_names$tax.level=="Species",], col=c("lightblue", "darkorange"))
boxplot(uncertainty~storBehav, data=seed.pred_overall_CR_names[seed.pred_overall_CR_names$tax.level=="Genus",], col=c("lightblue", "darkorange"))
boxplot(uncertainty~storBehav, data=seed.pred_overall_CR_names[seed.pred_overall_CR_names$tax.level=="Family",], col=c("lightblue", "darkorange"))
boxplot(uncertainty~storBehav, data=seed.pred_overall_CR_names[seed.pred_overall_CR_names$tax.level=="Order",], col=c("lightblue", "darkorange"))
par(mfrow=c(1,1))
boxplot(uncertainty~familyName_IUCN, data=seed.pred_overall_CR_names[seed.pred_overall_CR_names$uncertainty>0.2,], col=c("lightblue", "darkorange"))


#Some graphs ------

seed_plants<- seed.pred_overall_CR_names[seed.pred_overall_CR_names$groups=="Angiosperms" | seed.pred_overall_CR_names$groups=="Gymnosperms",]

table(seed_plants$storBehav) *100 / length(unique(seed_plants$internalTaxonId_IUCN))
table(seed_plants$tax.level) *100 / length(unique(seed_plants$internalTaxonId_IUCN))

mytable_iucn<- table(seed_plants$storBehav)
lbls<- paste0(names(mytable_iucn)," - ",mytable_iucn," sp")
pie(mytable_iucn, labels=lbls, main="Storage behaviour", col=c("white", "lightblue", "darkorange"))

mytable.sp<- table(seed_plants[seed_plants$tax.level=="Species",]$storBehav)
lbls.sp<- paste0(mytable.sp," sp")
pie(mytable.sp, labels=lbls.sp, main="Species level", col=c("lightblue", "darkorange"))

mytable.ge<- table(seed_plants[seed_plants$tax.level=="Genus",]$storBehav)
lbls.ge<- paste0(mytable.ge, " sp")
pie(mytable.ge, labels=lbls.ge, main="Genus level", col=c("lightblue", "darkorange"))

mytable.fa<- table(seed_plants[seed_plants$tax.level=="Family",]$storBehav)
lbls.fa<- paste0(mytable.fa, " sp")
pie(mytable.fa, labels=lbls.fa, main="Family level", col=c("lightblue", "darkorange"))

mytable.or<- table(seed_plants[seed_plants$tax.level=="Order",]$storBehav)
lbls.or<- paste0(mytable.or, sep=" sp")
pie(mytable.or, labels=lbls.or, main="Order level", col=c("white", "lightblue", "darkorange"))

plot.new()
legend("bottom", legend=c("Likely orthodox", "Likely recalcitrant", "InsufficientInfo"), fill=c("lightblue", "darkorange", "white"))

par(mfrow=c(1,4))
boxplot(probability.of.recalcitrance~storBehav, data=seed_plants[seed_plants$tax.level=="Species",], col=c("lightblue", "darkorange"))
boxplot(probability.of.recalcitrance~storBehav, data=seed_plants[seed_plants$tax.level=="Genus",], col=c("lightblue", "darkorange"))
boxplot(probability.of.recalcitrance~storBehav, data=seed_plants[seed_plants$tax.level=="Family",], col=c("lightblue", "darkorange"))
boxplot(probability.of.recalcitrance~storBehav, data=seed_plants[seed_plants$tax.level=="Order",], col=c("lightblue", "darkorange"))

######## Biological traits from seed predictor tool-------
seed.pred_overall_CR_names$desiccation<- ifelse(seed.pred_overall_CR_names$probability.of.recalcitrance<=0.25 &
                                                            seed.pred_overall_CR_names$tax.level=="Species", "likely tolerant",
                                                          ifelse(seed.pred_overall_CR_names$probability.of.recalcitrance<=0.25 &
                                                                   seed.pred_overall_CR_names$tax.level=="Genus", "likely tolerant",
                                                                 ifelse(seed.pred_overall_CR_names$probability.of.recalcitrance<=0.25 &
                                                                          seed.pred_overall_CR_names$tax.level=="Family", "likely tolerant",
                                                                        ifelse(seed.pred_overall_CR_names$probability.of.recalcitrance<=0.25 &
                                                                                 seed.pred_overall_CR_names$tax.level=="Order", "likely tolerant",
                                                                               ifelse(seed.pred_overall_CR_names$probability.of.recalcitrance>= 0.75 &
                                                                                        seed.pred_overall_CR_names$tax.level=="Species", "likely sensitive",
                                                                                      ifelse(seed.pred_overall_CR_names$probability.of.recalcitrance>= 0.85 &
                                                                                               seed.pred_overall_CR_names$tax.level=="Genus",  "likely sensitive",
                                                                                             ifelse(seed.pred_overall_CR_names$probability.of.recalcitrance>= 0.95 &
                                                                                                      seed.pred_overall_CR_names$tax.level=="Family","likely sensitive", NA)))))))


seed.pred_overall_CR_names$desiccation[is.na(seed.pred_overall_CR_names$desiccation)] <- "Insufficient info"

# write_xlsx(seed.pred_overall_CR_names, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/Seed storage predictor tool/seed.pred_overall_CR_names.xlsx")

### some graphs ----

seed_plants2<- seed.pred_overall_CR_names[seed.pred_overall_CR_names$groups=="Angiosperms" | seed.pred_overall_CR_names$groups=="Gymnosperms",]

table(seed_plants2$tax.level) *100 / length(unique(seed_plants2$internalTaxonId_IUCN))
table(seed_plants2$desiccation) *100 / length(unique(seed_plants2$internalTaxonId_IUCN))

mytable<- table(seed_plants2$desiccation)
lbls<- paste0(mytable, sep=" sp")
pie(mytable, labels=lbls, main="", col=c("white", "darkorange3","blue"))

mytable.sp<- table(seed_plants2[seed_plants2$tax.level=="Species",]$desiccation)
lbls.sp<- paste0(mytable.sp," sp")
pie(mytable.sp, labels=lbls.sp, main="", col=c( "darkorange3", "blue"))

mytable.ge<- table(seed_plants2[seed_plants2$tax.level=="Genus",]$desiccation)
lbls.ge<- paste0(mytable.ge, " sp")
pie(mytable.ge, labels=lbls.ge, main="Genus level", col=c("white", "darkorange3", "blue"))

mytable.fa<- table(seed_plants2[seed_plants2$tax.level=="Family",]$desiccation)
lbls.fa<- paste0(mytable.fa, " sp")
pie(mytable.fa, labels=lbls.fa, main="Family level",col=c("white", "darkorange3", "blue"))

mytable.or<- table(seed_plants2[seed_plants2$tax.level=="Order",]$desiccation)
lbls.or<- paste0(mytable.or, sep=" sp")
pie(mytable.or, labels=lbls.or, main="Order level", col=c("white", "blue"))

plot.new()
legend("bottom", legend=c("Likely desiccation tolerant", "Likely desiccation sensitive", "InsufficientInfo"), fill=c("blue", "darkorange3", "white"))

######## Biological traits from Hawaiian database -------

hawai<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/Hawai plants database/Hawai Seed Bank_full version.csv", sep=";")
hawai$Species #does not consider subsp. or var. level
length(unique(hawai$Species)) #1062

hawai[duplicated(hawai$Species, fromLast=T) | duplicated(hawai$Species),]

#Some species are duplicated if the hawaian database. Deal with them:
  # Select Tested research status (between Tested and Inferred)
  # For Myoporum spp. select Scrophulariaceae family (no Myoporaceae), because is the one that iucn and powo consider
dup.hawai <- hawai[duplicated(hawai$Species, fromLast=T) & hawai$Research.Status=="Tested" & hawai$Family!="Myoporaceae"
      | duplicated(hawai$Species) & hawai$Research.Status=="Tested" & hawai$Family!="Myoporaceae" ,] #select all raw that are duplicated (not only second row)
length(dup.hawai$Species) #6
length(unique(dup.hawai$Species)) #6

nondup.hawai<- anti_join(hawai, dup.hawai, by="Species")
length(unique(nondup.hawai$Species)) #1056
length(nondup.hawai$Species) #1056

hawai2<- rbind(nondup.hawai, dup.hawai)
length(hawai2$Species) #1062

#For new names, create one version without sp. or var. level
seed.pred_overall_CR_names2<- seed.pred_overall_CR_names
seed.pred_overall_CR_names2$new_names_short<- word(seed.pred_overall_CR_names2$new_names,1,2)

#Merge the newly created "seed.pred_overall_CR_names2" database with hawaiian database, both for IUCN and POWO names.
#We will use this to then merge the "seed.pred_overall_CR_names" the corresponding only one name

seed.pred_overall_CR_names3<- merge(seed.pred_overall_CR_names2[,c(1:3,31,51)], hawai2[,c(2,3)], by.x="species_name_IUCN", by.y="Species", all.x=T)
seed.pred_overall_CR_names4<- merge(seed.pred_overall_CR_names3, hawai2[,c(2,3)], by.x="new_names_short", by.y="Species", all.x=T)

#If IUCN and POWO names are the same, then match by POWO name (does not matter if it is IUCN or POWO name)
same<- merge(seed.pred_overall_CR_names4[seed.pred_overall_CR_names4$species_name_IUCN==seed.pred_overall_CR_names4$new_names_short &
                                           !is.na(seed.pred_overall_CR_names4$Research.Status.x) & !is.na(seed.pred_overall_CR_names4$Research.Status.y),],
               hawai2, by.x="new_names_short", by.y="Species")
#If IUCN and POWO names are different, but the research status (inferred vs. tested) is the same, then match by IUCN name (does not matter if it is IUCN or POWO name)
dif1<- merge(seed.pred_overall_CR_names4[seed.pred_overall_CR_names4$species_name_IUCN!=seed.pred_overall_CR_names4$new_names_short &
                                           seed.pred_overall_CR_names4$Research.Status.x==seed.pred_overall_CR_names4$Research.Status.y &
                                           !is.na(seed.pred_overall_CR_names4$Research.Status.x) & !is.na(seed.pred_overall_CR_names4$Research.Status.y),],
             hawai2, by.x="species_name_IUCN", by.y="Species")
#If IUCN and POWO names are different, and the research status is different, then match by POWO name (because those species have Tested research status)
dif2<- merge(seed.pred_overall_CR_names4[seed.pred_overall_CR_names4$species_name_IUCN!=seed.pred_overall_CR_names4$new_names_short &
                                           seed.pred_overall_CR_names4$Research.Status.x!=seed.pred_overall_CR_names4$Research.Status.y &
                                           !is.na(seed.pred_overall_CR_names4$Research.Status.x) & !is.na(seed.pred_overall_CR_names4$Research.Status.y),],
             hawai2, by.x="new_names_short", by.y="Species")
#If IUCN and POWO names are different, but only the IUCN name contains hawai info, then match by IUCN name
dif3<- merge(seed.pred_overall_CR_names4[seed.pred_overall_CR_names4$species_name_IUCN!=seed.pred_overall_CR_names4$new_names_short &
                                           !is.na(seed.pred_overall_CR_names4$Research.Status.x) & is.na(seed.pred_overall_CR_names4$Research.Status.y),],
             hawai2, by.x="species_name_IUCN", by.y="Species")
#If IUCN and POWO names are different, but only the POWO name contains hawai info, then match by POWO name
dif4<- merge(seed.pred_overall_CR_names4[seed.pred_overall_CR_names4$species_name_IUCN!=seed.pred_overall_CR_names4$new_names_short &
                                          is.na(seed.pred_overall_CR_names4$Research.Status.x) & !is.na(seed.pred_overall_CR_names4$Research.Status.y),],
             hawai2, by.x="new_names_short", by.y="Species")

#r bind all different data frames
onematch.hawai<- rbind(same, dif1, dif2, dif3, dif4)
dim(onematch.hawai) #320

# Now, join the main database with the hawaian information for the selected species
seed.pred_overall_CR_names_hawai<- merge(seed.pred_overall_CR_names, onematch.hawai[,c(3,9:21)], by="internalTaxonId_IUCN", all.x=T)
dim(seed.pred_overall_CR_names_hawai) #5026

# write_xlsx(seed.pred_overall_CR_names_hawai, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/seed.pred_overall_CR_names_hawai.xlsx")

######## Exceptional species list (Valerie Pence)-----------

excep_sp<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/Exceptional species list (Valerie Pence)/exceptional species.csv", sep=";")
head(excep_sp)
colnames(excep_sp)
dim(excep_sp) #775
length(unique(excep_sp$Species.name))

#For new names, create one version without sp. or var. level
seed.pred_overall_CR_names_hawai2<- seed.pred_overall_CR_names_hawai
seed.pred_overall_CR_names_hawai2$new_names_short<- word(seed.pred_overall_CR_names_hawai2$new_names,1,2)

## Check if some of the species are CR
#IUCN names
excep_sp_CR1<- semi_join(excep_sp, seed.pred_overall_CR_names_hawai2, by=c("Species.name"="species_name_IUCN"))
length(excep_sp_CR1$Species.name) #101

excep_sp_CR2<- semi_join(excep_sp, seed.pred_overall_CR_names_hawai2, by=c("WFO.taxon.name"="species_name_IUCN"))
length(excep_sp_CR2$WFO.taxon.name) #105

join1<- anti_join(excep_sp_CR1, excep_sp_CR2, by="Species.name")

excep_sp_CR2$species <-  excep_sp_CR2$WFO.taxon.name #They match by WFO name
join1$species <- join1$Species.name # They match by Species.name

excep_sp_CR_iucn<- rbind(join1[,c(6:13)], excep_sp_CR2[,c(6:13)])
length(excep_sp_CR_iucn$species) #109

#new names (after POWO checking)
excep_sp_CR3<- semi_join(excep_sp, seed.pred_overall_CR_names_hawai2, by=c("Species.name"="new_names_short"))
length(excep_sp_CR3$Species.name) #100

excep_sp_CR4<- semi_join(excep_sp, seed.pred_overall_CR_names_hawai2, by=c("WFO.taxon.name"="new_names_short"))
length(excep_sp_CR4$WFO.taxon.name) #106

join2<- anti_join(excep_sp_CR3, excep_sp_CR4, by="Species.name")

excep_sp_CR4$species <-  excep_sp_CR4$WFO.taxon.name #They match by WFO name
join2$species <- join2$Species.name # They match by Species.name

excep_sp_CR_powo<- rbind(join2[,c(6:13)], excep_sp_CR4[,c(6:13)])
length(excep_sp_CR_powo$species) #109
length(unique(excep_sp_CR_powo$species))#105

#Merge the newly created "seed.pred_overall_CR_names_hawai2" database with exceptional species database.
#to check for double matching of the IUCN and POWO name

check_doubles<- merge(seed.pred_overall_CR_names_hawai2[,c(1:3,64,31)], excep_sp_CR_iucn[,c(5,8)], by.x="species_name_IUCN", by.y="species", all.x=T)
check_doubles2<- merge(check_doubles, excep_sp_CR_powo[,c(5,8)], by.x="new_names_short", by.y="species", all.x=T)

#If IUCN and POWO names are the same, then merge by IUCN name
merge1<- merge(check_doubles2[check_doubles2$species_name_IUCN==check_doubles2$new_names_short,],
               excep_sp_CR_iucn, by.x="species_name_IUCN", by.y="species")
#If IUCN and POWO names are different, and exceptional status from POWO names is NA, then merge by IUCN name
merge2<- merge(check_doubles2[check_doubles2$species_name_IUCN!=check_doubles2$new_names_short &
                              !is.na(check_doubles2$Exceptional.status.justification.x) & is.na(check_doubles2$Exceptional.status.justification.y),],
               excep_sp_CR_iucn, by.x="species_name_IUCN", by.y="species")
#If IUCN and POWO names are different, and exceptional status from IUCN names is NA, then merge by POWO name
merge3<- merge(check_doubles2[check_doubles2$species_name_IUCN!=check_doubles2$new_names_short &
                                is.na(check_doubles2$Exceptional.status.justification.x) & !is.na(check_doubles2$Exceptional.status.justification.y),],
               excep_sp_CR_powo, by.x="new_names_short", by.y="species")
#If IUCN and POWO names are different, but there is information for exceptional status for IUCN and POWO name, then merge by IUCN name
merge4<- merge(check_doubles2[check_doubles2$species_name_IUCN!=check_doubles2$new_names_short &
                                !is.na(check_doubles2$Exceptional.status.justification.x) & !is.na(check_doubles2$Exceptional.status.justification.y),],
               excep_sp_CR_iucn, by.x="species_name_IUCN", by.y="species")

#r bind all different data frames
merge_all<- rbind(merge1, merge2, merge3, merge4)
dim(merge_all) #145
# check for duplicates
merge_all[duplicated(merge_all),]

# Now, join the main database with the merged information for the exceptional species
seed.pred_overall_CR_names_hawai_excepsp<- merge(seed.pred_overall_CR_names_hawai, merge_all[,c(3,8:14)], by="internalTaxonId_IUCN", all.x=T)
dim(seed.pred_overall_CR_names_hawai_excepsp) #5035
length(unique(seed.pred_overall_CR_names_hawai_excepsp$scientificName_IUCN)) #5026
#check for duplicates and remove
seed.pred_overall_CR_names_hawai_excepsp<- seed.pred_overall_CR_names_hawai_excepsp[!duplicated(seed.pred_overall_CR_names_hawai_excepsp),]
dim(seed.pred_overall_CR_names_hawai_excepsp) #5029
seed.pred_overall_CR_names_hawai_excepsp<- seed.pred_overall_CR_names_hawai_excepsp[!duplicated(seed.pred_overall_CR_names_hawai_excepsp$scientificName_IUCN),]
dim(seed.pred_overall_CR_names_hawai_excepsp) #5026

# write_xlsx(seed.pred_overall_CR_names_hawai_excepsp, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/seed.pred_overall_CR_names_hawai_excepsp.xlsx")



######## Summary of CR seed plants and spores ------------

#Upload data
seed_traits<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/CRsp_IucnPowo_SeedPredTool_Hawaii_Excep_Ref_summary.csv", sep=";")
dim(seed_traits)
# add information for spores
spores_traits<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/Ferns and Lycophytes CR_spores characteristics.csv", sep=";")
dim(spores_traits)
colnames(spores_traits)

## Add spores traits summary to main database
cr_bio_traits<- merge(seed_traits, spores_traits[,c(3,9:15)], by="scientificName_IUCN", all=T)
dim(cr_bio_traits)

##Add spores summary for desiccation and longevity
cr_bio_traits$Desiccation <- ifelse(cr_bio_traits$groups=="Ferns" |cr_bio_traits$groups=="Lycophytes", "Likely tolerant", as.character(cr_bio_traits$Desiccation))
cr_bio_traits$Longevity <- ifelse(cr_bio_traits$chlorophyllous=="chlorophyllous","Short lived",
                                 ifelse(cr_bio_traits$chlorophyllous=="Unknown", "Short lived?", as.character(cr_bio_traits$Longevity)))


cr_bio_traits[duplicated(cr_bio_traits$scientificName_IUCN),]

table(cr_bio_traits$Desiccation, cr_bio_traits$Longevity)
table(cr_bio_traits$Desiccation, cr_bio_traits$Freezing)
table(cr_bio_traits$Desiccation, cr_bio_traits$Others)

#Create variable to plot
cr_bio_traits[cr_bio_traits==""] <- NA
cr_bio_traits$problematic<- ifelse(cr_bio_traits$Desiccation=="Likely sensitive", "Likely sensitive",
                                   ifelse(cr_bio_traits$Desiccation=="Uncertain" | cr_bio_traits$Desiccation=="Unknown", "Unknown",
                                        ifelse(cr_bio_traits$Desiccation=="Likely tolerant" &
                                               is.na(cr_bio_traits$Freezing) & is.na(cr_bio_traits$Longevity) & is.na(cr_bio_traits$Others), "Likely tolerant", "Problematic biological traits")))

# write_xlsx(cr_bio_traits, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/CRsp_IucnPowo_BioTraits.xlsx")

mytable.traits<- table(cr_bio_traits$problematic)
lbls.traits<- paste0(mytable.traits," sp (", round(mytable.traits*100/dim(cr_bio_traits)[1],2)," %)")
pie(mytable.traits, labels=lbls.traits, main="", col=c( "darkorange", "darkolivegreen3", "lightblue", "white"))
plot.new()
legend("bottom", legend=names(mytable.traits), fill=c("darkorange", "darkolivegreen3", "lightblue", "white"))

########## Geographical factor ####################
###### Create database by country -----

### IUCN data by country ------

iucn_sep_country<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\CR_IUCN_Sep21/countries.csv")
head(iucn_sep_country)

length(unique(iucn_sep_country$scientificName)) #5026 unique values

#Consider the species range where the species was extinct?
table(iucn_sep_country$presence)
iucn_sep_country[iucn_sep_country$presence=="Extinct Post-1500",]
# Do not consider the extinct range. Exceot for Aloe decorsei (because Madagascar is the only range for the species)
iucn_sep_country2<- iucn_sep_country[!iucn_sep_country$presence=="Extinct Post-1500",]
iucn_sep_country2<- droplevels(iucn_sep_country2)
table(iucn_sep_country2$presence)
iucn_sep_country3<- rbind(iucn_sep_country2, iucn_sep_country[iucn_sep_country$scientificName=="Aloe decorsei",])
table(iucn_sep_country3$presence)

#Consider the species range where the species was Introduced?? YES
table(iucn_sep_country$origin)
iucn_sep_country[iucn_sep_country$origin=="Introduced",]

## Prepare data IUCN data
table(iucn_sep_country3$name)
length(unique(iucn_sep_country3$name))
length(unique(iucn_sep_country3$code))
n.sp_country<- aggregate(scientificName~ name, data=iucn_sep_country3, FUN=function(x) count=length(x))
length(n.sp_country$name)
## Add the country code
n.sp_country<- merge(n.sp_country, iucn_sep_country[,c(4,5)], by="name", all.x=T)
n.sp_country<- n.sp_country[!duplicated(n.sp_country),]
length(n.sp_country$name)
#Add Namibia country because it reads as NA
n.sp_country$code<- as.character(n.sp_country$code)
n.sp_country[n.sp_country$name=="Namibia",]$code <- as.character("NA")

str(n.sp_country)
n.sp_country$name <- as.character(n.sp_country$name)
n.sp_country$code <- as.character(n.sp_country$code)

colnames(n.sp_country)<- c("country_name","CR_species_IUCN", "iso_country")


### Add the number of collections at the MSB (Liu et al. 2018) -----
msb_collec <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Geographical traits/Udi_Data/MSB collections.csv", sep=";")
head(msb_collec)

#aggregate the number of species by country
msb_collec_country<- aggregate(taxa ~ Country, data= msb_collec, FUN=function(x) count=length(x))
msb_collec_country$Country <- as.character(msb_collec_country$Country)

#check if country names match between databases
anti_join(n.sp_country, msb_collec_country, by=c("country_name"="Country"))

#correct the mismatches
n.sp_country[n.sp_country$country_name=="Bolivia, Plurinational States of",1] <- "Bolivia"
n.sp_country[n.sp_country$country_name=="Bosnia and Herzegovina",1] <- "Bosnia & Herzegovina"
n.sp_country[n.sp_country$country_name=="Brunei Darussalam",1] <- "Brunei"
n.sp_country[n.sp_country$country_name=="Côte d'Ivoire",1] <- "Ivory Coast"
n.sp_country[n.sp_country$country_name=="Cabo Verde",1] <- "Cape Verde"
n.sp_country[n.sp_country$country_name=="Cayman Islands",1]<- "Cayman Is."
n.sp_country[n.sp_country$country_name=="Congo",1] <- "Republic of Congo"
n.sp_country[n.sp_country$country_name=="Congo, The Democratic Republic of the",1] <- "Congo, DRC"
n.sp_country[n.sp_country$country_name=="Cook Islands",1] <- "Cook Is."
n.sp_country[n.sp_country$country_name=="Czechia",1] <- "Czech Republic"
n.sp_country[n.sp_country$country_name=="Eswatini",1] <- "Swaziland"
n.sp_country[n.sp_country$country_name=="Iran, Islamic Republic of",1]<- "Iran"
n.sp_country[n.sp_country$country_name=="Korea, Democratic People's Republic of",1]<- "North Korea"
n.sp_country[n.sp_country$country_name=="Korea, Republic of",1]<- "South Korea"
n.sp_country[n.sp_country$country_name=="Lao People's Democratic Republic",1]<- "Laos"
n.sp_country[n.sp_country$country_name=="Palestine, State of",1]<- "Palestinian Territory, Occupied"
n.sp_country[n.sp_country$country_name=="Réunion",1]<- "Reunion"
n.sp_country[n.sp_country$country_name=="Saint Helena, Ascension and Tristan da Cunha",1]<- "St. Helena, Ascension & Tristan da Cunha"
n.sp_country[n.sp_country$country_name=="Saint Lucia",1]<- "St. Lucia"
n.sp_country[n.sp_country$country_name=="Sao Tome and Principe",1]<- "Sao Tome & Principe"
n.sp_country[n.sp_country$country_name=="Solomon Islands",1]<- "Solomon Is."
n.sp_country[n.sp_country$country_name=="Syrian Arab Republic",1]<- "Syria"
n.sp_country[n.sp_country$country_name=="Taiwan, Province of China",1]<- "Taiwan"
n.sp_country[n.sp_country$country_name=="Tanzania, United Republic of",1]<- "Tanzania"
n.sp_country[n.sp_country$country_name=="Timor-Leste",1]<- "East Timor"
n.sp_country[n.sp_country$country_name=="Trinidad and Tobago",1]<- "Trinidad & Tobago"
n.sp_country[n.sp_country$country_name=="Turks and Caicos Islands",1]<- "Turks & Caicos Is."
n.sp_country[n.sp_country$country_name=="United Kingdom",1]<- "UK"
n.sp_country[n.sp_country$country_name=="United States",1]<- "USA"
n.sp_country[n.sp_country$country_name=="Venezuela, Bolivarian Republic of",1]<- "Venezuela"
n.sp_country[n.sp_country$country_name=="Viet Nam",1]<- "Vietnam"
n.sp_country[n.sp_country$country_name=="Virgin Islands, British",1]<- "British Virgin Is."
n.sp_country[n.sp_country$country_name=="Virgin Islands, U.S.",1]<- "U.S. Virgin Is."

anti_join(n.sp_country, msb_collec_country, by=c("country_name"="Country"))

#join database
country_data1<- merge(n.sp_country, msb_collec_country, by.x="country_name", by.y="Country", all=T)
colnames(country_data1)[4]<- "collec_MSB"


### Add ABS clearing house data ------

abs <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Geographical traits/ABS clearing house/ABSCH-Country-List.csv", sep=";")

# Create a matrix of 0 and 1 for the different criteria
str(abs)
abs$st.nagoya<- ifelse(abs$Status=="Party", as.numeric(1),as.numeric(0))
abs$st.nfp <- ifelse(abs$NFP>0,as.numeric(1),as.numeric(0))
abs$st.cna <- ifelse(abs$CNA>0,as.numeric(1),as.numeric(0))
abs$st.ircc <- ifelse(abs$IRCC>0,as.numeric(1),as.numeric(0))

##Create ABS index
#Sum the 0-1 value of the four criteria
abs$index.abs<- rowSums(abs[,c("st.nagoya","st.nfp","st.cna","st.ircc")])

#check if country names match between databases
abs$Country<- as.character(abs$Country)
anti_join(country_data1, abs, by=c("country_name"="Country"))

#correct the mismatches
abs[abs$Country=="Bolivia (Plurinational State of)",1] <- "Bolivia"
abs[abs$Country=="Bosnia and Herzegovina",1] <- "Bosnia & Herzegovina"
abs[abs$Country=="Brunei Darussalam",1] <- "Brunei"
abs[abs$Country=="Cabo Verde",1] <- "Cape Verde"
abs[abs$Country=="Democratic Republic of the Congo",1] <- "Congo, DRC"
abs[abs$Country=="Cook Islands",1] <- "Cook Is."
abs[abs$Country=="Timor-Leste",1] <- "East Timor"
abs[abs$Country=="Gambia (the)",1] <- "Gambia"
abs[abs$Country=="Iran (Islamic Republic of)",1] <- "Iran"
abs[abs$Country=="C?te d'Ivoire",1] <- "Ivory Coast"
abs[abs$Country=="Lao People's Democratic Republic",1] <- "Laos"
abs[abs$Country=="North Macedonia",1] <- "Macedonia"
abs[abs$Country=="Micronesia (Federated States of)",1] <- "Micronesia, Federated States of "
abs[abs$Country=="Democratic People's Republic of Korea",1] <- "North Korea"
abs[abs$Country=="State of Palestine",1] <- "Palestinian Territory, Occupied"
abs[abs$Country=="Congo",1] <- "Republic of Congo"
abs[abs$Country=="Saint Lucia",1] <- "St. Lucia"
abs[abs$Country=="Sao Tome and Principe",1] <- "Sao Tome & Principe"
abs[abs$Country=="Solomon Islands",1] <- "Solomon Is."
abs[abs$Country=="Republic of Korea",1] <- "South Korea"
abs[abs$Country=="Syrian Arab Republic",1] <- "Syria"
abs[abs$Country=="Eswatini",1] <- "Swaziland"
abs[abs$Country=="United Republic of Tanzania",1] <- "Tanzania"
abs[abs$Country=="Trinidad and Tobago",1] <- "Trinidad & Tobago"
abs[abs$Country=="United Kingdom of Great Britain and Northern Ireland",1] <- "UK"
abs[abs$Country=="United States of America",1] <- "USA"
abs[abs$Country=="Venezuela (Bolivarian Republic of)",1] <- "Venezuela"
abs[abs$Country=="Viet Nam",1] <- "Vietnam"

anti_join(country_data1, abs, by=c("country_name"="Country"))

#join databases
country_data2<- merge(country_data1, abs[,c(1:4,8,13:17)], by.x="country_name", by.y="Country", all=T)


# For the overseas territories/islands, add the main country ABS information

#UK territories
country_data2[country_data2$country_name=="Anguilla",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Bermuda",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="British Virgin Is.",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Cayman Is.",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Falkland Is.",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Gibraltar",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Guernsey",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Isle of Man",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Jersey",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Montserrat",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Pitcairn",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="South Georgia & the South Sandwich Is.",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="St. Helena, Ascension & Tristan da Cunha",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]
country_data2[country_data2$country_name=="Turks & Caicos Is.",c(5:13)] <- country_data2[country_data2$country_name=="UK",c(5:13)]


#Australian territories
country_data2[country_data2$country_name=="Christmas I.",c(5:13)] <- country_data2[country_data2$country_name=="Australia",c(5:13)]
country_data2[country_data2$country_name=="Norfolk Island",c(5:13)] <- country_data2[country_data2$country_name=="Australia",c(5:13)]

#French territories
country_data2[country_data2$country_name=="French Guiana",c(5:13)] <- country_data2[country_data2$country_name=="France",c(5:13)]
country_data2[country_data2$country_name=="French Polynesia",c(5:13)] <- country_data2[country_data2$country_name=="France",c(5:13)]
country_data2[country_data2$country_name=="French Southern Territories",c(5:13)] <- country_data2[country_data2$country_name=="France",c(5:13)]
country_data2[country_data2$country_name=="Guadeloupe",c(5:13)] <- country_data2[country_data2$country_name=="France",c(5:13)]
country_data2[country_data2$country_name=="Martinique",c(5:13)] <- country_data2[country_data2$country_name=="France",c(5:13)]
country_data2[country_data2$country_name=="New Caledonia",c(5:13)] <- country_data2[country_data2$country_name=="France",c(5:13)]
country_data2[country_data2$country_name=="Reunion",c(5:13)] <- country_data2[country_data2$country_name=="France",c(5:13)]

#USA territories
country_data2[country_data2$country_name=="Guam",c(5:13)] <- country_data2[country_data2$country_name=="USA",c(5:13)]
country_data2[country_data2$country_name=="Northern Mariana Islands",c(5:13)] <- country_data2[country_data2$country_name=="USA",c(5:13)]
country_data2[country_data2$country_name=="Puerto Rico",c(5:13)] <- country_data2[country_data2$country_name=="USA",c(5:13)]
country_data2[country_data2$country_name=="U.S. Virgin Is.",c(5:13)] <- country_data2[country_data2$country_name=="USA",c(5:13)]

#Chinese territories
country_data2[country_data2$country_name=="Hong Kong",c(5:13)] <- country_data2[country_data2$country_name=="China",c(5:13)]
country_data2[country_data2$country_name=="Taiwan",c(5:13)] <- country_data2[country_data2$country_name=="China",c(5:13)]

#Netherlands territories
country_data2[country_data2$country_name=="Sint Maarten (Dutch Part)",c(5:13)] <- country_data2[country_data2$country_name=="Netherlands",c(5:13)]


#The Europeand Union has an ABSCH index of 2. Countries from EU with lower than two, upgrade
country_data2[country_data2$country_name=="Cyprus",c(5:13)] <- country_data2[country_data2$country_name=="European Union",c(5:13)]
country_data2[country_data2$country_name=="Ireland",c(5:13)] <- country_data2[country_data2$country_name=="European Union",c(5:13)]
country_data2[country_data2$country_name=="Italy",c(5:13)] <- country_data2[country_data2$country_name=="European Union",c(5:13)]
country_data2[country_data2$country_name=="Latvia",c(5:13)] <- country_data2[country_data2$country_name=="European Union",c(5:13)]
country_data2[country_data2$country_name=="Lithuania",c(5:13)] <- country_data2[country_data2$country_name=="European Union",c(5:13)]
country_data2[country_data2$country_name=="Slovenia",c(5:13)] <- country_data2[country_data2$country_name=="European Union",c(5:13)]


# write_xlsx(country_data2, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Geographical traits/country_CR_MSBcole_ABS.xlsx")


### Map IUCN CR species by country ---------

## Create ordinal variables for the number of CR species
hist(country_data2$CR_species_IUCN, breaks=50)
country_data2$category_iucn_cr <- ifelse(country_data2$CR_species_IUCN>=1 & country_data2$CR_species_IUCN<=5, 1,
                                        ifelse(country_data2$CR_species_IUCN>=6 & country_data2$CR_species_IUCN<=10, 2,
                                               ifelse(country_data2$CR_species_IUCN>=11 & country_data2$CR_species_IUCN<=20, 3,
                                                      ifelse(country_data2$CR_species_IUCN>=21 & country_data2$CR_species_IUCN<=50, 4,
                                                             ifelse(country_data2$CR_species_IUCN>=51 & country_data2$CR_species_IUCN<=100, 5,
                                                                    ifelse(country_data2$CR_species_IUCN>=101 & country_data2$CR_species_IUCN<=200, 6,
                                                                           ifelse(country_data2$CR_species_IUCN>=201 & country_data2$CR_species_IUCN<=500, 7,
                                                                                  ifelse(country_data2$CR_species_IUCN>=501,8,NA))))))))

#world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
head(world)
str(world)

## Join the world database with the country database (country_data2)
#first check for mismatches between databases
anti_join(country_data2, world, by = c('country_name' = 'sovereignt'))
anti_join(country_data2, world, by = c('iso_country' = 'iso_a2')) #use the iso_a2 to match


#join databases
data.map<-left_join(world, country_data2, by = c('iso_a2' = 'iso_country'))
class(data.map)

## Plot maps

#categorical scale
ggplot(data = data.map) +
  geom_sf(aes(fill= as.factor(category_iucn_cr))) +
  scale_fill_viridis_d(option="plasma", name="CR species",
                       labels=c("1-5","6-10","11-20","21-50","51-100", "101-200","201-500", ">500"))+
  theme_classic()


### Map ABS clearing house by country ------
#data to plot
data_plot<- country_data2

#change countries name so that they match the world database (database used for plotting)
anti_join(data_plot, world, by = c('country_name' = 'sovereignt'))

data_plot[data_plot$country_name=="Bosnia & Herzegovina",1] <- "Bosnia and Herzegovina"
data_plot[data_plot$country_name=="Congo, DRC",1] <- "Democratic Republic of the Congo"
data_plot[data_plot$country_name=="UK",1] <- "United Kingdom"
data_plot[data_plot$country_name=="USA",1] <-  "United States of America"
data_plot[data_plot$country_name=="Tanzania",1] <-  "United Republic of Tanzania"
data_plot[data_plot$country_name=="Serbia",1] <-  "Republic of Serbia"
data_plot[data_plot$country_name=="Russian Federation",1] <-  "Russia"
data_plot[data_plot$country_name=="Trinidad & Tobago",1] <-  "Trinidad and Tobago"
data_plot[data_plot$country_name=="Sao Tome & Principe",1] <-  "Sao Tome and Principe"
data_plot[data_plot$country_name=="Republic of Moldova",1] <-  "Moldova"

#join databases
data.map<-left_join(world, data_plot, by = c('sovereignt' = 'country_name'))
class(data.map)

#categorical scale
ggplot(data = data.map) +
  geom_sf(aes(fill= as.factor(index.abs)))+
  theme_classic()+
  scale_fill_discrete(name= "ABSCH index")


### Map MSB collections by country (Udi data) ---------

## Create ordinal variables for the number of MSB collections (same as Liu et al., 2017)

hist(data_plot$collec_MSB, breaks=50)
data_plot$category_msb_collec <- ifelse(data_plot$collec_MSB>=1 & data_plot$collec_MSB<=5, 1,
                                                 ifelse(data_plot$collec_MSB>=6 & data_plot$collec_MSB<= 10, 2,
                                                        ifelse(data_plot$collec_MSB>=11 & data_plot$collec_MSB<=20, 3,
                                                               ifelse(data_plot$collec_MSB>=21 & data_plot$collec_MSB<=50, 4,
                                                                      ifelse(data_plot$collec_MSB>=51 & data_plot$collec_MSB<=100, 5,
                                                                             ifelse(data_plot$collec_MSB>=101 & data_plot$collec_MSB<=200, 6,
                                                                                    ifelse(data_plot$collec_MSB>=201 & data_plot$collec_MSB<=500,7,
                                                                                           ifelse(data_plot$collec_MSB>=501 & data_plot$collec_MSB<=1000,8,
                                                                                                  ifelse(data_plot$collec_MSB>=1001,9,NA)))))))))


## Join the world database with the country database (data_plot)
#first check for mismatches between databases
anti_join(data_plot, world, by = c('country_name' = 'sovereignt'))

#join databases
data.map<-left_join(world, data_plot, by = c('sovereignt' = 'country_name'))
class(data.map)

ggplot(data = data.map) +
  geom_sf(aes(fill= as.factor(category_msb_collec))) +
  scale_fill_viridis_d(option="plasma", name="MSB collections",
                       labels=c("1-5","6-10","11-20","21-50","51-100", "101-200","201-500", "501-1000",">1000"))+
  theme_classic()
