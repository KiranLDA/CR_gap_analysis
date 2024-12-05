##### libraries --------

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

######## Explore Data Warehouse ------
## DW data: 9, 11, 12, 13 Aug -------------
#seed extract for species 9 aug
dw_se1 <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/SEEDEXTRACT_09-08-2021_at_09-42-26.CSV")
#seed extract for species 11 aug
dw_se2 <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/SEEDEXTRACT_11-08-2021_at_21-16-30.CSV")
# seed extract for 3 extra species
dw_se3 <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/SEEDEXTRACT_12-08-2021_at_14-11-06.CSV")
#seed extract for subspecies
dw_se4 <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/SEEDEXTRACT_12-08-2021_at_15-07-34.CSV")
#seed extract for species 13 aug
dw_se5 <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/SEEDEXTRACT_13-08-2021_at_21-14-20.CSV")

# Join all databases (only non repeated accessions): join all of them to dw_se2
identical(colnames(dw_se1), colnames(dw_se2))
identical(colnames(dw_se2), colnames(dw_se3))
identical(colnames(dw_se3), colnames(dw_se4))
identical(colnames(dw_se4), colnames(dw_se5))

dw_se3$ACCESSION <- as.factor(dw_se3$ACCESSION)
save1 <- anti_join(dw_se3, dw_se2, by="ACCESSION") #accessions missing from dw_se3
save2 <- anti_join(dw_se4, dw_se2, by="ACCESSION") #accessions missing from dw_se4

dw_se <- rbind(save1,save2,dw_se2)
dw_se[duplicated(dw_se$ACCESSION),] #Duplicated records are the ones with no accession number

save3 <- anti_join(dw_se1, dw_se, by="ACCESSION")
dw_seed_old <- rbind(save3,dw_se)
dw_seed_old[duplicated(dw_seed_old$ACCESSION),] #Duplicated records are the ones with no accession number

anti_join(dw_se5, dw_seed_old, by="ACCESSION") #All accessions present

#Delete author names from species name
dw_seed_old$species2 <- word(dw_seed_old$SPECIES, 1,2)
#Add subsp. and var. to teh species name
dw_seed_old[is.na(dw_seed_old)] <- ""
dw_seed_old$species3 <- paste(dw_seed_old$species2, dw_seed_old$RANK1, dw_seed_old$SP2)
dw_seed_old$species3 <- trimws(dw_seed_old$species3)
length(unique(dw_seed_old$species3)) #349


## DW data: New data after checking POWO names and new CR ------

dw_seed_new<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/NewCR_NewPowoName_SEEDEXTRACT_20-09-2021_at_21-32-17.CSV")
head(dw_seed_new)
length(unique(dw_seed_new$SPECIES))

semi_join(dw_seed_new, dw_seed_old, by="SPECIES") #all new species are NEW

#Delete author names from species name
dw_seed_new$species2 <- word(dw_seed_new$SPECIES, 1,2)
#Add subsp. and var. to teh species name
dw_seed_new[is.na(dw_seed_new)] <- ""
dw_seed_new$species3 <- paste(dw_seed_new$species2, dw_seed_new$RANK1, dw_seed_new$SP2)
dw_seed_new$species3 <- trimws(dw_seed_new$species3)
length(unique(dw_seed_new$species3)) #92

# Exclude the POWO names that were considered synonyms (include only homotypic synonym and no synonym)
iucn_CR_powo_names<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/POWO/iucn_CR_powo_names_check synonyms.csv", sep=";")

dw_seed_new_syninfo<- merge(dw_seed_new, iucn_CR_powo_names[,c(31,33)], by.x="species3", by.y="new_names", all.x=T)
dw_seed_new_syninfo<- dw_seed_new_syninfo[!duplicated(dw_seed_new_syninfo),]
length(unique(dw_seed_new_syninfo$species3)) #92
dw_seed_new_nosym<- dw_seed_new_syninfo[dw_seed_new_syninfo$synonym!="synonym",]
length(unique(dw_seed_new_nosym$species3)) #55

# Join all seed extracts together ------
dw_seed <- rbind(dw_seed_old, dw_seed_new_nosym[,-c(218)])
length(unique(dw_seed$species3)) #404

## Compare SEED EXTRACT data with IUCN  ----

check_iucn <- anti_join(dw_seed, iucn_CR_powo_names, by= c("species3" = "new_names"))
unique(check_iucn$species3)
check_iucn2 <- anti_join(check_iucn, iucn_CR_powo_names, by= c("species3" = "scientificName_IUCN"))
unique(check_iucn2$species3)

##These subsp. are assessed as CR at the species level: Species missing at dw_seed
# "Bupleurum dianthifolium subsp. barceloi"
# "Claoxylon linostachys subsp. brachyphyllum"
# "Claoxylon linostachys subsp. linostachys"
# "Grevillea curviloba subsp. incurva"
# "Grevillea scortechinii subsp. sarmentosa"
# "Hieracium lucidum subsp. cophanense"
# "Pinus torreyana subsp. insularis"
# "Grevillea curviloba subsp. curviloba"
# "Paphiopedilum primulinum var. primulinum"

##This species is misspelt in DW and is CR at the species level
# "Lathyrus oderatus cv. 'Bicolor'" --> Lathyrus odoratus

##These species are not CR
# Erythrina perrieri --> Endangered
# Isoetes durieui --> Least concern
# Sarcolaena grandiflora --> Vulnerable
# Melanophylla angustior --> Endangered

#Change to species level
dw_seed[dw_seed$species3=="Bupleurum dianthifolium subsp. barceloi","species3"] <- "Bupleurum dianthifolium"
dw_seed[dw_seed$species3=="Claoxylon linostachys subsp. brachyphyllum","species3"] <- "Claoxylon linostachys"
dw_seed[dw_seed$species3=="Claoxylon linostachys subsp. linostachys", "species3"] <- "Claoxylon linostachys"
dw_seed[dw_seed$species3=="Grevillea curviloba subsp. incurva","species3"] <- "Grevillea curviloba"
dw_seed[dw_seed$species3=="Grevillea scortechinii subsp. sarmentosa", "species3"] <- "Grevillea scortechinii"
dw_seed[dw_seed$species3=="Hieracium lucidum subsp. cophanense","species3"] <- "Hieracium lucidum"
dw_seed[dw_seed$species3=="Pinus torreyana subsp. insularis","species3"] <- "Pinus torreyana"
dw_seed[dw_seed$species3=="Grevillea curviloba subsp. curviloba","species3"] <- "Grevillea curviloba"
dw_seed[dw_seed$species3=="Paphiopedilum primulinum var. primulinum","species3"] <- "Paphiopedilum primulinum"
# Correct "Lathyrus oderatus"
dw_seed[dw_seed$species3=="Lathyrus oderatus cv. 'Bicolor'","species3"] <- "Lathyrus odoratus"

#Create a variable for IUCN status, and the delete the non CR
dw_seed$IUCN_st <- ifelse(dw_seed$species3=="Erythrina perrieri" | dw_seed$species3=="Melanophylla angustior", "EN",
                          ifelse(dw_seed$species3=="Sarcolaena grandiflora", "VU",
                                 ifelse(dw_seed$species3=="Isoetes durieui","LC","CR")))
dw_seed_cr<- dw_seed[dw_seed$IUCN_st=="CR",]
length(unique(dw_seed_cr$species3))
length(unique(dw_seed_cr$species2))
length(unique(dw_seed_cr$ID))

check_iucn3 <- anti_join(dw_seed_cr, iucn_CR_powo_names, by= c("species3" = "new_names"))
unique(check_iucn3$species3)
check_iucn4 <- anti_join(check_iucn3, iucn_CR_powo_names, by= c("species3" = "scientificName_IUCN"))
unique(check_iucn4$species3)

## DW data by groups: --------

#Create groups: Lycophytes, Ferns, Gymnosperms, Angiosperms
table(dw_seed_cr$FAMILY)

dw_seed_cr$groups<- ifelse(dw_seed_cr$FAMILY=="Isoetaceae", "Lycophytes",
                           ifelse(dw_seed_cr$FAMILY=="Polypodiaceae"|dw_seed_cr$FAMILY=="Marattiaceae"|
                                    dw_seed_cr$FAMILY=="Aspleniaceae" |dw_seed_cr$FAMILY=="Pteridaceae", "Ferns",
                                  ifelse(dw_seed_cr$FAMILY=="Podocarpaceae"|dw_seed_cr$FAMILY=="Pinaceae"| dw_seed_cr$FAMILY=="Podocarpaceae"|
                                           dw_seed_cr$FAMILY=="Cupressaceae"|dw_seed_cr$FAMILY=="Araucariaceae", "Gymnosperm", "Angiosperms")))

####### Representativeness CR in MSBP ------
# # write_xlsx(dw_seed_cr, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Databases created/accessions_CR_MSBP_all.xlsx")

table(dw_seed_cr$FAMILY, dw_seed_cr$groups)
aggregate(species3~groups, data=dw_seed_cr, FUN=function(x) count=length(unique(x)))
aggregate(species3~groups, data=dw_seed_cr[dw_seed_cr$SEEDBANK=="MSB",], FUN=function(x) count=length(unique(x)))
aggregate(species3~groups, data=dw_seed_cr[dw_seed_cr$SEEDBANK!="MSB",], FUN=function(x) count=length(unique(x)))

#Select only unique rows. For selecting the species present at the MSBP
species_CR_MSBP_all<- data.frame(species3=distinct(dw_seed_cr, species3,.keep_all = T)[,c("species3")],
                             MSBP_species=distinct(dw_seed_cr, species3,.keep_all = T)[,c("species3")])
dim(species_CR_MSBP_all) #394

# # write_xlsx(species_CR_MSBP_all, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Databases created/species_CR_MSBP_all.xlsx")

####### Determine reasons underlying sampling gaps-----
## Taxonomy------
#CR database + POWO names
iucn_CR_powo_name <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases\\POWO/iucn_CR_powo_names_check synonyms.csv", sep=";")

## Match MSBP species with CR plants list
#MSBP species matching with IUCN names complete name
splevel_iucn<- merge(iucn_CR_powo_name, species_CR_MSBP_all, by.x="scientificName_IUCN", by.y="species3", all=F)
dim(splevel_iucn) #341
length(unique(splevel_iucn$scientificName_IUCN)) #341
splevel_iucn$MSBP_collection<- "Yes"
splevel_iucn$MSBP_matching_name<- "IUCN name"
#MSBP species matching with POWO names
no_iucn<- anti_join(species_CR_MSBP_all, splevel_iucn,by=c("species3"="scientificName_IUCN")) #exclude the names already matched with IUCN
dim(no_iucn)#53
splevel_powo<- merge(iucn_CR_powo_name, no_iucn, by.x="new_names", by.y="species3", all=F)
dim(splevel_powo) #54. One species more because Puccinellia distans is considered at sp level by POWO, but two subsp. are CR
splevel_powo$MSBP_collection<- "Yes"
splevel_powo$MSBP_matching_name<- "POWO name"

#CR species at MSBP with IUCN and POWO names (Join previously craeted two dataframes)
msbp_cr_iucn_powo<- rbind(splevel_iucn, splevel_powo)
dim(msbp_cr_iucn_powo) #395
length(unique(msbp_cr_iucn_powo$MSBP_species)) #394 (because Puccinellia distans is repeated)

# Database with CR species not present in MSBP
not_msbp_cr_iucn_powo <- anti_join(iucn_CR_powo_name, msbp_cr_iucn_powo, by="internalTaxonId_IUCN")
not_msbp_cr_iucn_powo$MSBP_collection<- "No"
dim(not_msbp_cr_iucn_powo) #4631
length(unique(not_msbp_cr_iucn_powo$scientificName_IUCN)) #4631

#Join two dataframes: all CR sp with presence or NOT at MSBP
iucn_CR_powo_name_msbp<- smartbind(msbp_cr_iucn_powo, not_msbp_cr_iucn_powo)

# Taxonomic reasons underlying gaps: Final status of IUCN names in POWO
iucn_CR_powo_name_msbp$gap_reason <- ifelse(iucn_CR_powo_name_msbp$reason=="not found", "Not in POWO",
                                      ifelse(iucn_CR_powo_name_msbp$synonym=="synonym", "Synonym",
                                       ifelse(iucn_CR_powo_name_msbp$synonym=="homotypic synonym", "Homotypic synonym",
                                        ifelse(iucn_CR_powo_name_msbp$synonym=="no synonym" & iucn_CR_powo_name_msbp$taxonomic_status_POWO=="Synonym", "Synonym",
                                         ifelse(iucn_CR_powo_name_msbp$synonym=="no synonym" & iucn_CR_powo_name_msbp$reason=="synonym match", "Synonym",
                                           ifelse(iucn_CR_powo_name_msbp$synonym=="no synonym" & iucn_CR_powo_name_msbp$taxonomic_status_POWO=="Homotypic_Synonym"
                                                  & iucn_CR_powo_name_msbp$reason!="synonym match", "Homotypic synonym",
                                            ifelse(iucn_CR_powo_name_msbp$synonym=="no synonym" & iucn_CR_powo_name_msbp$taxonomic_status_POWO=="Unplaced", "Unplaced",
                                              ifelse(iucn_CR_powo_name_msbp$synonym=="no synonym" & iucn_CR_powo_name_msbp$taxonomic_status_POWO=="Artificial Hybrid", "Hybrid",
                                               ifelse(iucn_CR_powo_name_msbp$synonym=="no synonym" & iucn_CR_powo_name_msbp$taxonomic_status_POWO=="Accepted"
                                                      & iucn_CR_powo_name_msbp$reason=="hybrid", "Hybrid",
                                                ifelse(iucn_CR_powo_name_msbp$synonym=="no synonym" & iucn_CR_powo_name_msbp$taxonomic_status_POWO=="Accepted"
                                                        & iucn_CR_powo_name_msbp$reason=="misspelt" | iucn_CR_powo_name_msbp$reason=="misspelt ephipet" |
                                                         iucn_CR_powo_name_msbp$reason=="misspelt genus" |  iucn_CR_powo_name_msbp$reason=="misspelt infraname" | iucn_CR_powo_name_msbp$reason=="misspelt type", "Misspelt",
                                                  ifelse(iucn_CR_powo_name_msbp$synonym=="no synonym" & iucn_CR_powo_name_msbp$taxonomic_status_POWO=="Accepted"
                                                         & iucn_CR_powo_name_msbp$reason=="species level","Accepted at sp leve", "Accepted")))))))))))

# # write_xlsx(iucn_CR_powo_name_msbp, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Gap analysis - in and out MSBP/iucn_CR_powo_name_msbp.xlsx")

#Graphs for gap reasons
missing<- table(iucn_CR_powo_name_msbp[iucn_CR_powo_name_msbp$MSBP_collection=="No",]$gap_reason)
lbls_missing<- paste0(names(missing)," - ",missing," sp")

present<- table(iucn_CR_powo_name_msbp[iucn_CR_powo_name_msbp$MSBP_collection=="Yes",]$gap_reason)
lbls_present<- paste0(names(present)," - ",present," sp")

par(mar = c(0,0,1,0))
pie(missing, labels=NA, cex=.75,
    col=c("darkolivegreen2","darkolivegreen","darkmagenta","gold","firebrick1","lightblue","darkorange"))
plot.new()
legend("bottom", legend=lbls_missing, ncol=2,fill=c("darkolivegreen2","darkolivegreen","darkmagenta","gold","firebrick1","lightblue","darkorange"))

pie(present, labels=NA, cex=.75,
    col=c("darkolivegreen2","darkolivegreen","darkmagenta","lightblue","darkorange"))
plot.new()
legend("bottom", legend=lbls_present, ncol=2,fill=c("darkolivegreen2","darkolivegreen","darkmagenta","lightblue","darkorange"))

## Biological -----
cr_bio_traits<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/CRsp_IucnPowo_BioTraits_KD.csv", sep=",")
# test=readLines("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Biological traits/CRsp_IucnPowo_BioTraits.csv", sep=";")


## Match accession with CR plants list
#accessions matching with IUCN names complete name
splevel_iucn2<- merge(cr_bio_traits, species_CR_MSBP_all, by.x="scientificName_IUCN", by.y="species3", all=F)
dim(splevel_iucn2) #341
length(unique(splevel_iucn2$scientificName_IUCN)) #341
splevel_iucn2$MSBP_collection<- "Yes"
splevel_iucn2$MSBP_matching_name<- "IUCN name"
#accessions matching with POWO names
no_iucn2<- anti_join(species_CR_MSBP_all, splevel_iucn2,by=c("species3"="scientificName_IUCN")) #exclude the names already matched with IUCN
dim(no_iucn2)#53
splevel_powo2<- merge(cr_bio_traits, no_iucn2, by.x="new_names", by.y="species3", all=F)
dim(splevel_powo2) #54. One species more because Puccinellia distans is considered at sp level by POWO, but two subsp. are CR
splevel_powo2$MSBP_collection<- "Yes"
splevel_powo2$MSBP_matching_name<- "POWO name"

#CR species at MSBP with IUCN and POWO names (Join previously craeted two dataframes)
msbp_cr_iucn_powo2<- rbind(splevel_iucn2, splevel_powo2)
dim(msbp_cr_iucn_powo2) #395
length(unique(msbp_cr_iucn_powo2$MSBP_species)) #394 (because Puccinellia distans is repeated)

# Database with CR species not present in MSBP
not_msbp_cr_iucn_powo2 <- anti_join(cr_bio_traits, msbp_cr_iucn_powo2, by="internalTaxonId_IUCN")
not_msbp_cr_iucn_powo2$MSBP_collection<- "No"
dim(not_msbp_cr_iucn_powo2) #4631
length(unique(not_msbp_cr_iucn_powo2$scientificName_IUCN)) #4631

#Join two dataframes: all CR sp with presence or NOT at MSBP
cr_bio_traits_msbp<- smartbind(msbp_cr_iucn_powo2, not_msbp_cr_iucn_powo2)

# # write_xlsx(cr_bio_traits_msbp, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021/Databases/cr_bio_traits_msbp.xlsx")

#REasons underlaying gaps
table(cr_bio_traits_msbp$problematic, cr_bio_traits_msbp$MSBP_collection)

#Graphs for gap reasons

mytable.traits<- table(cr_bio_traits$problematic)
lbls.traits<- paste0(mytable.traits," sp (", round(mytable.traits/dim(cr_bio_traits)[1],2)," %)")
# lbls.traits<- paste0(mytable.sp," sp (", round(mytable.traits/dim(cr_bio_traits)[1],2)," %)")
pie(mytable.traits, labels=lbls.traits, main="", col=c( "darkorange", "darkolivegreen3", "lightblue", "white"))
plot.new()
legend("bottom", legend=names(mytable.traits), fill=c("darkorange", "darkolivegreen3", "lightblue", "white"))

missing_traits<- table(cr_bio_traits_msbp[cr_bio_traits_msbp$MSBP_collection=="No",]$problematic)
lbls_missing_traits<- paste0(names(missing_traits)," - ",missing_traits," sp")

present_traits<- table(cr_bio_traits_msbp[cr_bio_traits_msbp$MSBP_collection=="Yes",]$problematic)
lbls_missing_present<- paste0(names(present_traits)," - ",present_traits," sp")

pie(missing_traits, labels=NA, cex=.75,
    col=c("darkorange", "darkolivegreen3", "lightblue", "white"))
plot.new()
legend("bottom", legend=lbls_missing_traits, ncol=2,fill=c("darkorange", "darkolivegreen3", "lightblue", "white"))

pie(present_traits, labels=NA, cex=.75,
    col=c("darkorange", "darkolivegreen3", "lightblue", "white"))
plot.new()
legend("bottom", legend=lbls_missing_present, ncol=2,fill=c("darkorange", "darkolivegreen3", "lightblue", "white"))


## Geographic -------

#For cultivated species add country of provenance
cultivated_parentcountry<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Databases to import/MSB cultivated collections - Parent provanence.csv", sep=";")
dw_seed_cr<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Databases created/accessions_CR_MSBP_all.csv", sep=";")
cultivated_accessions<- dw_seed_cr[dw_seed_cr$CULTIVATED=="Yes",]
dim(cultivated_accessions)
cultivated_accessions_country<- merge(cultivated_accessions, cultivated_parentcountry, by="ACCESSION", all.x=T)
dim(cultivated_accessions_country)

noncultivated_accessions<- dw_seed_cr[dw_seed_cr$CULTIVATED!="Yes",]
noncultivated_accessions$Country<- noncultivated_accessions$COUNTRY

all_accessions_country<- smartbind(cultivated_accessions_country, noncultivated_accessions)

#Select unique country for each species and create database by country (n of CR species by country)
species_CR_MSBP_country<- unique(all_accessions_country[,c("species3","Country")])
n.sp.msbp_country<- aggregate(species3 ~ Country, data=species_CR_MSBP_country, FUN=function(x) count=length(x))
colnames(n.sp.msbp_country)[2]<- "cr_sp_msbp"

#join database to the database of countries
country_data3<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Geographical traits/country_CR_MSBcole_ABS.csv")
anti_join(n.sp.msbp_country,country_data3, by=c("Country"="country_name"))
country_data4<- merge(country_data3, n.sp.msbp_country, by.x="country_name", by.y="Country", all=T)

#proportion of CR sampled
country_data4$prop_cr_msbp<- country_data4$cr_sp_msbp/country_data4$CR_species_IUCN
country_data4$prop_cr_msbp<- ifelse(country_data4$CR_species_IUCN>0 & is.na(country_data4$prop_cr_msbp),0,(country_data4$prop_cr_msbp))
summary(country_data4$prop_cr_msbp)
#categorical
country_data4$prop_cr_msbp_cat<- ifelse(country_data4$prop_cr_msbp>1, "1<",
                                        ifelse(country_data4$prop_cr_msbp>=0.5 & country_data4$prop_cr_msbp<=1,"1-0.5",
                                               ifelse(country_data4$prop_cr_msbp>=0.01 & country_data4$prop_cr_msbp<0.5, "0.49-0.01",
                                            ifelse(country_data4$prop_cr_msbp==0, "0",NA))))

#Variables for reasons underlying gaps
str(country_data4$index.abs)
country_data4$abs<- ifelse(country_data4$index.abs>=2, "Strong Regulation","Weak regulation")
country_data4$col_msb<- ifelse(!is.na(country_data4$collec_MSB),"Collections in MSB","No collections in MSB")
country_data4$cr_msbp<- ifelse(!is.na(country_data4$cr_sp_msbp),"CR in MSBP","CR not in MSBP")

table(country_data4[country_data4$cr_msbp=="CR not in MSBP",]$abs, country_data4[country_data4$cr_msbp=="CR not in MSBP",]$col_msb)

# # write_xlsx(country_data4, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Databases/iucn_CR_countries_msbp.xlsx")

# MAP for CR-----
#data to plot
data_plot<-country_data4
world <- ne_countries(scale = "medium", returnclass = "sf")

#Check in country names matches with world database (database used for the map)
data_plot$country_name<- as.character(data_plot$country_name)
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

## Maps

#Number of CR at the MSBP
ggplot(data = data.map) +
  geom_sf(aes(fill= cr_sp_msbp))+
  scale_fill_viridis_c(option="plasma", name="CR species in MSBP")+
  theme_classic()

#Proportion of CR species at teh MSBO based on the total number of CR from each country
ggplot(data = data.map) +
  geom_sf(aes(fill= prop_cr_msbp_cat))+
  scale_fill_discrete(name="Proportion of CR species in MSBP")+
  theme_classic()

# countries with strong ABSCH regulations vs. weak
ggplot(data = data.map) +
  geom_sf(aes(fill= as.factor(abs))) +
  scale_fill_discrete(name= "ABSCH")+
  theme_classic()


###### Data quality - Data Warehosue database ----------
dw_seed_cr<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Databases created/accessions_CR_MSBP_all.csv", sep=";")
colnames(dw_seed_cr)

#Cultivated accession-----
table(dw_seed$CULTIVATED, dw_seed$TTH!="*")
cultivated_all <- dw_seed[dw_seed$CULTIVATED=="Yes" & dw_seed$TTH!="*",]
# # write_xlsx(cultivated_all, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Subsets of seed extract/cultivated_allaccessions.xlsx")

# TTH: Transfer to History -----
table(dw_seed_cr$TTH) #by accessions
aggregate(species3~TTH, data=dw_seed_cr, FUN=function(x) count=length(unique(x))) #by species
# (total species: 394 - no tth species:384)= 10 species are lost with tth

tth<- dw_seed_cr[dw_seed_cr$TTH=="*",]
# # write_xlsx(tth, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Subsets of seed extract\\tth.xlsx")

table(tth$SEEDBANK) #all tth belong to MSB

table(tth$SEEDDUPS) #Only 2 species with duplicates

# Duplicated ------
duplicated<- dw_seed_cr[dw_seed_cr$SEEDDUPS!="",]
# # write_xlsx(duplicated, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Subsets of seed extract\\duplicate_accessions.xlsx")

pot.dup<- data[duplicated(data[,c("species3", "DAY","MONTH","YEAR", "COUNTRY")]) | duplicated(data[,c("species3", "DAY","MONTH","YEAR", "COUNTRY")], fromLast=T),]
# # write_xlsx(pot.dup, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Subsets of seed extract\\potential_duplicated.xlsx")

# Collection and Banking date---------
nobankdate_msb <- dw_seed_cr[dw_seed_cr$BANKDATE=="  /  /    " & dw_seed_cr$SEEDBANK=="MSB" & dw_seed_cr$TTH!="*",]
# # write_xlsx(nobankdate_msb, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Subsets of seed extract\\nobankdate_msb.xlsx")

nocollectdate <- dw_seed_cr[dw_seed_cr$YEAR=="0" & dw_seed_cr$TTH!="*",]
# # write_xlsx(nocollectdate, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Subsets of seed extract\\nocollectdate.xlsx")

# Exclude tth-------
dw_seed_cr_notth <- dw_seed_cr[dw_seed_cr$TTH!="*",]

length(unique(dw_seed_cr_notth$ID))
length(unique(dw_seed_cr_notth$species3)) #species in MSB-P

length(unique(dw_seed_cr_notth[dw_seed_cr_notth$SEEDBANK=="MSB",]$ID))
length(unique(dw_seed_cr_notth[dw_seed_cr_notth$SEEDBANK=="MSB",]$species3)) #species in MSB

length(unique(dw_seed_cr_notth[dw_seed_cr_notth$SEEDBANK!="MSB",]$ID))
length(unique(dw_seed_cr_notth[dw_seed_cr_notth$SEEDBANK!="MSB",]$species3)) #species in Partners

table(dw_seed_cr_notth$SEEDBANK)

aggregate(species3~groups, data=dw_seed_cr_notth, FUN=function(x) count=length(unique(x))) #by major groups
aggregate(species3~groups, data=dw_seed_cr_notth[dw_seed_cr_notth$SEEDBANK=="MSB",], FUN=function(x) count=length(unique(x)))
aggregate(species3~groups, data=dw_seed_cr_notth[dw_seed_cr_notth$SEEDBANK!="MSB",], FUN=function(x) count=length(unique(x)))
# Taxomony verification data ------
dw_ver1 <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/Ver of CR species.csv", sep=";")
dw_ver2 <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/17_7accs-with-ver-status.csv", sep=";")
dw_ver3 <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/NewCR_NewPowoName_VERIFICATIONS.csv", sep=";")
length(unique(dw_ver1$ACCESSION)) #612 accessions
length(unique(dw_ver2$ACCESSION)) #23 accessions
length(unique(dw_ver3$ACCESSION)) #212 accessions

#check for duplicates and join data bases
dw_ver1$ACCESSION <- as.factor(dw_ver1$ACCESSION); dw_ver2$ACCESSION <- as.factor(dw_ver2$ACCESSION)
inner_join(dw_ver1, dw_ver2, dw_ver3, by="ACCESSION") # no matching accessions between the data bases
dw_ver1[duplicated(dw_ver1$ACCESSION),] # no duplicated accessions
dw_ver2[duplicated(dw_ver2$ACCESSION) | duplicated(dw_ver2$ACCESSION, fromLast=T),] #duplicated accessions, select only "VERDADERO"
dw_ver3[duplicated(dw_ver3$ACCESSION),] # no duplicated accessions

dw_ver <- smartbind(dw_ver1, dw_ver2[dw_ver2$Current.Name.=="VERDADERO",], dw_ver3)
dw_ver<- dw_ver[!duplicated(dw_ver$ACCESSION),] #still some duplicates, remove them
length(dw_ver$ACCESSION) #847 accessions

table(dw_ver$Verification.Status)

#Create a new variable: verified vs. unverified
dw_ver$ver.st <- ifelse(dw_ver$Verification.Status=="Field identification by specialist. Taken as verified."|dw_ver$Verification.Status=="Taxonomic change of material previously verified"|
                          dw_ver$Verification.Status=="Verified by Kew Herbarium"|dw_ver$Verification.Status=="Verified by other institution from field or garden voucher or photograph", "verified", "unverified")
table(dw_ver$Verification.Status, dw_ver$ver.st)

#Add taxonomy status to main database
dw_seed_cr_notth_ver<- merge(dw_seed_cr_notth, dw_ver[,c(1,18)], by="ACCESSION", all.x=T)
table(dw_seed_cr_notth_ver$ver.st, dw_seed_cr_notth_ver$SEEDBANK)

length(unique(dw_seed_cr_notth_ver$species3))

# Cultivation protocol data -------
cul_prot <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/SBD CR cultivated data to Itxaso 13Aug2021.csv", sep=";")
# add cultivation protocol for new accessions (after checking POwo names and IUCN update)
cul_prot_new<- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Databases to import/cultivated_new with MSB data.csv", sep=";")

#join cultivation protocol info to main database. First check if accessions are the same
colnames(cul_prot)
cul_prot$MSB.serial.number <- as.factor(cul_prot$MSB.serial.number)
anti_join(cul_prot, dw_seed_cr_notth_ver, by=c("MSB.serial.number"="ACCESSION")) #the not matching accessions are the tth ones
colnames(cul_prot_new)
cul_prot_new$serial.number <- as.factor(cul_prot_new$serial.number)
anti_join(cul_prot_new, dw_seed_cr_notth_ver, by=c("serial.number"="ACCESSION"))
#join both cultivation protocol data
colnames(cul_prot)[1]<- "serial.number"
cul_prot_all<- smartbind(cul_prot, cul_prot_new)
colnames(cul_prot_all)
#join with main database the following info: n regenerated, n harvested, relation to parent and isolation technique
dw_seed_cr_notth_ver_cul <- merge(dw_seed_cr_notth_ver, cul_prot_all[,c(1,10,11,18,19)], by.x="ACCESSION", by.y="serial.number", all.x=T)
length(unique(dw_seed_cr_notth_ver_cul$species3))

table(dw_seed_cr_notth_ver_cul$CULTIVATED)
table(dw_seed_cr_notth_ver_cul[dw_seed_cr_notth_ver_cul$SEEDBANK=="MSB",]$CULTIVATED)
table(dw_seed_cr_notth_ver_cul[dw_seed_cr_notth_ver_cul$SEEDBANK!="MSB",]$CULTIVATED)


### Create a matrix to fill in with data value -----------
data <- dw_seed_cr_notth_ver_cul

# Type of Partners (data only partners vs. Other partners)
data$type_partner<- ifelse(data$SEEDBANK=='NBC, Bhutan' |data$SEEDBANK=='IAVH' |data$SEEDBANK=='BOYACA'
                           |data$SEEDBANK=='Cibodas' |data$SEEDBANK=='NZIFSB'|data$SEEDBANK=='NGBRC', "Data only partners",
                           ifelse(data$SEEDBANK=='MSB', "MSB", "Other partners"))

# Last year test information in date format and create a variable with only year
data$LASTTEST <- as.Date(data$LASTTEST, "%d/%m/%Y")
data$LASTTESTYEAR <- format(data$LASTTEST, format="%Y")

# Modify plant total and plant sampled to numbers manually
data$PLANTSAMP <- as.character(data$PLANTSAMP)
data$plantsamp_n <-  ifelse(data$PLANTSAMP=="2-4" | data$PLANTSAMP=="3 clumps" | data$PLANTSAMP==">3","3",
                            ifelse(data$PLANTSAMP=="4-5","4",
                                   ifelse(data$PLANTSAMP=="~10" | data$PLANTSAMP=="<10","10",
                                          ifelse(data$PLANTSAMP=="12+","12",
                                                 ifelse(data$PLANTSAMP=="10-24" | data$PLANTSAMP=="10 - 25"| data$PLANTSAMP=="10-25","17",
                                                        ifelse(data$PLANTSAMP==">20" | data$PLANTSAMP=="20+" |data$PLANTSAMP=="10-30","20",
                                                               ifelse(data$PLANTSAMP==">30", "30",
                                                                      ifelse(data$PLANTSAMP=="25-50", "37",
                                                                             ifelse(data$PLANTSAMP==">50","50",
                                                                                    ifelse(data$PLANTSAMP=="11-100","55",
                                                                                           ifelse(data$PLANTSAMP=="50-99","75",
                                                                                                  ifelse(data$PLANTSAMP==">100" | data$PLANTSAMP=="100+","100",
                                                                                                         ifelse(data$PLANTSAMP=="200-500","300",
                                                                                                                ifelse(data$PLANTSAMP=="100-1000","550",
                                                                                                                       ifelse(data$PLANTSAMP=="" | data$PLANTSAMP=="0",NA, data$PLANTSAMP)))))))))))))))



table(data$plantsamp_n)


data$PLANTTOTAL <- as.character(data$PLANTTOTAL)
data$planttotal_n <- ifelse(data$PLANTTOTAL=="2-4" | data$PLANTTOTAL=="3 clumps","3",
                      ifelse(data$PLANTTOTAL=="<1-10" | data$PLANTTOTAL=="1-10" | data$PLANTTOTAL=="5+", "5",
                       ifelse(data$PLANTTOTAL==">= 7 clump","7",
                        ifelse(data$PLANTTOTAL==">8","8",
                         ifelse(data$PLANTTOTAL==">=10" | data$PLANTTOTAL=="<10", "10",
                          ifelse(data$PLANTTOTAL=="12-15","13",
                           ifelse(data$PLANTTOTAL=="7 + 8 clum","15",
                            ifelse(data$PLANTTOTAL==">=19", "19",
                             ifelse(data$PLANTTOTAL=="20+","20",
                              ifelse(data$PLANTTOTAL=="20-30","25",
                               ifelse(data$PLANTTOTAL=="30+" | data$PLANTTOTAL=="11-50", "30",
                                ifelse(data$PLANTTOTAL==">35","35",
                                 ifelse(data$PLANTTOTAL=="25-50","37",
                                  ifelse(data$PLANTTOTAL=="45+","45",
                                   ifelse(data$PLANTTOTAL=="50+" | data$PLANTTOTAL==">50", "50",
                                    ifelse(data$PLANTTOTAL=="10-100", "55",
                                     ifelse(data$PLANTTOTAL==">60" | data$PLANTTOTAL=="25-99", "60",
                                      ifelse(data$PLANTTOTAL==">70", "70",
                                       ifelse(data$PLANTTOTAL=="50 - 100" | data$PLANTTOTAL=="50-100 mat" | data$PLANTTOTAL=="51-100" | data$PLANTTOTAL=="50-100", "75",
                                        ifelse(data$PLANTTOTAL=="~80", "80",
                                         ifelse(data$PLANTTOTAL=="~100" | data$PLANTTOTAL=="091-100"  | data$PLANTTOTAL=="100s" |
                                                data$PLANTTOTAL=="> 100" | data$PLANTTOTAL==">>100" | data$PLANTTOTAL==">100" | data$PLANTTOTAL=="100+", "100",
                                           ifelse(data$PLANTTOTAL=="~150" | data$PLANTTOTAL==">150" | data$PLANTTOTAL=="150+", "150",
                                            ifelse(data$PLANTTOTAL=="150-200" | data$PLANTTOTAL=="101-250", "175",
                                             ifelse(data$PLANTTOTAL=="~200" | data$PLANTTOTAL==">200" | data$PLANTTOTAL=="200+", "200",
                                              ifelse(data$PLANTTOTAL=="250+", "250",
                                               ifelse(data$PLANTTOTAL=="100-500", "300",
                                                ifelse(data$PLANTTOTAL==">350", "350",
                                                 ifelse(data$PLANTTOTAL=="250-500" | data$PLANTTOTAL=="251 - 500", "375",
                                                  ifelse(data$PLANTTOTAL=="400+","400",
                                                   ifelse(data$PLANTTOTAL=="400-500", "450",
                                                    ifelse(data$PLANTTOTAL==">500" | data$PLANTTOTAL==">500 matur", "500",
                                                     ifelse(data$PLANTTOTAL=="100-1,000" | data$PLANTTOTAL=="100-1000" | data$PLANTTOTAL=="100-999", "550",
                                                      ifelse(data$PLANTTOTAL=="500-1000","750",
                                                      ifelse(data$PLANTTOTAL=="<1000" | data$PLANTTOTAL==">1000" | data$PLANTTOTAL=="1000's" |data$PLANTTOTAL=="1000+", "1000",
                                                       ifelse(data$PLANTTOTAL=="1001-2500", "1750",
                                                        ifelse(data$PLANTTOTAL==">2000", "2000",
                                                         ifelse(data$PLANTTOTAL=="6000+", "6000",
                                                          ifelse(data$PLANTTOTAL=="200000+", "200000",
                                                           ifelse(data$PLANTTOTAL=="" | data$PLANTTOTAL=="0" | data$PLANTTOTAL=="999",NA, data$PLANTTOTAL)))))))))))))))))))))))))))))))))))))))



table(data$planttotal_n)


# Create matrix
matrix<- data.frame(ID=data$ID,
                    ACCESSION=data$ACCESSION,
                    species=data$species3,
                    species_short= data$species2,
                    FAMILY=data$FAMILY,
                    group=data$groups,
                    SEEDBANK=data$SEEDBANK,
                    SEEDUPS= data$SEEDDUPS,
                    COORDINATES= paste0(round(data$LAT,2), ",",round(data$LONG,2)),
                    LOCALITY= ifelse(data$LAT>0, as.character(paste0(data$LAT, ",",data$LONG)),
                                     ifelse(data$LAT==0 & data$LOCNOTES!="", as.character(data$LOCNOTES),
                                            ifelse(data$LAT==0 & data$LOCNOTES=="" & data$MINORAREA!="", as.character(data$MINORAREA),
                                                   ifelse(data$LAT==0 & data$LOCNOTES=="" & data$MINORAREA=="" & data$MAJORAREA!="", as.character(data$MAJORAREA),
                                                          ifelse(data$LAT==0 & data$LOCNOTES=="" & data$MINORAREA=="" & data$MAJORAREA=="" & data$COUNTRY!="", as.character(data$COUNTRY),NA))))),
                    COUNTRY= data$COUNTRY,
                    BANKDATE=as.Date(data$BANKDATE, "%d/%m/%Y"),
                    COLLECTYEAR=data$YEAR,
                    CURRCOUNT=data$CURRCOUNT,
                    ADJSTCOUNT=data$ADJSTCOUNT,
                    LASTTESTYEAR= data$LASTTESTYEAR,
                    BESTLAST=data$BESTLAST,
                    GENERATION = data$Relation.to.Parent,
                    ISOLATION = data$Isolation.Technique,
                    PLANTHARVESTD= data$Number.Harvested,
                    PLANTSAMPLED= data$plantsamp_n,
                    PLANTTOTAL= data$planttotal_n,
                    PLANTSEED_pr= ifelse(data$plantsamp_n!="" & data$planttotal_n!="" & data$plantsamp_n==data$planttotal_n & data$PCSEED=="", 100,as.numeric(data$PCSEED)),
                    seedbank= ifelse(data$SEEDBANK=="MSB",1,0),
                    bankdate=ifelse(data$BANKDATE=="  /  /    ",0,1),
                    collectdate=ifelse(data$YEAR=="0",0,1),
                    coordinates=ifelse(data$LONG=="0",0,1),
                    area=ifelse(data$MAJORAREA!="" | data$MINORAREA!="" | data$LOCNOTES!="" ,1,0),
                    country=ifelse(data$COUNTRY=="?",0,1),
                    taxonomy= ifelse(data$ver.st=="verified",1,0),
                    currcount=ifelse(data$CURRCOUNT=="0" & data$BANKDATE=="  /  /    ",0,
                                     ifelse(data$CURRCOUNT=="0" & data$BANKDATE!="  /  /    ",-9,1)),
                    adjcount=ifelse(data$ADJSTCOUNT=="0" & data$CURRCOUNT=="0" & data$XRAYDATE=="  /  /    " & data$CUTTEST=="No" & data$TZDATE=="  /  /    ",0,
                                    ifelse(data$ADJSTCOUNT=="0" & data$CURRCOUNT!="0" & data$XRAYDATE=="  /  /    " & data$CUTTEST=="No" & data$TZDATE=="  /  /    ",0,
                                           ifelse(data$ADJSTCOUNT=="0" & data$CURRCOUNT=="0" & data$XRAYDATE!="  /  /    " , -9,1))),
                    germ.test=ifelse(data$FIRSTTEST=="  /  /    ",0,1),
                    germ.test.15=ifelse(data$FIRSTTEST!="  /  /    " & data$LASTTESTYEAR>=2006,1,0),
                    germ.test.249=ifelse(data$FIRSTTEST!="  /  /    " & data$CURRCOUNT>=249,1,
                                         ifelse(data$FIRSTTEST=="  /  /    " & data$CURRCOUNT>=249,0,NA)),
                    cultivated=ifelse(data$CULTIVATED=="Yes",1,0),
                    generation=ifelse(data$Relation.to.Parent=="" | is.na(data$Relation.to.Parent),0,1),
                    isolation=ifelse(data$Isolation.Technique=="" | is.na(data$Isolation.Technique),0,1),
                    plant.sampled= ifelse(is.na(data$plantsamp_n),0,1),
                    plant.tot= ifelse(is.na(data$planttotal_n),0,1),
                    plant.seed= ifelse(data$PCSEED!="",1,
                                       ifelse(!is.na(data$plantsamp_n) & !is.na(data$planttotal_n) & data$plantsamp_n==data$planttotal_n & data$PCSEED=="",1,0)))


# Difference between NO current count and current count==0?
## All accession with 0 current count, no banking data--> They are processing, we assume no current count available yet
## Check Accession: NZ69, current count=0, but banking date (does it mean there are no seeds)?

# Difference between NO adjusted count and adjusted count==0?
##Accession with adjusted count =0 and current count =0 --> They area processing, no adjusted count yet
##Accession with adjusted count =0 and current count >0 and no test available (xray, cut, tz)--> No adjuested count
## Check Accession: NZ69, adjusted and current count=0, but test were done and banking date (does it mean there are no seeds)?

# For MISSING data Proportion of plants with seeds(PLANTSEED_pr):
## If plant sampled == plan total, then we assumed that the plants with seeds where %100
# PCseeds--> proportion of plants in seeds, when we only have this value we can't know the number of sampled plants.


# # write_xlsx(matrix, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Subsets of seed extract\\matrix_09_10_21.xlsx")

## Raw numbers for accessions---------

## Information data variables

#MSB-P
table(matrix$coordinates)
table(matrix$area)
table(matrix$country)
table(matrix$taxonomy)
table(matrix$collectdate)
table(matrix$bankdate)

#MSB
table(matrix[matrix$SEEDBANK=="MSB",]$coordinates)
table(matrix[matrix$SEEDBANK=="MSB",]$area)
table(matrix[matrix$SEEDBANK=="MSB",]$country)
table(matrix[matrix$SEEDBANK=="MSB",]$taxonomy)
table(matrix[matrix$SEEDBANK=="MSB",]$collectdate)
table(matrix[matrix$SEEDBANK=="MSB",]$bankdate)

#Partners
table(matrix[matrix$SEEDBANK!="MSB",]$coordinates)
table(matrix[matrix$SEEDBANK!="MSB",]$area)
table(matrix[matrix$SEEDBANK!="MSB",]$country)
table(matrix[matrix$SEEDBANK!="MSB",]$taxonomy)
table(matrix[matrix$SEEDBANK!="MSB",]$collectdate)
table(matrix[matrix$SEEDBANK!="MSB",]$bankdate)

## Viability data variables

#MSB-P
table(matrix$currcount)
table(matrix$adjcount)
table(matrix$germ.test)
table(matrix$germ.test.15)
table(matrix$germ.test.249)

#MSB
table(matrix[matrix$SEEDBANK=="MSB",]$currcount)
table(matrix[matrix$SEEDBANK=="MSB",]$adjcount)
table(matrix[matrix$SEEDBANK=="MSB",]$germ.test)
table(matrix[matrix$SEEDBANK=="MSB",]$germ.test.15)
table(matrix[matrix$SEEDBANK=="MSB",]$germ.test.249)

#Partners
table(matrix[matrix$SEEDBANK!="MSB",]$currcount)
table(matrix[matrix$SEEDBANK!="MSB",]$adjcount)
table(matrix[matrix$SEEDBANK!="MSB",]$germ.test)
table(matrix[matrix$SEEDBANK!="MSB",]$germ.test.15)
table(matrix[matrix$SEEDBANK!="MSB",]$germ.test.249)

## Genetic data variables

#MSB-P
table(matrix[matrix$cultivated=="1",]$generation)
table(matrix[matrix$cultivated=="1",]$isolation)
table(matrix[matrix$cultivated=="0",]$plant.sampled)
table(matrix[matrix$cultivated=="0",]$plant.tot)
table(matrix[matrix$cultivated=="0",]$plant.seed)

#MSB
table(matrix[matrix$cultivated=="1" & matrix$SEEDBANK=="MSB",]$generation)
table(matrix[matrix$cultivated=="1" & matrix$SEEDBANK=="MSB",]$isolation)
table(matrix[matrix$cultivated=="0" & matrix$SEEDBANK=="MSB",]$plant.sampled)
table(matrix[matrix$cultivated=="0" & matrix$SEEDBANK=="MSB",]$plant.tot)
table(matrix[matrix$cultivated=="0" & matrix$SEEDBANK=="MSB",]$plant.seed)

#Partners
table(matrix[matrix$cultivated=="1" & matrix$SEEDBANK!="MSB",]$generation)
table(matrix[matrix$cultivated=="1" & matrix$SEEDBANK!="MSB",]$isolation)
table(matrix[matrix$cultivated=="0" & matrix$SEEDBANK!="MSB",]$plant.sampled)
table(matrix[matrix$cultivated=="0" & matrix$SEEDBANK!="MSB",]$plant.tot)
table(matrix[matrix$cultivated=="0" & matrix$SEEDBANK!="MSB",]$plant.seed)



## Create graphs of raw numbers --------

stacked_matrix_msb<- stack(matrix[matrix$seedbank=="1",c("coordinates", "area", "country", "taxonomy", "collectdate","currcount","adjcount", "germ.test","germ.test.15", "germ.test.249","cultivated","generation","isolation","plant.tot","plant.seed", "plant.sampled")])
msb_info<-aggregate(stacked_matrix_msb$values, by=list(variable=stacked_matrix_msb$ind), FUN=sum, na.rm=T)
msb_info$variable <- as.character(msb_info$variable)
msb_info<- rbind (msb_info, c(as.character("non.cultivated"),nrow(matrix[matrix$seedbank=="1" & matrix$cultivated==0,])))
msb_info$seedbank <- rep("MSB", dim(msb_info)[1])
msb_info$variable <- as.factor(msb_info$variable); msb_info$x <- as.numeric(msb_info$x)
msb_info$percent <- round(msb_info$x *100 / dim(matrix[matrix$seedbank==1,])[1], 0) #696 is the total number of accessions from MSB
msb_info$percent[c(10)] <- round(msb_info$x[c(10)] * 100 / length(matrix[matrix$CURRCOUNT>=249 & matrix$seedbank==1,]$germ.test.249), 0) #385 is the number of accessions with current coun >=249 for MSB
msb_info$percent[c(12,13)] <- round(msb_info$x[c(12,13)] * 100 / length(matrix[matrix$cultivated==1 & matrix$seedbank==1,]$cultivated), 0) #113 is the number of cultivated accessions from MSB
msb_info$percent[c(14:16)] <- round(msb_info$x[c(14:16)] * 100 / length(matrix[matrix$cultivated==0 & matrix$seedbank==1,]$cultivated), 0) #484 is the number of non-cultivated accessions from MSB

stacked_matrix_partners<- stack(matrix[matrix$seedbank=="0",c("coordinates", "area", "country", "taxonomy", "collectdate", "currcount","adjcount", "germ.test", "germ.test.15", "germ.test.249","cultivated","generation","isolation","plant.tot","plant.seed","plant.sampled")])
partners_info<- aggregate(stacked_matrix_partners$values, by=list(variable=stacked_matrix_partners$ind), FUN=sum, na.rm=T)
partners_info$variable <- as.character(partners_info$variable)
partners_info<- rbind (partners_info, c(as.character("non.cultivated"),nrow(matrix[matrix$seedbank=="0" & matrix$cultivated==0,])))
partners_info$seedbank <- rep("Partners", dim(partners_info)[1])
partners_info$variable <- as.factor(partners_info$variable); partners_info$x <- as.numeric(partners_info$x)
partners_info$percent <- round(partners_info$x *100 / dim(matrix[matrix$seedbank==0,])[1], 0) #1458 is the total number of accessions from the Partners
partners_info$percent[c(10)] <- round(partners_info$x[c(10)] * 100 / length(matrix[matrix$CURRCOUNT>=249 & matrix$seedbank==0,]$germ.test.249), 0) #79 is the number of accessions with current coun >=249 for MSB
partners_info$percent[c(12,13)] <- round(partners_info$x[c(12,13)] * 100 / length(matrix[matrix$cultivated==1 & matrix$seedbank==0,]$cultivated), 0) #73 is the number of cultivated accessions from Partners
partners_info$percent[c(14:16)] <- round(partners_info$x[c(14:16)] * 100 / length(matrix[matrix$cultivated==0 & matrix$seedbank==0,]$cultivated), 0) #1111 is the number of non-cultivated accessions from Partners


data_variables <- rbind(msb_info,partners_info)


info_variables <- data_variables[c(1:5,18:22),]
info_variables$variable <- factor(info_variables$variable, levels= c( "coordinates","area","country","taxonomy","collectdate"))

info_gg<- ggplot(info_variables, aes(fill=seedbank, y=x, x=variable, label=paste0(percent,"%"))) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(position= position_dodge(1), vjust=-0.5,colour = "black")+
  scale_fill_manual(values = c("darkorange","cornflowerblue"))+
  theme_classic()+
  theme(legend.position = "none",
        text = element_text(size = 15))+
  labs(x ="Information variables", y= "Number of accessions")+
  scale_x_discrete(labels=c("coordinates"="Coordinates","area"="Area", "country"="Country",
                            "taxonomy"="Taxonomic\nverification", "collectdate"="Collection year"))+
  ylim(0,1500)


viab_variables <- data_variables[c(6:10,23:27),]
viab_variables$variable <- factor(viab_variables$variable, levels= c("currcount","adjcount","germ.test","germ.test.15", "germ.test.249"))

viab_gg<- ggplot(viab_variables, aes(fill=seedbank, y=x, x=variable, label=paste0(percent,"%"))) +
  geom_bar(position="dodge", stat="identity")+
  geom_text(position= position_dodge(1), vjust=-0.5,colour = "black")+
  scale_fill_manual(values = c("darkorange","cornflowerblue"))+
  theme_classic()+
  theme(legend.position = "none",
        text = element_text(size = 15))+
  labs(x ="Viability variables", y= "Number of accessions")+
  scale_x_discrete(labels=c("currcount"="Current count","adjcount"="Adjusted count", "germ.test"="Germination\ntest",
                            "germ.test.15"="Recent germination\ntest", "germ.test.249"="Germination test\n>249 seeds"))+
  ylim(0,1500)


gen_variables <- data_variables[c(11:17,28:34),]
gen_variables$variable <- factor(gen_variables$variable, levels= c("cultivated","generation","isolation","non.cultivated","plant.sampled","plant.tot","plant.seed"))

gen_gg<- ggplot(gen_variables, aes(fill=seedbank, y=x, x=variable, label=paste0(percent,"%"))) +
  geom_bar(position="dodge", stat="identity")+
  geom_text(position= position_dodge(1), vjust=-0.5,colour = "black")+
  scale_fill_manual(values = c("darkorange","cornflowerblue"))+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15))+
  labs(x ="Genetic diversity variables", y= "Number of accessions")+
  scale_x_discrete(labels=c("cultivated"="Cultivated","generation"="Relation to\nparent","isolation"="Isolation\nmethod",
                            "non.cultivated"="Non\ncultivated","plant.tot"="Total plants", "plant.seed"="Plants with\nseeds", "plant.sampled"="Sampled\nplants"))+
  ylim(0,1500)

ggarrange(info_gg,viab_gg,gen_gg,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)


## Calculate data value index ------

# Data index:

## [(Geo + Taxa + Collect.year) /2 + (Current.count + Adjusted.count + Germination.test) /3 + Genetic.div] /3
# where:
# Geo: 0 no info, 0.5 country, 0.75 area, 1 coordinates
# Taxa: 0 not verified or no info, 1 verified
# Collect.year: 0 no infor, 1 year data
# Current.count: 0 no (in process), 1 yes
# Adjusted.count: 0 no, 1 yes
# Germination.test: 0 no test, 0.5 test older than 15years, 1 recent test
# Genetic.div
# Cultivated: 0 no info or coded as non-cultivated, 0.5 info: "Relation to parent" OR "Isolation technique", 1 info: "Relation to parent" AND "Isolation technique"
# Non-cultivated: 0 no info, 0.2 info: "Plant total" OR "Plant seed", 0.4 info: Plant total" AND "Plant seed",
#                 0.6 info: "Plant sampled", 0.8 info: "Plant sampled" AND "Plant total" OR "Plant seed",
#                 1 info: "Plant total" AND "Plant with seeds" AND "Plant sampled"

matrix$Geo <- ifelse(matrix$coordinates=="1",1,
                     ifelse(matrix$coordinates=="0" & matrix$area=="1", 0.75,
                            ifelse(matrix$coordinates=="0" & matrix$area=="0" & matrix$country=="1", 0.5,0)))
matrix$Taxa <- ifelse(matrix$taxonomy=="0" | is.na(matrix$taxonomy),0,1)
matrix$Collect.year <- ifelse(matrix$collectdate=="1",1,0)
matrix$Current.count <- ifelse(matrix$currcount=="1",1,0)
matrix$Adjusted.count <- ifelse(matrix$adjcount=="1",1,0)
matrix$Germination.test <- ifelse(matrix$germ.test=="1" & matrix$germ.test.15=="1",1,
                                  ifelse(matrix$germ.test=="1" & matrix$germ.test.15=="0", 0.5,0))  #for cultivated
matrix$Genetic.div <- ifelse(matrix$cultivated=="1" & matrix$generation=="0" & matrix$isolation=="0", 0,
                             ifelse(matrix$cultivated=="1" & matrix$generation=="1" & matrix$isolation=="0", 0.5,
                                    ifelse(matrix$cultivated=="1" & matrix$generation=="0" & matrix$isolation=="1", 0.5,
                                           #for non-cultivated
                                           ifelse(matrix$cultivated=="0" & matrix$plant.sampled=="0" & matrix$plant.tot=="0" &  matrix$plant.seed=="0",0,
                                                  ifelse(matrix$cultivated=="0" & matrix$plant.sampled=="0" & matrix$plant.tot=="1" & matrix$plant.seed=="0",0.2,
                                                         ifelse(matrix$cultivated=="0" & matrix$plant.sampled=="0" & matrix$plant.tot=="0" & matrix$plant.seed=="1",0.2,
                                                                ifelse(matrix$cultivated=="0" & matrix$plant.sampled=="0" & matrix$plant.tot=="1" & matrix$plant.seed=="1",0.4,
                                                                       ifelse(matrix$cultivated=="0" & matrix$plant.sampled=="1" & matrix$plant.tot=="0" & matrix$plant.seed=="0", 0.6,
                                                                              ifelse(matrix$cultivated=="0" & matrix$plant.sampled=="1" & matrix$plant.tot=="1" & matrix$plant.seed=="0", 0.8,
                                                                                     ifelse(matrix$cultivated=="0" & matrix$plant.sampled=="1" & matrix$plant.tot=="0" & matrix$plant.seed=="1", 0.8,1))))))))))




#Index calculation formula:
matrix$index_info <- with(matrix, (Geo + Taxa + Current.count)/3)
matrix$index_viab <- with(matrix, (Current.count + Adjusted.count + Germination.test)/3)
matrix$index_gene <- matrix$Genetic.div
matrix$index_total <- with(matrix, (index_info + index_viab + index_gene)/3)


table(matrix$index_total)

# # write_xlsx(matrix, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Subsets of seed extract\\matrix_index_09_10_21.xlsx")

summary(matrix[matrix$seedbank==1,]$index_info)
summary(matrix[matrix$seedbank==0,]$index_info)

summary(matrix[matrix$seedbank==1,]$index_viab)
summary(matrix[matrix$seedbank==0,]$index_viab)

summary(matrix[matrix$seedbank==1 & matrix$cultivated==1,]$index_gene)
summary(matrix[matrix$seedbank==0 & matrix$cultivated==1,]$index_gene)

summary(matrix[matrix$seedbank==1 & matrix$cultivated==0,]$index_gene)
summary(matrix[matrix$seedbank==0 & matrix$cultivated==0,]$index_gene)

summary(matrix[matrix$seedbank==1,]$index_total)
summary(matrix[matrix$seedbank==0,]$index_total)


## Plot information value index -----

#Prepare data to plot: stack the index values, separate cultivated and non-cultiavted, and separe data from MSB and for partners
stacked <- stack(matrix[,c("index_info", "index_viab", "index_total")])
stacked_gen <- data.frame(values= c(matrix[matrix$cultivated==1,]$index_gene,matrix[matrix$cultivated==0,]$index_gene),
                          ind= c(rep("index_gene_cul", length(matrix[matrix$cultivated==1,]$index_gene)), rep("index_gene_noncul", length(matrix[matrix$cultivated==0,]$index_gene))))
stacked_index <- rbind(stacked, stacked_gen)

stacked_MSB <- stack(matrix[matrix$seedbank=="1",c("index_info", "index_viab", "index_total")])
stacked_gen_MSB <- data.frame(values= c(matrix[matrix$cultivated==1 & matrix$seedbank==1,]$index_gene,matrix[matrix$cultivated==0 & matrix$seedbank==1,]$index_gene),
                              ind= c(rep("index_gene_cul", length(matrix[matrix$cultivated==1 & matrix$seedbank==1,]$index_gene)), rep("index_gene_noncul", length(matrix[matrix$cultivated==0 & matrix$seedbank==1,]$index_gene))))
stacked_index_MSB<- rbind(stacked_MSB, stacked_gen_MSB)

stacked_partners <- stack(matrix[matrix$seedbank=="0",c("index_info", "index_viab", "index_total")])
stacked_gen_partners <- data.frame(values= c(matrix[matrix$cultivated==1 & matrix$seedbank==0,]$index_gene,matrix[matrix$cultivated==0 & matrix$seedbank==0,]$index_gene),
                                   ind= c(rep("index_gene_cul", length(matrix[matrix$cultivated==1 & matrix$seedbank==0,]$index_gene)), rep("index_gene_noncul", length(matrix[matrix$cultivated==0 & matrix$seedbank==0,]$index_gene))))
stacked_index_partners<- rbind(stacked_partners, stacked_gen_partners)

#Plot the index using violin boxplots

#MSB-p
stacked_index$ind<- factor(stacked_index$ind, levels=c("index_info", "index_viab", "index_gene_cul", "index_gene_noncul", "index_total"))

vioplot(stacked_index$values ~ stacked_index$ind,
        col="darkmagenta",
        ylab="Index", xlab="", xaxt="n")

axis(side=1, at=1:5,labels=c("Information", "Viability", "Genetic diversity\nCultivated plants", "Genetic diversity\nNon-Cultivated plants", "Total"))

legend("bottomleft", legend = c("MSB-P"), fill = c("darkmagenta"), cex = 1)

#Separate MSB and Partners
stacked_index_MSB$ind<- factor(stacked_index_MSB$ind, levels=c("index_info", "index_viab", "index_gene_cul", "index_gene_noncul", "index_total"))
stacked_index_partners$ind<- factor(stacked_index_partners$ind, levels=c("index_info", "index_viab", "index_gene_cul", "index_gene_noncul", "index_total"))

vioplot(stacked_index_MSB$values ~ stacked_index_MSB$ind, side="left",
        col="darkorange",
        ylab="Index", xlab="", xaxt="n")

vioplot(stacked_index_partners$values ~ stacked_index_partners$ind,side="right",
        col="cornflowerblue",
        ylab="Index", xlab="",xaxt="n",
        add=T)

axis(side=1, at=1:5,labels=c("Information", "Viability", "Genetic diversity\nCultivated plants", "Genetic diversity\nNon-Cultivated plants", "Total"))

legend("bottomleft", legend = c("MSB", "Partners"), fill = c("darkorange", "cornflowerblue"), cex = 1)

# Plot index values for information variables
stacked_index_info_MSB <- stack(matrix[matrix$seedbank=="1",c("Geo","Taxa","Collect.year","index_info")])
stacked_index_info_MSB$seedbank <- rep("MSB", dim(stacked_index_info_MSB)[1])
stacked_index_info_Partners <- stack(matrix[matrix$seedbank=="0",c("Geo","Taxa","Collect.year","index_info")])
stacked_index_info_Partners$seedbank <- rep("Partners", dim(stacked_index_info_Partners)[1])

stacked_index_info <- rbind(stacked_index_info_MSB, stacked_index_info_Partners)

ggplot(stacked_index_info,aes(x=ind,y=values,fill=seedbank)) +
  geom_violin()

# Plot index values for viability variables
stacked_index_viab_MSB <- stack(matrix[matrix$seedbank=="1",c("Current.count","Adjusted.count","Germination.test","index_viab")])
stacked_index_viab_MSB$seedbank <- rep("MSB", dim(stacked_index_viab_MSB)[1])
stacked_index_viab_Partners <- stack(matrix[matrix$seedbank=="0",c("Current.count","Adjusted.count","Germination.test","index_viab")])
stacked_index_viab_Partners$seedbank <- rep("Partners", dim(stacked_index_viab_Partners)[1])

stacked_index_viab <- rbind(stacked_index_viab_MSB, stacked_index_viab_Partners)

ggplot(stacked_index_viab,aes(x=ind,y=values,fill=seedbank)) +
  geom_violin()

# Plot index values for genetic variables
stacked_index_culgen_MSB <- data.frame(values= matrix[matrix$seedbank=="1"  & matrix$cultivated=="1",]$"Genetic.div",
                                       ind= rep("Cultivated.Genetic", length(matrix[matrix$seedbank=="1"  & matrix$cultivated=="1",]$"Genetic.div")),
                                       seedbank= rep("MSB", length(matrix[matrix$seedbank=="1"  & matrix$cultivated=="1",]$"Genetic.div")))
stacked_index_nonculgen_MSB <- data.frame(values= matrix[matrix$seedbank=="1"  & matrix$cultivated=="0",]$"Genetic.div",
                                          ind= rep("NonCultivated.Genetic", length(matrix[matrix$seedbank=="1"  & matrix$cultivated=="0",]$"Genetic.div")),
                                          seedbank= rep("MSB", length(matrix[matrix$seedbank=="1"  & matrix$cultivated=="0",]$"Genetic.div")))
stacked_index_culgen_Partners <- data.frame(values= matrix[matrix$seedbank=="0"  & matrix$cultivated=="1",]$"Genetic.div",
                                            ind= rep("Cultivated.Genetic", length(matrix[matrix$seedbank=="0"  & matrix$cultivated=="1",]$"Genetic.div")),
                                            seedbank= rep("Partners", length(matrix[matrix$seedbank=="0"  & matrix$cultivated=="1",]$"Genetic.div")))
stacked_index_nonculgen_Partners <- data.frame(values= matrix[matrix$seedbank=="0"  & matrix$cultivated=="0",]$"Genetic.div",
                                               ind= rep("NonCultivated.Genetic", length(matrix[matrix$seedbank=="0"  & matrix$cultivated=="0",]$"Genetic.div")),
                                               seedbank= rep("Partners", length(matrix[matrix$seedbank=="0"  & matrix$cultivated=="0",]$"Genetic.div")))



stacked_index_gen <- rbind(stacked_index_culgen_MSB, stacked_index_nonculgen_MSB, stacked_index_culgen_Partners, stacked_index_nonculgen_Partners)

ggplot(stacked_index_gen,aes(x=ind,y=values,fill=seedbank)) +
  geom_violin()


## Plot index values in histogram and groups them
tab_viab<- as.data.frame(table(matrix$seedbank, round(matrix$index_viab,2)))
ggplot(aes(y = Freq, x = Var2, fill = Var1), data = tab_viab[tab_viab$Freq>0,]) +
  geom_bar(stat="identity")+
  geom_text(data=tab_viab[tab_viab$Freq>0,], aes( y=Freq, x= Var2, label=Freq), size=3, position=position_stack(vjust=0.5))+
  theme_classic()+
  labs(x ="Viability data Index", y= "Number of accessions")+
  scale_fill_manual(name="Seedbank", labels=c("Partners", "MSB"), values = c("cornflowerblue", "darkorange"))+
  geom_vline(xintercept = 1.5, color="red")

tab_gen_noncul<- as.data.frame(table(matrix[matrix$cultivated==0,]$seedbank, round(matrix[matrix$cultivated==0,]$index_gene,2)))
ggplot(aes(y = Freq, x = Var2, fill = Var1), data = tab_gen_noncul[tab_gen_noncul$Freq>0,]) +
  geom_bar(stat="identity")+
  geom_text(data=tab_gen_noncul[tab_gen_noncul$Freq>0,], aes( y=Freq, x= Var2, label=Freq), size=3, position=position_stack(vjust=0.5))+
  theme_classic()+
  labs(x ="Genetic diversity data Index\nNon-cultivated", y= "Number of accessions")+
  scale_fill_manual(name="Seedbank", labels=c("Partners", "MSB"), values = c("cornflowerblue", "darkorange"))+
  geom_vline(xintercept = 3.5, color="red")


#Select only unique rows. For selecting species in the MSBP (no tth) -----
species_CR_MSBP<- distinct(matrix, species,.keep_all = T)[,c(1:5)]
dim(species_CR_MSBP) #384
dim(distinct(matrix, species_short,.keep_all = T)[,c(1:5)]) #417
# # write_xlsx(species_CR_MSBP, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Databases created/species_CR_MSBP_nothh.xlsx")

#### By species: Data quality -----------------

matrix <- as.data.table(matrix)
#Database for cultivated and non cultivated accessions
matrix_cul <- matrix[matrix$cultivated==1,]
matrix_noncul <- matrix[matrix$cultivated==0,]
#Database for MSB and Partners accessions
matrix_msb <- matrix[matrix$seedbank==1,]
matrix_msb_cul <- matrix_msb[matrix_msb$cultivated==1,]
matrix_msb_noncul <- matrix_msb[matrix_msb$cultivated==0,]
matrix_partners <- matrix[matrix$seedbank==0,]
matrix_partners_cul <- matrix_partners[matrix_partners$cultivated==1,]
matrix_partners_noncul <- matrix_partners[matrix_partners$cultivated==0,]

#select the first accession max value of index
#MSB-P
species_info<- matrix[matrix[, .I[which.max(index_info)], by=species]$V1]
species_viab<- matrix[matrix[, .I[which.max(index_viab)], by=species]$V1]
species_cul_gen<- matrix_cul[matrix_cul[, .I[which.max(index_gene)], by=species]$V1]
species_noncul_gen<- matrix_noncul[matrix_noncul[, .I[which.max(index_gene)], by=species]$V1]
species_tot <- matrix[matrix[, .I[which.max(index_total)], by=species]$V1]
#MSB
species_info_msb<- matrix_msb[matrix_msb[, .I[which.max(index_info)], by=species]$V1]
species_viab_msb<- matrix_msb[matrix_msb[, .I[which.max(index_viab)], by=species]$V1]
species_cul_gen_msb<- matrix_msb_cul[matrix_msb_cul[, .I[which.max(index_gene)], by=species]$V1]
species_noncul_gen_msb<- matrix_msb_noncul[matrix_msb_noncul[, .I[which.max(index_gene)], by=species]$V1]
species_tot_msb <- matrix_msb[matrix_msb[, .I[which.max(index_total)], by=species]$V1]
#Partners
species_info_partners<- matrix_partners[matrix_partners[, .I[which.max(index_info)], by=species]$V1]
species_viab_partners<- matrix_partners[matrix_partners[, .I[which.max(index_viab)], by=species]$V1]
species_cul_gen_partnes<- matrix_partners_cul[matrix_partners_cul[, .I[which.max(index_gene)], by=species]$V1]
species_noncul_gen_partners<- matrix_partners_noncul[matrix_partners_noncul[, .I[which.max(index_gene)], by=species]$V1]
species_tot_partners <- matrix_partners[matrix_partners[, .I[which.max(index_total)], by=species]$V1]

anti_join(species_tot_partners,species_tot_msb, by="species")
anti_join(species_tot_msb,species_tot_partners, by="species")

#Select all the accessions with max values of index
species_info2 <- matrix[matrix[, .I[index_info == max(index_info)], by=species]$V1]
species_viab2 <- matrix[matrix[, .I[index_viab == max(index_viab)], by=species]$V1]
species_cul_gen2 <- matrix_cul[matrix_cul[, .I[index_gene == max(index_gene)], by=species]$V1]
species_noncul_gen2 <- matrix_cul[matrix_cul[, .I[index_gene == max(index_gene)], by=species]$V1]

## Raw numbers for species----

## Information variables
#MSB-P
table(species_info$coordinates)
table(species_info$area)
table(species_info$country)
table(species_info$Taxa)
table(species_info$collectdate)
table(species_info$bankdate)
table(species_info$seedbank)
#MSB
table(species_info_msb$coordinates)
table(species_info_msb$area)
table(species_info_msb$country)
table(species_info_msb$Taxa)
table(species_info_msb$collectdate)
table(species_info_msb$bankdate)
# Partners
table(species_info_partners$coordinates)
table(species_info_partners$area)
table(species_info_partners$country)
table(species_info_partners$Taxa)
table(species_info_partners$collectdate)
table(species_info_partners$bankdate)

##Viability variables
#MSB-P
table(species_viab$currcount)
table(species_viab$adjcount)
table(species_viab$germ.test)
table(species_viab$germ.test.15)
#MSB
table(species_viab_msb$currcount)
table(species_viab_msb$adjcount)
table(species_viab_msb$germ.test)
table(species_viab_msb$germ.test.15)
#MSB
table(species_viab_partners$currcount)
table(species_viab_partners$adjcount)
table(species_viab_partners$germ.test)
table(species_viab_partners$germ.test.15)

##Genetic diversity variables
#MSB-P
table(species_cul_gen$generation)
table(species_cul_gen$isolation)
table(species_noncul_gen$plant.tot)
table(species_noncul_gen$plant.seed)
table(species_noncul_gen$plant.sampled)
#MSB
table(species_cul_gen_msb$generation)
table(species_cul_gen_msb$isolation)
table(species_noncul_gen_msb$plant.tot)
table(species_noncul_gen_msb$plant.seed)
table(species_noncul_gen_msb$plant.sampled)
#Partners
table(species_cul_gen_partnes$generation)
table(species_cul_gen_partnes$isolation)
table(species_noncul_gen_partners$plant.tot)
table(species_noncul_gen_partners$plant.seed)
table(species_noncul_gen_partners$plant.sampled)

## Create barplot for raw data: species------
#Info variables
stacked_species_info<- stack(species_info[,c("coordinates", "area", "country", "taxonomy", "collectdate")])
sp_info<-aggregate(stacked_species_info$values, by=list(variable=stacked_species_info$ind), FUN=sum, na.rm=T)
sp_info$seedbank <- rep("MSB-P", dim(sp_info)[1])

stacked_species_info_msb<- stack(species_info_msb[,c("coordinates", "area", "country", "taxonomy", "collectdate")])
sp_info_msb<-aggregate(stacked_species_info_msb$values, by=list(variable=stacked_species_info_msb$ind), FUN=sum, na.rm=T)
sp_info_msb$seedbank <- rep("MSB", dim(sp_info_msb)[1])

stacked_species_info_partners<- stack(species_info_partners[,c("coordinates", "area", "country", "taxonomy", "collectdate")])
sp_info_partners<-aggregate(stacked_species_info_partners$values, by=list(variable=stacked_species_info_partners$ind), FUN=sum, na.rm=T)
sp_info_partners$seedbank <- rep("Partners", dim(sp_info_partners)[1])

sp_info_variables<-rbind(sp_info, sp_info_msb, sp_info_partners)

#Viability variables
stacked_species_viab<- stack(species_viab[,c("currcount","adjcount", "germ.test", "germ.test.15")])
sp_viab<-aggregate(stacked_species_viab$values, by=list(variable=stacked_species_viab$ind), FUN=sum, na.rm=T)
sp_viab$seedbank <- rep("MSB-P", dim(sp_viab)[1])

stacked_species_viab_msb<- stack(species_viab_msb[,c("currcount","adjcount", "germ.test", "germ.test.15")])
sp_viab_msb<-aggregate(stacked_species_viab_msb$values, by=list(variable=stacked_species_viab_msb$ind), FUN=sum, na.rm=T)
sp_viab_msb$seedbank <- rep("MSB", dim(sp_viab_msb)[1])

stacked_species_viab_partners<- stack(species_viab_partners[,c("currcount","adjcount", "germ.test", "germ.test.15")])
sp_viab_partners<-aggregate(stacked_species_viab_partners$values, by=list(variable=stacked_species_viab_partners$ind), FUN=sum, na.rm=T)
sp_viab_partners$seedbank <- rep("Partners", dim(sp_viab_partners)[1])

sp_viab_variables<-rbind(sp_viab, sp_viab_msb, sp_viab_partners)

#Genetic diversity variables
stacked_species_cul<- stack(species_cul_gen[,c("generation", "isolation")])
stacked_species_noncul<- stack(species_noncul_gen[,c("plant.tot","plant.seed", "plant.sampled")])
stacked_species_gene <- rbind(stacked_species_cul, stacked_species_noncul)
sp_gene<-aggregate(stacked_species_gene$values, by=list(variable=stacked_species_gene$ind), FUN=sum, na.rm=T)
sp_gene$seedbank <- rep("MSB-P", dim(sp_gene)[1])

stacked_species_cul_msb<- stack(species_cul_gen_msb[,c("generation", "isolation")])
stacked_species_noncul_msb<- stack(species_noncul_gen_msb[,c("plant.tot","plant.seed", "plant.sampled")])
stacked_species_gene_msb <- rbind(stacked_species_cul_msb, stacked_species_noncul_msb)
sp_gene_msb<-aggregate(stacked_species_gene_msb$values, by=list(variable=stacked_species_gene_msb$ind), FUN=sum, na.rm=T)
sp_gene_msb$seedbank <- rep("MSB", dim(sp_gene_msb)[1])

stacked_species_cul_partners<- stack(species_cul_gen_partnes[,c("generation", "isolation")])
stacked_species_noncul_partners<- stack(species_noncul_gen_partners[,c("plant.tot","plant.seed", "plant.sampled")])
stacked_species_gene_partners <- rbind(stacked_species_cul_partners, stacked_species_noncul_partners)
sp_gene_partners<-aggregate(stacked_species_gene_partners$values, by=list(variable=stacked_species_gene_partners$ind), FUN=sum, na.rm=T)
sp_gene_partners$seedbank <- rep("Partners", dim(sp_gene_partners)[1])

sp_gene_variables<-rbind(sp_gene, sp_gene_msb, sp_gene_partners)

#Create barplot

sp_info_gg<- ggplot(sp_info_variables, aes(fill=seedbank, y=x, x=variable)) +
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c("firebrick2","rosybrown","palegreen4"))+
  theme_classic()+
  theme(legend.position = "none",
        text = element_text(size = 15))+
  labs(x ="Information variables", y= "Number of species")+
  scale_x_discrete(labels=c("coordinates"="Coordinates","area"="Area", "country"="Country",
                            "taxonomy"="Taxonomic\nverification", "collectdate"="Collection year"))

sp_viab_gg<- ggplot(sp_viab_variables, aes(fill=seedbank, y=x, x=variable)) +
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c("firebrick2","rosybrown","palegreen4"))+
  theme_classic()+
  theme(legend.position = "none",
        text = element_text(size = 15))+
  labs(x ="Viability variables", y= "Number of species")+
  scale_x_discrete(labels=c("currcount"="Current count","adjcount"="Adjusted count", "germ.test"="Germination\ntest",
                            "germ.test.15"="Recent germination\ntest"))+
  ylim(0,320)

sp_gen_gg<- ggplot(sp_gene_variables, aes(fill=seedbank, y=x, x=variable)) +
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c("firebrick2","rosybrown","palegreen4"))+
  theme_classic()+
  theme(legend.title = element_blank(),
        text = element_text(size = 15))+
  labs(x ="Genetic diversity variables", y= "Number of species")+
  scale_x_discrete(labels=c("cultivated"="Cultivated","generation"="Relation to\nparent","isolation"="Isolation\nmethod",
                            "non.cultivated"="Non\ncultivated","plant.tot"="Total plants", "plant.seed"="Plants with\nseeds", "plant.sampled"="Sampled\nplants"))+
  ylim(0,320)

ggarrange(sp_info_gg,sp_viab_gg,sp_gen_gg,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


## Plot index values:species--------
#Prepare data to plot
sp_index <- data.frame(value=c(species_info$index_info, species_viab$index_viab, species_cul_gen$index_gene, species_noncul_gen$index_gene,
                               species_info_msb$index_info, species_viab_msb$index_viab, species_cul_gen_msb$index_gene, species_noncul_gen_msb$index_gene,
                               species_info_partners$index_info, species_viab_partners$index_viab, species_cul_gen_partnes$index_gene, species_noncul_gen_partners$index_gene),
                       index= c(rep("index_info", dim(species_info)[1]),rep("index_viab", dim(species_viab)[1]),
                                rep("index_gen_cul", dim(species_cul_gen)[1]),rep("index_gen_noncul", dim(species_noncul_gen)[1]),
                                rep("index_info", dim(species_info_msb)[1]),rep("index_viab", dim(species_viab_msb)[1]),
                                rep("index_gen_cul", dim(species_cul_gen_msb)[1]),rep("index_gen_noncul", dim(species_noncul_gen_msb)[1]),
                                rep("index_info", dim(species_info_partners)[1]),rep("index_viab", dim(species_viab_partners)[1]),
                                rep("index_gen_cul", dim(species_cul_gen_partnes)[1]),rep("index_gen_noncul", dim(species_noncul_gen_partners)[1])),
                       seedbank=c(rep("MSB-P", dim(species_info)[1]),rep("MSB-P", dim(species_viab)[1]),
                                  rep("MSB-P", dim(species_cul_gen)[1]),rep("MSB-P", dim(species_noncul_gen)[1]),
                                  rep("MSB", dim(species_info_msb)[1]),rep("MSB", dim(species_viab_msb)[1]),
                                  rep("MSB", dim(species_cul_gen_msb)[1]),rep("MSB", dim(species_noncul_gen_msb)[1]),
                                  rep("Partners", dim(species_info_partners)[1]),rep("Partners", dim(species_viab_partners)[1]),
                                  rep("Partners", dim(species_cul_gen_partnes)[1]),rep("Partners", dim(species_noncul_gen_partners)[1])))

#Plot the index using violin boxplots

ggplot(sp_index,aes(x=index,y=value,fill=seedbank)) +
  geom_violin()


# Add extra space to right of plot area
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

vioplot(value ~ seedbank:index,data=sp_index,
        col=c(rep(c("firebrick2","rosybrown","palegreen4"),4)),
        ylab="Index", xlab="",xaxt="n",
        at=c(1.5,2,2.5,3.5,4,4.5,5.5,6,6.5,7.5,8,8.5)) #for spacing between groups

axis(side=1, at=c(2,4,6,8),labels=c("Genetic diversity\nCultivated", "Genetic diversity\nNon-cultivated","Information", "Viability"))

legend("topright", inset=c(-0.125,0), legend = c("MSB","MSB-P","Partners"), fill = c("firebrick2","rosybrown","palegreen4"), cex = 1)

par(mar=c(5.1, 4.1, 4.1, 4.1), xpd=TRUE)

#Descriptive results
summary(sp_index[sp_index$index=="index_info" & sp_index$seedbank=="MSB-P",])
summary(sp_index[sp_index$index=="index_info" & sp_index$seedbank=="MSB",])
summary(sp_index[sp_index$index=="index_info" & sp_index$seedbank=="Partners",])

summary(sp_index[sp_index$index=="index_viab" & sp_index$seedbank=="MSB-P",])
summary(sp_index[sp_index$index=="index_viab" & sp_index$seedbank=="MSB",])
summary(sp_index[sp_index$index=="index_viab" & sp_index$seedbank=="Partners",])

summary(sp_index[sp_index$index=="index_gen_cul" & sp_index$seedbank=="MSB-P",])
summary(sp_index[sp_index$index=="index_gen_cul" & sp_index$seedbank=="MSB",])

summary(sp_index[sp_index$index=="index_gen_noncul" & sp_index$seedbank=="MSB-P",])
summary(sp_index[sp_index$index=="index_gen_noncul" & sp_index$seedbank=="MSB",])
summary(sp_index[sp_index$index=="index_gen_noncul" & sp_index$seedbank=="Partners",])

######## Biological quality ----------------

## Create dataframe -------

## Groups for seed/spores count
# No seed: 0
# Below banking standards: 1-249
# Over Banking standards but Below restoration standards: 250-1049
# Over restoration standards: > 1050

## Groups for sampled and total plants:
# Group a: 1
# Group b: 2-10
# Group c: 11-20
# Group d: 21-50
# Group e: 51-100
# Group f: 101-200
# Group g: 201-500
# Group h: 501-1000
# Group i: 1001-2000
# Group j: 2001-5000
# Group k: 5001-10000
# Group l: > 10001

matrix$PLANTSAMPLED <- as.numeric(as.character(matrix$PLANTSAMPLED))
matrix$PLANTTOTAL <- as.numeric(as.character(matrix$PLANTTOTAL))

bio_matrix<- data.frame(ID=matrix$ID,
                        ACCESSION=matrix$ACCESSION,
                        species=matrix$species,
                        FAMILY=matrix$FAMILY,
                        group=matrix$group,
                        seedbank=matrix$seedbank,
                        SEEDUPS= matrix$SEEDUPS,
                        COORDINATES= matrix$COORDINATES,
                        COUNTRY= matrix$COUNTRY,
                        index_info= matrix$index_info,
                        index_viab= matrix$index_viab,
                        index_gene= matrix$index_gene,
                        index_tot= matrix$index_total,
                        CURRCOUNT= matrix$CURRCOUNT,
                        currcount=matrix$currcount,
                        currcount_groups= ifelse(matrix$currcount==1 & matrix$CURRCOUNT==0, "a.no seeds",
                                                 ifelse(matrix$CURRCOUNT>=1 & matrix$CURRCOUNT<=249,"b.Below banking standards",
                                                        ifelse(matrix$CURRCOUNT>=250 & matrix$CURRCOUNT<=1049, "c.Over banking standards, below restoration standards",
                                                               ifelse(matrix$CURRCOUNT>=1050, "d.Over restoration standards", NA)))),
                        currcount_2= ifelse(matrix$CURRCOUNT>=5000, 5000, matrix$CURRCOUNT),
                        ADJSTCOUNT=matrix$ADJSTCOUNT,
                        adjcount=matrix$adjcount,
                        adjstcount_groups= ifelse(matrix$adjcount==1 & matrix$ADJSTCOUNT==0, "a.no seeds",
                                                  ifelse(matrix$ADJSTCOUNT>=1 & matrix$ADJSTCOUNT<=249,"b.Below banking standards",
                                                         ifelse(matrix$ADJSTCOUNT>=250 & matrix$ADJSTCOUNT<=1049, "c.Over banking standards, below restoration standards",
                                                                ifelse(matrix$ADJSTCOUNT>=1050, "d.Over restoration standards", NA)))),
                        adjstcount_2=ifelse(matrix$ADJSTCOUNT>=5000,5000, matrix$ADJSTCOUNT),
                        LASTTESTYEAR= matrix$LASTTESTYEAR,
                        BESTLAST=matrix$BESTLAST,
                        germiantion.75= ifelse(matrix$BESTLAST>=75,">75%","<75%"),
                        cultivated=matrix$cultivated,
                        generation=matrix$generation,
                        GENERATION = matrix$GENERATION,
                        isolation= matrix$isolation,
                        ISOLATION= matrix$ISOLATION,
                        PLANTHARVESTED= matrix$PLANTHARVESTD,
                        plant.seed=matrix$plant.seed,
                        plant.seed.pr=matrix$PLANTSEED_pr,
                        plant.sampled=matrix$plant.sampled,
                        plant.sampled.n= matrix$PLANTSAMPLED,
                        plant.tot=matrix$plant.tot,
                        plant.tot.n= matrix$PLANTTOTAL,
                        plant.sampled.groups= ifelse(matrix$PLANTSAMPLED==1,"a.1",
                                                     ifelse(matrix$PLANTSAMPLED>=1 & matrix$PLANTSAMPLED<=10, "b.2-10",
                                                            ifelse(matrix$PLANTSAMPLED>=11 & matrix$PLANTSAMPLED<=20, "c.11-20",
                                                                   ifelse(matrix$PLANTSAMPLED>=21 & matrix$PLANTSAMPLED<=50, "d.21-50",
                                                                          ifelse(matrix$PLANTSAMPLED>=51 & matrix$PLANTSAMPLED<=100, "e.51-100",
                                                                                 ifelse(matrix$PLANTSAMPLED>=101 & matrix$PLANTSAMPLED<=200, "f.101-200",
                                                                                        ifelse(matrix$PLANTSAMPLED>=201 & matrix$PLANTSAMPLED<=500, "g.201-500",NA))))))),
                        plant.tot.groups= ifelse(matrix$PLANTTOTAL==1,"a.1",
                                                 ifelse(matrix$PLANTTOTAL>=1 & matrix$PLANTTOTAL<=10, "b.2-10",
                                                        ifelse(matrix$PLANTTOTAL>=11 & matrix$PLANTTOTAL<=20, "c.11-20",
                                                               ifelse(matrix$PLANTTOTAL>=21 & matrix$PLANTTOTAL<=50, "d.21-50",
                                                                      ifelse(matrix$PLANTTOTAL>=51 & matrix$PLANTTOTAL<=100, "e.51-100",
                                                                             ifelse(matrix$PLANTTOTAL>=101 & matrix$PLANTTOTAL<=200, "f.101-200",
                                                                                    ifelse(matrix$PLANTTOTAL>=201 & matrix$PLANTTOTAL<=500, "g.201-500",
                                                                                           ifelse(matrix$PLANTTOTAL>=501 & matrix$PLANTTOTAL<=1000,"h.501-1000",
                                                                                                  ifelse(matrix$PLANTTOTAL>=1001 & matrix$PLANTTOTAL<=2000,"i.1001-2000",
                                                                                                         ifelse(matrix$PLANTTOTAL>=2001 & matrix$PLANTTOTAL<=5000,"j.2001-5000",
                                                                                                                ifelse(matrix$PLANTTOTAL>=5001 & matrix$PLANTTOTAL<=10000,"k.5001-10000",
                                                                                                                       ifelse(matrix$PLANTTOTAL>=10001,"l.>10001",NA)))))))))))))



## Exploratory graphs: Accessions biological quality ------------
## Current count

hist(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT, breaks=50, # MSB: between 1 and 5000
     main="MSB",xlab="Current seed/spores count (1-5000)", ylab="Number of accessions",
     col="darkorange",
     xaxt="n")
axis(side=1, at=seq(min(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT),max(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT),500),
     labs=seq(min(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT),max(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT),500))

hist(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT, breaks=50, # MSB: >5001
     main="MSB",xlab="Current seed/spores count (>5000)", ylab="Number of accessions",
     col="darkorange",
     xaxt="n")
axis(side=1, at=seq(min(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT),max(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT),20000),
     labs=seq(min(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT),max(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT),20000))



hist(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT, breaks=50, # Partners: between 1 and 5000
     main="Partners",xlab="Current seed/spores count (1-5000)", ylab="Number of accessions",
     col="cornflowerblue",
     xaxt="n")
axis(side=1, at=seq(min(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT),max(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT),500),
     labs=seq(min(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT),max(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT<=5000,]$CURRCOUNT),500))


hist(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT, breaks=50, # Partners: >5000
     main="Partners",xlab="Current seed/spores count (>5000)", ylab="Number of accessions",
     col="cornflowerblue",
     xaxt="n")
axis(side=1, at=seq(min(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT),max(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT),500),
     labs=seq(min(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT),max(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0 & bio_matrix$CURRCOUNT>=5000,]$CURRCOUNT),500))


boxplot(currcount_2~seedbank, data=bio_matrix[bio_matrix$currcount==1,],
        xlab="", ylab="Current seed/spores count", names=c("Partners", "MSB"),
        col=c("cornflowerblue","darkorange"))

# MSB-P
ggplot(data.frame(bio_matrix), aes(x=currcount_groups)) +
  geom_bar(aes(fill=as.factor(seedbank)))+
  theme_classic()+
  scale_fill_manual(name = "", labels = c("Partners", "MSB"), values = c("cornflowerblue","darkorange"))+
  labs(x ="Current seed/spore count", y= "Number of accessions")+
  scale_x_discrete(labels=c("b.Below banking standards" = "1-249",
                            "c.Over banking standards, below restoration standards" = "250-1049",
                            "d.Over restoration standards" = "> 1050"))+
  geom_vline(xintercept = c(4.5,6.5, 7.5), color=c("black", "black", "red"))

table(bio_matrix$currcount_groups)

#MSB
ggplot(data.frame(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1,]), aes(x=currcount_groups)) +
  geom_bar(fill="darkorange")+
  theme_classic()+
  labs(x ="Current seed/spore count", y= "Number of accessions")+
  scale_x_discrete(labels=c("b.Below banking standards" = "1-249",
                            "c.Over banking standards, below restoration standards" = "250-1049",
                            "d.Over restoration standards" = "> 1050"))+
  geom_vline(xintercept = c(4.5,6.5))

table(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==1,]$currcount_groups)

#Partners
ggplot(data.frame(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0,]), aes(x=currcount_groups)) +
  geom_bar(fill="cornflowerblue")+
  theme_classic()+
  labs(x ="Current seed/spore count", y= "Number of accessions")+
  scale_x_discrete(labels=c("b.Below banking standards" = "1-249",
                            "c.Over banking standards, below restoration standards" = "250-1049",
                            "d.Over restoration standards" = "> 1050"))+
  geom_vline(xintercept = c(4.5,6.5))

table(bio_matrix[bio_matrix$currcount==1 & bio_matrix$seedbank==0,]$currcount_groups)


## Adjusted count

hist(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT, breaks=50, # MSB: between 1 and 5000
     main="MSB",xlab="Adjusted seed/spores count (1-5000)", ylab="Number of accessions",
     col="darkorange",
     xaxt="n")
axis(side=1, at=seq(min(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT),max(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT),500),
     labs=seq(min(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT),max(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT),500))

hist(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT, breaks=50, # MSB: >5001
     main="MSB",xlab="Adjusted seed/spores count (>5000)", ylab="Number of accessions",
     col="darkorange",
     xaxt="n")
axis(side=1, at=seq(min(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT),max(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT),20000),
     labs=seq(min(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT),max(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT),20000))



hist(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT, breaks=50, # Partners: between 1 and 5000
     main="Partners",xlab="Adjusted seed/spores count (1-5000)", ylab="Number of accessions",
     col="cornflowerblue",
     xaxt="n")
axis(side=1, at=seq(min(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT),max(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT),500),
     labs=seq(min(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT),max(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT<=5000,]$ADJSTCOUNT),500))

hist(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT, breaks=50, # Partners: >5001
     main="Partners",xlab="Adjusted seed/spores count (>5000)", ylab="Number of accessions",
     col="cornflowerblue",
     xaxt="n")
axis(side=1, at=seq(min(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT),max(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT),5000),
     labs=seq(min(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT),max(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0 & bio_matrix$ADJSTCOUNT>=5000,]$ADJSTCOUNT),5000))


boxplot(adjstcount_2~seedbank, data=bio_matrix[bio_matrix$adjcount==1,],
        xlab="", ylab="Adjusted seed/spores count", names=c("Partners", "MSB"),
        col=c("cornflowerblue","darkorange"))


# MSB-P
ggplot(data.frame(bio_matrix), aes(x=adjstcount_groups)) +
  geom_bar(aes(fill=as.factor(seedbank)))+
  theme_classic()+
  scale_fill_manual(name = "", labels = c("Partners", "MSB"), values = c("cornflowerblue","darkorange"))+
  labs(x ="Adjusted seed/spore count", y= "Number of accessions")+
  scale_x_discrete(labels=c("b.Below banking standards" = "1-249",
                            "c.Over banking standards, below restoration standards" = "250-1049",
                            "d.Over restoration standards" = "> 1050"))+
  geom_vline(xintercept = c(4.5,6.5, 7.5), color=c("black","black","red"))

table(bio_matrix$adjstcount_groups)

#MSB
ggplot(data.frame(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1,]), aes(x=adjstcount_groups)) +
  geom_bar(fill="darkorange")+
  theme_classic()+
  labs(x ="Adjusted seed/spore count", y= "Number of accessions")+
  scale_x_discrete(labels=c("b.Below banking standards" = "1-249",
                            "c.Over banking standards, below restoration standards" = "250-1049",
                            "d.Over restoration standards" = "> 1050"))+
  geom_vline(xintercept = c(4.5,6.5))

table(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==1,]$adjstcount_groups)

#Partners
ggplot(data.frame(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0,]), aes(x=adjstcount_groups)) +
  geom_bar(fill="cornflowerblue")+
  theme_classic()+
  labs(x ="Adjusted seed/spore count", y= "Number of accessions")+
  scale_x_discrete(labels=c("b.Below banking standards" = "1-249",
                            "c.Over banking standards, below restoration standards" = "250-1049",
                            "d.Over restoration standards" = "> 1050"))+
  geom_vline(xintercept = c(0.5,2.5))

table(bio_matrix[bio_matrix$adjcount==1 & bio_matrix$seedbank==0,]$adjstcount_groups)


## Germination test

hist(bio_matrix[bio_matrix$seedbank==1 & !is.na(bio_matrix$LASTTESTYEAR),]$BESTLAST, breaks=20,
     main="MSB", xlab="Germination (%)", ylab="Number of accessions",
     col="Darkorange")
abline(v=75)

table(bio_matrix[!is.na(bio_matrix$LASTTESTYEAR) & bio_matrix$seedbank==1,]$germiantion.75)

hist(bio_matrix[bio_matrix$seedbank==0 & !is.na(bio_matrix$LASTTESTYEAR),]$BESTLAST, breaks=20,
     main="Partners", xlab="Germination (%)", ylab="Number of accessions",
     col="cornflowerblue")
abline(v=75)

table(bio_matrix[!is.na(bio_matrix$LASTTESTYEAR) & bio_matrix$seedbank==0,]$germiantion.75)


boxplot(BESTLAST~seedbank, data=bio_matrix[!is.na(bio_matrix$LASTTESTYEAR),],
        xlab="", ylab="Germination (%)", names=c("Partners", "MSB"),
        col=c("cornflowerblue","darkorange"))

boxplot(BESTLAST ~ germiantion.75+seedbank, data=bio_matrix[!is.na(bio_matrix$LASTTESTYEAR),],
        xlab="", ylab="Germination (%)", names=c("Partners <75%","Partners >75%", "MSB <75%", "MSB >75%"),
        col=c("cornflowerblue","cornflowerblue","darkorange","darkorange"))


ggplot(data.frame(bio_matrix[!is.na(bio_matrix$LASTTESTYEAR),]), aes(x=germiantion.75)) +
  geom_bar(aes(fill=as.factor(seedbank)))+
  theme_classic()+
  scale_fill_manual(name = "", labels = c("Partners", "MSB"), values = c("cornflowerblue","darkorange"))+
  labs(x ="Germination (%)", y= "Number of accessions")

## Exploratory graphs: Accession genetic diversity quality ------------

## Cultivated

# Generation
ggplot(data.frame(bio_matrix[bio_matrix$cultivated==1 & bio_matrix$generation==1 & bio_matrix$seedbank==1,]), aes(x=GENERATION)) +
  geom_bar(fill="darkorange")+
  geom_text(stat="count", aes(label=..count..), vjust=0.5)+
  theme_classic()+
  labs(x ="Relation to parent", y= "Number of accessions")+
  scale_x_discrete(labels=c("GENERATION 0 (EQUIVALENT GEN. TO 'PARENTAL COLLECTION' SEED)"="GENERATION 0"))

#Plants harvested
bio_matrix[bio_matrix$cultivated==1 & bio_matrix$generation==1 & bio_matrix$seedbank==1 & bio_matrix$GENERATION=="GENERATION 0 (EQUIVALENT GEN. TO 'PARENTAL COLLECTION' SEED)",]$PLANTHARVESTED

ggplot(data.frame(bio_matrix[bio_matrix$cultivated==1 & bio_matrix$generation==1 & bio_matrix$seedbank==1 & bio_matrix$GENERATION=="GENERATION 0 (EQUIVALENT GEN. TO 'PARENTAL COLLECTION' SEED)",]),
       aes(x=as.factor(PLANTHARVESTED))) +
  geom_bar(fill="darkorange4")+
  geom_text(stat="count", aes(label=..count..), vjust=2.5, color="white", size=5)+
  theme_classic()+
  labs(x ="Plants harvested", y= "Number of accessions")

#group by isolation types
bio_matrix$isolation.types <- ifelse(bio_matrix$ISOLATION=="Isolation techniques employed but not specified" | bio_matrix$ISOLATION=="Unknown", "No info",
                                     ifelse(bio_matrix$ISOLATION=="None. Assume open pollinated.", "Not applied",
                                            ifelse(bio_matrix$ISOLATION=="Isolation unnecessary - only member of genus regen. prog."| bio_matrix$ISOLATION=="Physical barrier to insects attempted" | bio_matrix$ISOLATION=="Physical pollen barrier" | bio_matrix$ISOLATION=="Spatial separation attempted","Isolation type",NA)))

bio_matrix$ISOLATION <- factor(bio_matrix$ISOLATION , levels= c("Physical barrier to insects attempted","Physical pollen barrier","Spatial separation attempted","Isolation unnecessary - only member of genus regen. prog.",
                                                                "Isolation techniques employed but not specified" ,"Unknown",
                                                                "None. Assume open pollinated."))

ggplot(data.frame(bio_matrix[bio_matrix$cultivated==1 & bio_matrix$isolation==1 & bio_matrix$seedbank==1,]), aes(x=isolation.types, fill=as.factor(ISOLATION))) +
  geom_bar(stat="count")+
  geom_text(stat="count", position=position_stack(0.5), aes(label=..count..), color="black", size=5)+
  theme_classic()+
  labs(x ="", y= "Number of accessions")+
  scale_fill_manual(name="Isolation technique",
                    labels=c("Isolation techniques employed but not specified"="Not specified",
                             "Isolation unnecessary - only member of genus regen. prog."="Isolation unnecessary",
                             "None. Assume open pollinated."="None",
                             "Physical barrier to insects attempted"="Physical barrier to\ninsects attempted",
                             "Physical pollen barrier"="Physical pollen\nbarrier",
                             "Spatial separation attempted"="Spatial separation\nattempted"),
                    values=c("darkolivegreen1", "darkolivegreen3","darkolivegreen4","darkgreen",
                             "firebrick1","firebrick4",
                             "darkorange"))



## Non-cultivated

#Plants sampled
hist(bio_matrix[bio_matrix$plant.sampled==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==1,]$plant.sampled.n, breaks=50,
     main="MSB",xlab="Plants sampled", ylab="Number of accessions",
     col="darkorange")

ggplot(bio_matrix[bio_matrix$plant.sampled==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==1,], aes(x=plant.sampled.groups)) +
  geom_bar(fill="darkorange")+
  geom_text(stat="count", aes(label=..count..), vjust=1)+
  theme_classic()+
  labs(x ="Plants sampled", y= "Number of accessions")+
  scale_x_discrete(labels=c("a.1"="1",
                            "b.2-10"="2-10",
                            "c.11-20"="11-20",
                            "d.21-50"="21-50",
                            "e.51-100"="51-100",
                            "f.201-500"="201-500",
                            "g.201-500"="201-500"))

hist(bio_matrix[bio_matrix$plant.sampled==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==0,]$plant.sampled.n, breaks=50,
     main="Partners",xlab="Plants sampled", ylab="Number of accessions",
     col="cornflowerblue")

ggplot(bio_matrix[bio_matrix$plant.sampled==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==0,], aes(x=plant.sampled.groups)) +
  geom_bar(fill="cornflowerblue")+
  geom_text(stat="count", aes(label=..count..), vjust=1)+
  theme_classic()+
  labs(x ="Plants sampled", y= "Number of accessions")+
  scale_x_discrete(labels=c("a.1"="1",
                            "b.2-10"="2-10",
                            "c.11-20"="11-20",
                            "d.21-50"="21-50",
                            "e.51-100"="51-100",
                            "f.201-500"="201-500",
                            "g.201-500"="201-500"))

#Plants total

hist(bio_matrix[bio_matrix$plant.tot==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==1,]$plant.tot.n, breaks=50,
     main="MSB",xlab="Total population", ylab="Number of accessions",
     col="darkorange")

ggplot(bio_matrix[bio_matrix$plant.tot==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==1,], aes(x=plant.tot.groups)) +
  geom_bar(fill="darkorange")+
  geom_text(stat="count", aes(label=..count..), vjust=1)+
  theme_classic()+
  labs(x ="Total population", y= "Number of accessions")+
  scale_x_discrete(labels=c("a.1"="1",
                            "b.2-10"="2-10",
                            "c.11-20"="11-20",
                            "d.21-50"="21-50",
                            "e.51-100"="51-100",
                            "f.201-500"="201-500",
                            "g.201-500"="201-500",
                            "h.501-1000"="501-1000",
                            "i.1001-2000"="1001-2000",
                            "j.2001-5000"="2001-5000",
                            "k.5001-10000"="5001-10000",
                            "l.>10001"=">10001"))

hist(bio_matrix[bio_matrix$plant.tot==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==0,]$plant.tot.n, breaks=50,
     main="Partners",xlab="Total population", ylab="Number of accessions",
     col="cornflowerblue")

ggplot(bio_matrix[bio_matrix$plant.tot==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==0,], aes(x=plant.tot.groups)) +
  geom_bar(fill="cornflowerblue")+
  geom_text(stat="count", aes(label=..count..), vjust=1)+
  theme_classic()+
  labs(x ="Total population", y= "Number of accessions")+
  scale_x_discrete(labels=c("a.1"="1",
                            "b.2-10"="2-10",
                            "c.11-20"="11-20",
                            "d.21-50"="21-50",
                            "e.51-100"="51-100",
                            "f.201-500"="201-500",
                            "g.201-500"="201-500",
                            "h.501-1000"="501-1000",
                            "i.1001-2000"="1001-2000",
                            "j.2001-5000"="2001-5000",
                            "k.5001-10000"="5001-10000",
                            "l.>10001"=">10001"))

#Plants seeds

hist(bio_matrix[bio_matrix$plant.seed==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==1,]$plant.seed.pr, breaks=50,
     main="MSB",xlab="Plans with seed (%)", ylab="Number of accessions",
     col="darkorange")

hist(bio_matrix[bio_matrix$plant.seed==1 & bio_matrix$cultivated==0 & bio_matrix$seedbank==0,]$plant.seed.pr, breaks=50,
     main="Partners",xlab="Total population", ylab="Number of accessions",
     col="cornflowerblue")



## Genetic diversity quality for accessions ------

#Calculate proportion of plants sampled from the total population
bio_matrix$plant.samp.pr.tot <- bio_matrix$plant.sampled.n * 100 / bio_matrix$plant.tot.n


#Calculate number of plants in seed
bio_matrix$plant.seed.n <- round(bio_matrix$plant.seed.pr * bio_matrix$plant.tot.n /100)

#Calculate proportion of plants sampled from plants in seed
bio_matrix$plant.samp.pr.seed <- bio_matrix$plant.sampled.n * 100 / bio_matrix$plant.seed.n

check_plant.tot <- bio_matrix[bio_matrix$plant.samp.pr.tot>=100.01 & !is.na(bio_matrix$plant.samp.pr.tot),]
check_plant.seed <- bio_matrix[bio_matrix$plant.samp.pr.seed>=100.01 & !is.na(bio_matrix$plant.samp.pr.seed),] #PCSeed should be 100%

check_plant <- rbind(check_plant.tot,check_plant.seed)
# # write_xlsx(check_plant, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse\\Accessions to check/check_planttotal&seeds.xlsx")

only_pcseed<- data[data$PCSEED!="" & data$PLANTSAMP=="" & data$PLANTTOTAL=="",]
# # write_xlsx(only_pcseed, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Databases\\Data Warehouse\\Accessions to check/only_pcseed.xlsx")


#Calculate the genetic standards
bio_matrix$standar_gene <- ifelse(is.na(bio_matrix$plant.sampled.n), NA,
                                  ifelse(bio_matrix$plant.sampled.n>=50, 1,
                                         ifelse(bio_matrix$plant.sampled.n<50 & bio_matrix$plant.samp.pr.tot>=100 | bio_matrix$plant.samp.pr.seed>=100, 1, 0)))

# # write_xlsx(bio_matrix, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Databases created/bio_matrix_15_10_21.xlsx")

#Create plots
plot(bio_matrix[bio_matrix$currcount_2>0 & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.sampled.n>0 & !is.na(bio_matrix$plant.sampled.n),]$currcount_2,
     bio_matrix[bio_matrix$currcount_2>0  & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.sampled.n>0 & !is.na(bio_matrix$plant.sampled.n),]$plant.sampled.n,
     xlab="Number of seeds", ylab="Plant sampled")
abline(h=100, v=1050, col="red")

dim(bio_matrix[bio_matrix$currcount_2>0 & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.sampled.n>0 & !is.na(bio_matrix$plant.sampled.n),])

plot(bio_matrix[bio_matrix$currcount_2>0 & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.samp.pr.tot>0 & bio_matrix$plant.samp.pr.tot<110 & !is.na(bio_matrix$plant.samp.pr.tot),]$currcount_2,
     bio_matrix[bio_matrix$currcount_2>0  & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.samp.pr.tot>0 & bio_matrix$plant.samp.pr.tot<110 & !is.na(bio_matrix$plant.samp.pr.tot),]$plant.samp.pr.tot,
     xlab="Current count", ylab="Proportion of plant sampled from Total plants")
abline(v=c(250,1050), col="red")

dim(bio_matrix[bio_matrix$currcount_2>0 & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.samp.pr.tot>0 & bio_matrix$plant.samp.pr.tot<110 & !is.na(bio_matrix$plant.samp.pr.tot),])

plot(bio_matrix[bio_matrix$currcount_2>0 & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.samp.pr.tot<1000 & bio_matrix$plant.samp.pr.seed>0 & bio_matrix$plant.samp.pr.seed<200 & !is.na(bio_matrix$plant.samp.pr.seed),]$currcount_2,
     bio_matrix[bio_matrix$currcount_2>0  & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.samp.pr.tot<1000 & bio_matrix$plant.samp.pr.seed>0 & bio_matrix$plant.samp.pr.seed<200 & !is.na(bio_matrix$plant.samp.pr.seed),]$plant.samp.pr.seed,
     xlab="Current count", ylab="Proportion of plant sampled from plantswith seeds")
abline(v=c(250,1050), col="red")

dim(bio_matrix[bio_matrix$currcount_2>0 & !is.na(bio_matrix$currcount_2) & bio_matrix$plant.samp.pr.tot<1000 & bio_matrix$plant.samp.pr.seed>0 & bio_matrix$plant.samp.pr.seed<200 & !is.na(bio_matrix$plant.samp.pr.seed),])

## Species biological quality ------------

species_viab_MSBP<- rbind(species_viab_msb, species_viab_partners)

anti_join(species_viab_partners[species_viab_partners$currcount==1,],species_viab_msb[species_viab_msb$currcount==1,], by="species")
anti_join(species_viab_partners[species_viab_partners$adjcount==1,],species_viab_msb[species_viab_msb$adjcount==1,], by="species")
anti_join(species_viab_partners[species_viab_partners$germ.test==1,],species_viab_msb[species_viab_msb$germ.test==1,], by="species")

bio_species<- data.frame(ID=species_viab_MSBP$ID,
                         ACCESSION=species_viab_MSBP$ACCESSION,
                         species=species_viab_MSBP$species,
                         FAMILY=species_viab_MSBP$FAMILY,
                         group=species_viab_MSBP$group,
                         seedbank=species_viab_MSBP$seedbank,
                         SEEDUPS= species_viab_MSBP$SEEDUPS,
                         CURRCOUNT= species_viab_MSBP$CURRCOUNT,
                         currcount=species_viab_MSBP$currcount,
                         currcount_groups= ifelse(species_viab_MSBP$currcount==1 & species_viab_MSBP$CURRCOUNT==0, "0.no seeds",
                                                  ifelse(species_viab_MSBP$CURRCOUNT>=1 & species_viab_MSBP$CURRCOUNT<=10,"1.very few",
                                                         ifelse(species_viab_MSBP$CURRCOUNT>=11 & species_viab_MSBP$CURRCOUNT<=50, "2.few",
                                                                ifelse(species_viab_MSBP$CURRCOUNT>=51 & species_viab_MSBP$CURRCOUNT<=100, "3.not enough",
                                                                       ifelse(species_viab_MSBP$CURRCOUNT>=101 & species_viab_MSBP$CURRCOUNT<=250, "4.almost enough",
                                                                              ifelse(species_viab_MSBP$CURRCOUNT>=251 & species_viab_MSBP$CURRCOUNT<=500, "5.enough",
                                                                                     ifelse(species_viab_MSBP$CURRCOUNT>=501 & species_viab_MSBP$CURRCOUNT<=1000, "6.more than enough",
                                                                                            ifelse(species_viab_MSBP$CURRCOUNT>=1001 & species_viab_MSBP$CURRCOUNT<=2000, "7.enough restoration",
                                                                                                   ifelse(species_viab_MSBP$CURRCOUNT>=2001,"8.more than enough restoration",NA))))))))),
                         currcount_2= ifelse(species_viab_MSBP$CURRCOUNT>=5000, 5000, species_viab_MSBP$CURRCOUNT),
                         ADJSTCOUNT=species_viab_MSBP$ADJSTCOUNT,
                         adjcount=species_viab_MSBP$adjcount,
                         adjstcount_groups= ifelse(species_viab_MSBP$adjcount==1 &species_viab_MSBP$ADJSTCOUNT==0, "0.no seeds",
                                                   ifelse(species_viab_MSBP$ADJSTCOUNT>=1 & species_viab_MSBP$ADJSTCOUNT<=10,"1.very few",
                                                          ifelse(species_viab_MSBP$ADJSTCOUNT>=11 & species_viab_MSBP$ADJSTCOUNT<=50, "2.few",
                                                                 ifelse(species_viab_MSBP$ADJSTCOUNT>=51 & species_viab_MSBP$ADJSTCOUNT<=100, "3.not enough",
                                                                        ifelse(species_viab_MSBP$ADJSTCOUNT>=101 & species_viab_MSBP$ADJSTCOUNT<=250, "4.almost enough",
                                                                               ifelse(species_viab_MSBP$ADJSTCOUNT>=251 & species_viab_MSBP$ADJSTCOUNT<=500, "5.enough",
                                                                                      ifelse(species_viab_MSBP$ADJSTCOUNT>=501 & species_viab_MSBP$ADJSTCOUNT<=1000, "6.more than enough",
                                                                                             ifelse(species_viab_MSBP$ADJSTCOUNT>=1001 & species_viab_MSBP$ADJSTCOUNT<=2000, "7.enough restoration",
                                                                                                    ifelse(species_viab_MSBP$ADJSTCOUNT>=2001,"8.more than enough restoration",NA))))))))),
                         adjstcount_2=ifelse(species_viab_MSBP$ADJSTCOUNT>=5000,5000, species_viab_MSBP$ADJSTCOUNT),
                         LASTTESTYEAR= species_viab_MSBP$LASTTESTYEAR,
                         BESTLAST=species_viab_MSBP$BESTLAST)

## Current count

hist(bio_species[bio_species$currcount==1 & bio_species$seedbank==1,]$currcount_2, breaks=50,
     main="MSB", xlab="Current seed/spores count", ylab="Number of species",
     col="firebrick2")

hist(bio_species[bio_species$currcount==1 & bio_species$seedbank==0,]$currcount_2, breaks=50,
     main="Partners", xlab="Current seed/spores count", ylab="Number of species",
     col="palegreen4")

boxplot(currcount_2~seedbank, data=bio_species[bio_species$currcount==1,],
        xlab="", ylab="Current seed/spores count", names=c("Partners", "MSB"),
        col=c("palegreen4","firebrick2"))

#MSB
ggplot(data.frame(bio_species[bio_species$currcount==1 & bio_species$seedbank==1,]), aes(x=currcount_groups)) +
  geom_bar(fill="firebrick2")+
  theme_classic()+
  labs(x ="Current seed/spore count", y= "Number of species")+
  scale_x_discrete(labels=c("1.very few"="1-10","2.few"="11-50", "3.not enough"="51-100",
                            "4.almost enough"="101-250", "5.enough"="251-500", "6.more than enough"="501-1000",
                            "7.enough restoration"="1001-2000", "8.more than enough restoration"="> 2001"))

#Partners
ggplot(data.frame(bio_species[bio_species$currcount==1 & bio_species$seedbank==0,]), aes(x=currcount_groups)) +
  geom_bar(fill="palegreen4")+
  theme_classic()+
  labs(x ="Current seed/spore count", y= "Number of species")+
  scale_x_discrete(labels=c("1.very few"="1-10","2.few"="11-50", "3.not enough"="51-100",
                            "4.almost enough"="101-250", "5.enough"="251-500", "6.more than enough"="501-1000",
                            "7.enough restoration"="1001-2000", "8.more than enough restoration"="> 2001"))

## Adjusted count

hist(bio_species[bio_species$adjcount==1 & bio_species$seedbank==1,]$adjstcount_2, breaks=50,
     main="MSB", xlab="Adjusted seed/spores count", ylab="Number of species",
     col="firebrick2")

hist(bio_species[bio_species$adjcount==1 & bio_species$seedbank==0,]$adjstcount_2, breaks=50,
     main="Partners", xlab="Adjusted seed/spores count", ylab="Number of species",
     col="palegreen4")

boxplot(adjstcount_2~seedbank, data=bio_species[bio_species$adjcount==1,],
        xlab="", ylab="Adjusted seed/spores count", names=c("Partners", "MSB"),
        col=c("palegreen4","firebrick2"))

ggplot(data.frame(bio_species[bio_species$adjcount==1 & bio_species$seedbank==1,]), aes(x=adjstcount_groups)) +
  geom_bar(fill="firebrick2")+
  theme_classic()+
  labs(x ="Adjusted seed/spore count", y= "Number of species")+
  scale_x_discrete(labels=c("1.very few"="1-10","2.few"="11-50", "3.not enough"="51-100",
                            "4.almost enough"="101-250", "5.enough"="251-500", "6.more than enough"="501-1000",
                            "7.enough restoration"="1001-2000", "8.more than enough restoration"="> 2001"))

## Germination test

hist(bio_species[bio_species$seedbank==1 & !is.na(bio_species$LASTTESTYEAR),]$BESTLAST, breaks=20,
     main="MSB", xlab="Germination test", ylab="Number of species",
     col="firebrick2")

hist(bio_species[bio_species$seedbank==0 & !is.na(bio_species$LASTTESTYEAR),]$BESTLAST, breaks=20,
     main="Partners", xlab="Germination test", ylab="Number of species",
     col="palegreen4")

boxplot(BESTLAST~seedbank, data=bio_species[!is.na(bio_species$LASTTESTYEAR),],
        xlab="", ylab="Germination test", names=c("Partners", "MSB"),
        col=c("palegreen4","firebrick2"))


######## SPECIES LISTS########

#Number of species
species_msb <- bio_matrix[bio_matrix$seedbank==1,]
species_partners<- bio_matrix[bio_matrix$seedbank==0,]

unique_msb <- anti_join(species_msb, species_partners, by="species")
length(unique(unique_msb$species))
unique_partners <- anti_join(species_partners, species_msb, by="species")
length(unique(unique_partners$species))


## Viability-----
# Number of species with viability index >0.3 --> At least current count
species_ccount  <- bio_matrix[bio_matrix$index_viab>0.3,]
length(unique(species_ccount$species)) #291 species

# # write_xlsx(species_ccount, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/species_ccount.xlsx")


species_msb_ccount<- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$seedbank==1,] # in MSB
length(unique(species_msb_ccount$species)) #229 species
species_partners_ccount<- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$seedbank==0,] # in Partners
length(unique(species_partners_ccount$species)) #44 species

unique_msb_ccount<-anti_join(species_msb_ccount, species_partners_ccount, by="species")
length(unique(unique_msb_ccount$species)) #208 species
unique_partners_ccount<- anti_join(species_partners_ccount, species_msb_ccount, by="species")
length(unique(unique_partners_ccount$species)) #23 species

# # write_xlsx(unique_msb_ccount, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/unique_msb_ccount.xlsx")
# # write_xlsx(unique_partners_ccount, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/unique_partners_ccount.xlsx")


#Number of species with current count >=250
species_ccount_250<- species_ccount[species_ccount$CURRCOUNT>=250,]
length(unique(species_ccount_250$species)) #183 species

# # write_xlsx(species_ccount_250, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/species_ccount_250.xlsx")

species_msb_ccount_250<- species_ccount[species_ccount$CURRCOUNT>=250 & species_ccount$seedbank==1,]
length(unique(species_msb_ccount_250$species)) #162 species
species_partners_ccount_250<- species_ccount[species_ccount$CURRCOUNT>=250 & species_ccount$seedbank==0,]
length(unique(species_partners_ccount_250$species)) #30 species

unique_msb_ccount_250<-anti_join(species_msb_ccount_250, species_partners_ccount_250, by="species")
length(unique(unique_msb_ccount_250$species)) #153 species
unique_partners_ccount_250<- anti_join(species_partners_ccount_250, species_msb_ccount_250, by="species")
length(unique(unique_partners_ccount_250$species)) #23 species

# # write_xlsx(unique_msb_ccount_250, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/unique_msb_ccount_250.xlsx")
# # write_xlsx(unique_partners_ccount_250, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/unique_partners_ccount_250.xlsx")


#Number of species with current count >=1050
species_ccount_1050<- species_ccount[species_ccount$CURRCOUNT>=1050,]
length(unique(species_ccount_1050$species)) #129 species

# # write_xlsx(species_ccount_1050, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list//species_ccount_1050.xlsx")

species_msb_ccount_1050<- species_ccount[species_ccount$CURRCOUNT>=1050 & species_ccount$seedbank==1,]
length(unique(species_msb_ccount_1050$species)) #112 species
species_partners_ccount_1050<- species_ccount[species_ccount$CURRCOUNT>=1050 & species_ccount$seedbank==0,]
length(unique(species_partners_ccount_1050$species)) #20 species

unique_msb_ccount_1050<-anti_join(species_msb_ccount_1050, species_partners_ccount_1050, by="species")
length(unique(unique_msb_ccount_1050$species)) #109 species
unique_partners_ccount_1050<- anti_join(species_partners_ccount_1050, species_msb_ccount_1050, by="species")
length(unique(unique_partners_ccount_1050$species)) #17 species

# # write_xlsx(unique_msb_ccount_1050, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/unique_msb_ccount_1050.xlsx")
# # write_xlsx(unique_partners_ccount_1050, "C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/unique_partners_ccount_1050.xlsx")

### PLOT

# Create dataframe for the plot

species_plot<- data.frame(num_species= c(length(unique(species_ccount$species)), length(unique(species_ccount_250$species)),length(unique(species_ccount_1050$species)),
                                         length(unique(species_msb_ccount$species)), length(unique(species_msb_ccount_250$species)), length(unique(species_msb_ccount_1050$species)),
                                         length(unique(species_partners_ccount$species)), length(unique(unique_partners_ccount_250$species)), length(unique(unique_partners_ccount_1050$species))),
                          seedbank= c(rep("MSB-P",3), rep("MSB",3), rep("Partners",3)),
                          type= c("Current count", "> 250", "> 1050", "Current count", "> 250", "> 1050", "Current count", "> 250", "> 1050"))

# Change order of factors

species_plot$type <- factor(species_plot$type , levels= c("Current count", "> 250", "> 1050"))
species_plot$seedbank <- factor(species_plot$seedbank , levels= c("MSB-P", "MSB", "Partners"))

# plot
ggplot(species_plot, aes(x=type, y=num_species, fill=seedbank)) +
  geom_bar(position="dodge", stat="identity")+
  geom_text(position=position_dodge(0.9), aes(label=num_species), color="black", size=5, vjust=1.5)+
  theme_classic()+
  labs(x ="", y= "Number of species")+
  scale_fill_manual(values=c("darkmagenta", "darkorange", "cornflowerblue"))

## Genetic + Viability ------
# Number of species with viability index >0.3 & genetic index >0.6
species_ccount_gene  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$index_gene>0.6,]
length(unique(species_ccount_gene$species)) #182 species

species_ccount_gene2  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$standar_gene==1,]
length(unique(species_ccount_gene2$species)) #85 species

# # write_xlsx(species_ccount_gene2,"C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/species_ccount_gene.xlsx")

species_msb_ccount_gene  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$index_gene>0.6 & bio_matrix$seedbank==1,]
length(unique(species_msb_ccount_gene$species)) #174 species
species_partners_ccount_gene  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$index_gene>0.6 & bio_matrix$seedbank==0,]
length(unique(species_partners_ccount_gene$species)) # 8 species

species_msb_ccount_gene2  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$standar_gene==1 & bio_matrix$seedbank==1,]
length(unique(species_msb_ccount_gene2$species)) #77 species
species_partners_ccount_gene2  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$standar_gene==1 & bio_matrix$seedbank==0,]
length(unique(species_partners_ccount_gene2$species)) # 9 species

unique_msb_ccount_gene<-anti_join(species_msb_ccount_gene, species_partners_ccount_gene, by="species")
length(unique(unique_msb_ccount_gene$species)) #174 species
unique_partners_ccount_gene<- anti_join(species_partners_ccount_gene, species_msb_ccount_gene, by="species")
length(unique(unique_partners_ccount_gene$species)) #8 species

# Select accessions current count >250
species_ccount_gene_250  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$index_gene>0.6 & bio_matrix$CURRCOUNT>=250,]
length(unique(species_ccount_gene_250$species)) #125 species

species_ccount_gene2_250  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$standar_gene==1 & bio_matrix$CURRCOUNT>=250,]
length(unique(species_ccount_gene2_250$species)) #63 species

# # write_xlsx(species_ccount_gene2_250,"C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/species_ccount_gene_250.xlsx")


species_msb_ccount_gene_250 <- species_ccount_gene_250[species_ccount_gene_250$seedbank==1,]
length(unique(species_msb_ccount_gene_250$species))
species_partners_ccount_gene_250 <- species_ccount_gene_250[species_ccount_gene_250$seedbank==0,]
length(unique(species_partners_ccount_gene_250$species))

species_msb_ccount_gene2_250 <- species_ccount_gene2_250[species_ccount_gene2_250$seedbank==1,]
length(unique(species_msb_ccount_gene2_250$species)) #56
species_partners_ccount_gene2_250 <- species_ccount_gene2_250[species_ccount_gene2_250$seedbank==0,]
length(unique(species_partners_ccount_gene2_250$species)) # 8

# Select accessions current count >1050
species_ccount_gene_1050  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$index_gene>0.6 & bio_matrix$CURRCOUNT>=1050,]
length(unique(species_ccount_gene_1050$species)) #92 species

species_ccount_gene2_1050  <- bio_matrix[bio_matrix$index_viab>0.3 & bio_matrix$standar_gene==1 & bio_matrix$CURRCOUNT>=1050,]
length(unique(species_ccount_gene2_1050$species)) #48 species

# # write_xlsx(species_ccount_gene2_1050,"C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\Data Warehouse/Species list/species_ccount_gene_1050.xlsx")

species_msb_ccount_gene_1050 <- species_ccount_gene_1050[species_ccount_gene_1050$seedbank==1,]
length(unique(species_msb_ccount_gene_1050$species))
species_partners_ccount_gene_1050 <- species_ccount_gene_1050[species_ccount_gene_1050$seedbank==0,]
length(unique(species_partners_ccount_gene_1050$species))

species_msb_ccount_gene2_1050 <- species_ccount_gene2_1050[species_ccount_gene2_1050$seedbank==1,]
length(unique(species_msb_ccount_gene2_1050$species))
species_partners_ccount_gene2_1050 <- species_ccount_gene2_1050[species_ccount_gene2_1050$seedbank==0,]
length(unique(species_partners_ccount_gene2_1050$species))

# Create dataframe for the plot

species_plot2<- data.frame(num_species= c(length(unique(species_ccount_gene2$species)), length(unique(species_ccount_gene2_250$species)), length(unique(species_ccount_gene2_1050$species)),
                                          length(unique(species_msb_ccount_gene2$species)), length(unique(species_msb_ccount_gene2_250$species)), length(unique(species_msb_ccount_gene2_1050$species)),
                                          length(unique(species_partners_ccount_gene2$species)), length(unique(species_partners_ccount_gene2_250$species)), length(unique(species_partners_ccount_gene2_1050$species))),
                           seedbank= c(rep("MSB-P",3), rep("MSB",3), rep("Partners",3)),
                           type= c("Current count &\n Genetic standard", "> 250 &\n Genetic standard", "> 1050 &\n Genetic standard",
                                   "Current count &\n Genetic standard", "> 250 &\n Genetic standard", "> 1050 &\n Genetic standard",
                                   "Current count &\n Genetic standard", "> 250 &\n Genetic standard", "> 1050 &\n Genetic standard"))

# Change order of factors

species_plot2$type <- factor(species_plot2$type , levels= c("Current count &\n Genetic standard", "> 250 &\n Genetic standard", "> 1050 &\n Genetic standard"))
species_plot2$seedbank <- factor(species_plot2$seedbank , levels= c("MSB-P", "MSB", "Partners"))

# plot
ggplot(species_plot2, aes(x=type, y=num_species, fill=seedbank)) +
  geom_bar(position="dodge", stat="identity")+
  geom_text(position=position_dodge(0.9), aes(label=num_species), color="black", size=5, vjust=1.5)+
  theme_classic()+
  labs(x ="", y= "Number of species")+
  scale_fill_manual(values=c("darkmagenta", "darkorange", "cornflowerblue"))


#### Extras ------
#Sum all current count for same species
species_ccount_sum <- aggregate(species_ccount$CURRCOUNT, by=list(species= species_ccount$species), FUN= sum)
#Select species with current count >=250
species_ccount_sum_250<- species_ccount_sum[species_ccount_sum$x>=250,]
length(unique(species_ccount_sum_250$species)) #196 species
#Select species with current count >=1050
species_ccount_sum_1050<- species_ccount_sum[species_ccount_sum$x>=1050,]
length(unique(species_ccount_sum_1050$species)) #138 species

species_msb_ccount_sum <- aggregate(species_msb_ccount$CURRCOUNT, by=list(species= species_msb_ccount$species), FUN= sum)
species_msb_ccount_sum_250<- species_msb_ccount_sum[species_msb_ccount_sum$x>=250,]
length(unique(species_msb_ccount_sum_250$species)) #170 species
species_msb_ccount_sum_1050<- species_msb_ccount_sum[species_msb_ccount_sum$x>=1050,]
length(unique(species_msb_ccount_sum_1050$species)) #117 species

species_partners_ccount_sum <- aggregate(species_partners_ccount$CURRCOUNT, by=list(species= species_partners_ccount$species), FUN= sum)
species_partners_ccount_sum_250<- species_partners_ccount_sum[species_partners_ccount_sum$x>=250,]
length(unique(species_partners_ccount_sum_250$species)) #31 species
species_partners_ccount_sum_1050<- species_partners_ccount_sum[species_partners_ccount_sum$x>=1050,]
length(unique(species_partners_ccount_sum_1050$species)) #22 species

unique_msb_ccount_sum_250<-anti_join(species_msb_ccount_sum_250, species_partners_ccount_sum_250, by="species")
length(unique(unique_msb_ccount_sum_250$species)) #161 species
unique_partners_ccount_sum_250<- anti_join(species_partners_ccount_sum_250, species_msb_ccount_sum_250, by="species")
length(unique(unique_partners_ccount_sum_250$species)) #22 species

unique_msb_ccount_sum_1050<-anti_join(species_msb_ccount_sum_1050, species_partners_ccount_sum_1050, by="species")
length(unique(unique_msb_ccount_sum_1050$species)) #113 species
unique_partners_ccount_sum_1050<- anti_join(species_partners_ccount_sum_1050, species_msb_ccount_sum_1050, by="species")
length(unique(unique_partners_ccount_sum_1050$species)) #18 species


#Select the accession with max value of current count for each species
species_ccount <- as.data.table(species_ccount)
species_ccount_max<- species_ccount[species_ccount[, .I[CURRCOUNT == max(CURRCOUNT)], by=species]$V1]

species_msb_ccount <- as.data.table(species_msb_ccount)
species_msb_ccount_max<- species_msb_ccount[species_msb_ccount[, .I[CURRCOUNT == max(CURRCOUNT)], by=species]$V1]
table(species_msb_ccount_max$index_gene)

species_partners_ccount <- as.data.table(species_partners_ccount)
species_partners_ccount_max<- species_partners_ccount[species_partners_ccount[, .I[CURRCOUNT == max(CURRCOUNT)], by=species]$V1]
table(species_partners_ccount_max$index_gene)




