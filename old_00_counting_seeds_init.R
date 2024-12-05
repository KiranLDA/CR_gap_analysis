library(tidyverse)


dta = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/species_count.csv")
head(dta)

# how many species are in the dataset?
spp <- dta %>% count(species)
nrow(spp)

# Subset the bankable seeds
viable = dta[dta$seedbank ==1,]
nrow(viable) # how many accessions?
vspp <- viable %>% count(species)
nrow(vspp) # how many species


#proportion of viable collections that meet targets
props = viable %>% count(currcount_groups)
props[2,2]/nrow(dta)
props[3,2]/nrow(dta)

# Number of viable seeds per species
seeds_p_spp = aggregate(adjstcount_2 ~ species, data = dta, FUN = sum)
viable_seeds_p_spp = seeds_p_spp[seeds_p_spp$adjstcount_2 >0,]

#cound number of species whose seeds meet different targets
bank_total = nrow(seeds_p_spp)
bank_viable = nrow(viable_seeds_p_spp)
bank_250 = nrow(seeds_p_spp[seeds_p_spp$adjstcount_2 >250,])
bank_1050 = nrow(seeds_p_spp[seeds_p_spp$adjstcount_2 >1050,])
bank_250_1050= bank_250-bank_1050


#proportion of species with viable collections
bank_250_1050/bank_total
bank_1050/bank_total
bank_250/bank_total

# proportion of collections that are individually viable




####### Explore EX species IUCN Sep 21 #####
extint<- read.csv("C:\\Users\\alber\\The Royal Botanic Gardens, Kew\\Juan Viruel - GAP_ANALYSIS_CR2021\\Databases\\EX_IUCN_Sep21/assessments.csv")
length(unique(extint$scientificName))

semi_join(extint, data, by=c("scientificName"="species3"))
semi_join(data, extint, by=c("species3"="scientificName"))
######## Explore IUCN data September 2021 #####
iucn_assess_sep <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/CR_IUCN_Sep21/assessments.csv")
iucn_taxo_sep <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/CR_IUCN_Sep21/taxonomy.csv")
iucn_syn_sep <- read.csv("C:\\Users\\kdh10kg\\OneDrive - The Royal Botanic Gardens, Kew\\GAP_ANALYSIS_CR2021\\IUCN and POWO Databases/CR_IUCN_Sep21/synonyms.csv")

length(unique(iucn_assess_sep$scientificName)) #5026 unique values
length(unique(iucn_taxo_sep$scientificName)) #5026 unique values
length(unique(iucn_syn_sep$scientificName)) #1084 unique values

#fuzzyjoin synonyms
library("fuzzyjoin")
wcvp = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/wcvp__2_/wcvp_names.csv", sep = "|")
iucn_syn_sep$synonym = paste(iucn_syn_sep$genusName, iucn_syn_sep$speciesName)
stringdist_join(spp$species[3],iucn_assess_sep$scientificName[1])
test = stringdist_join(spp, iucn_assess_sep[,1:8], by=c(species="scientificName"))

spp$species[1:10] %in% iucn_syn_sep$synonym
spp$species[1:10] %in% iucn_assess_sep$scientificName


syn = cbind(name = spp$species,
            iucn = spp$species %in% iucn_assess_sep$scientificName,
            synonym = spp$species %in% iucn_syn_sep$synonym)

syn = cbind(syn, no_match = syn[,"iucn"] == FALSE & syn[,"synonym"] == FALSE)
