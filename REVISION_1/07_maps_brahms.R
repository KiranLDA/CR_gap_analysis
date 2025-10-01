# library("rnaturalearth")
# library("rnaturalearthdata")
library(geodata)
library(dplyr)
library(stringdist)
library(sf)
library(sp)
library(biscale)
library(ggplot2)
library(cowplot)
library(vioplot)
library(viridis)
library(biscale)
library(classInt)
library(rnaturalearth)
library(rnaturalearthdata)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"
plotpath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/plots/revision_1/"

#######   FUNCTION   ##################################################################################

# function to normalise data
normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}

#rotate matrix
rotate <- function(x) t(apply(x, 2, rev))

# Function to perform fuzzy matching
fuzzy_match <- function(str1, str2) {
  distance <- stringdist(str1, str2, method = "jw")  # Using Jaro-Winkler distance
  return(distance < 0.2)  # Adjust threshold as needed
}

# bi-color scale creation
colmat<-function(nquantiles=4, upperleft=rgb(0,150,235, maxColorValue=255),
                 upperright=rgb(130,0,80, maxColorValue=255),
                 bottomleft="grey",
                 bottomright=rgb(255,230,15, maxColorValue=255)
                 , xlab="x label", ylab="y label"
){
  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="fisher")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
  }
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  return(col.matrix[c(seqs), c(seqs)])
}

#################   DATA MUNGING   ########################################################################


world <- sf::st_as_sf(world(path="."))


#########################################################################################################
##### MATCH IUCN TO WCVP AND COUNTRY DATA ###############################################################
#########################################################################################################
wcvp_countries <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

#read in iucn data and wcvp data
iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"), encoding = "UTF-8")
tdwg3_countries <- read.csv(paste0(basepath, "country_tdwg3_mapping.csv"))
tdwg3_countries$ISO_code[is.na(tdwg3_countries$ISO_code)] ="NA"

# make sure variables are in same format
iucn_wcvp_matched$wcvp_accepted_id <- as.numeric(iucn_wcvp_matched$wcvp_accepted_id)
wcvp_countries$plant_name_id <- as.numeric(wcvp_countries$plant_name_id)

# put wcvp tdwg3 region data into iucn data
iucn_wcvp_matched_countries = iucn_wcvp_matched %>% left_join(wcvp_countries[,c("plant_name_id",
                                                                                "area_code_l3",
                                                                                "area")],
                                                              by = c("wcvp_accepted_id" = "plant_name_id"),
                                                              relationship = "many-to-many")


# match with country data
iucn_wcvp_matched_countries_tdwg3 = iucn_wcvp_matched_countries %>% left_join(tdwg3_countries[,c("LEVEL3_COD",
                                                                                                 "Gallagher_country",
                                                                                                 "ISO_code",
                                                                                                 "COUNTRY")],
                                                                              by = c("area_code_l3" = "LEVEL3_COD"))

# identify the duplicates
iucn_wcvp_matched_countries_tdwg3$country_duplicate = duplicated(iucn_wcvp_matched_countries_tdwg3[,c("wcvp_accepted_id","Gallagher_country")])

# match with world data
iucn_wcvp_matched_countries_tdwg3$NewCountryName = iucn_wcvp_matched_countries_tdwg3$Gallagher_country


# to_compare = unique(data.frame(world)[,c("GID_0","NAME_0")])
# to_compare = to_compare[order(to_compare$NAME_0),]
# View(to_compare)

# test = data.frame(world)[,c("GID_0","NAME_0")]
country_names = data.frame(unique(iucn_wcvp_matched_countries_tdwg3[,"NewCountryName"]))
colnames(country_names) = "NewCountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")],
                                            by=c("NewCountryName" = "NAME_0"))

country_names$NewCountryName[which(is.na(country_names$GID_0))]

country_names$NewCountryName[country_names$NewCountryName == "British Overseas Territories"] = "United Kingdom"
country_names$NewCountryName[country_names$NewCountryName == "Canary Is."] = "Spain"
country_names$NewCountryName[country_names$NewCountryName == "Zaïre"] = "Democratic Republic of the Congo"
country_names$NewCountryName[country_names$NewCountryName == "Lebanon-Syria"] = "Lebanon"
country_names$NewCountryName[country_names$NewCountryName == "New Guinea"] = "Papua New Guinea"
country_names$NewCountryName[country_names$NewCountryName == "Borneo"] = "Indonesia"
country_names$NewCountryName[country_names$NewCountryName == "Solomon Is." ] = "Solomon Islands"
country_names$NewCountryName[country_names$NewCountryName == "East Himalaya" ] = "Nepal"
country_names$NewCountryName[country_names$NewCountryName == "Cook Is."] = "Cook Islands"
country_names$NewCountryName[country_names$NewCountryName == "Transcaucasus" ] = "Georgia"
country_names$NewCountryName[country_names$NewCountryName == "Great Britain"] = "United Kingdom"
country_names$NewCountryName[country_names$NewCountryName == "French Offshore Territories" ] ="France"
country_names$NewCountryName[country_names$NewCountryName == "Yugoslavia"] = "Bosnia and Herzegovina"
country_names$NewCountryName[country_names$NewCountryName == "Azores" ] = "Portugal"
country_names$NewCountryName[country_names$NewCountryName == "Madeira"] = "Portugal"
country_names$NewCountryName[country_names$NewCountryName == "Norfolk Is." ] = "Australia"
country_names$NewCountryName[country_names$NewCountryName == "Czechoslovakia" ] = "Czech Republic"
country_names$NewCountryName[country_names$NewCountryName == "Baltic States"] = "Latvia"
country_names$NewCountryName[country_names$NewCountryName == "Korea"] = "North Korea"
country_names$NewCountryName[country_names$NewCountryName == "Jawa"] = "Indonesia"
country_names$NewCountryName[country_names$NewCountryName == "Sumatera"] = "Indonesia"
country_names$NewCountryName[country_names$NewCountryName == "Tubuai Is." ] = "French Polynesia"
country_names$NewCountryName[country_names$NewCountryName == "Andaman Is." ] = "India"
country_names$NewCountryName[country_names$NewCountryName == "Nicobar Is."] = "India"
country_names$NewCountryName[country_names$NewCountryName == "Malaya" ] = "Malaysia"
country_names$NewCountryName[country_names$NewCountryName == "Ivory Coast" ] = "Côte d'Ivoire"
country_names$NewCountryName[country_names$NewCountryName == "Gulf of Guinea Is."] = "São Tomé and Príncipe"
country_names$NewCountryName[country_names$NewCountryName == "Rodrigues" ] = "Mauritius"
country_names$NewCountryName[country_names$NewCountryName == "Kirgizistan" ] = "Kyrgyzstan"
country_names$NewCountryName[country_names$NewCountryName == "Tadzhikistan" ]  = "Tajikistan"
country_names$NewCountryName[country_names$NewCountryName == "Cape Verde"] = "Cabo Verde"
country_names$NewCountryName[country_names$NewCountryName == "Selvagens" ] = "Portugal"
country_names$NewCountryName[country_names$NewCountryName == "Socotra" ] = "Yemen"
country_names$NewCountryName[country_names$NewCountryName == "Lesser Sunda Is."] = "Indonesia"
country_names$NewCountryName[country_names$NewCountryName == "Sulawesi" ]  = "Indonesia"
country_names$NewCountryName[country_names$NewCountryName == "Aldabra"] = "Seychelles"
country_names$NewCountryName[country_names$NewCountryName == "Kazan-retto"] = "Japan"
country_names$NewCountryName[country_names$NewCountryName == "Maluku" ]  = "Indonesia"
country_names$NewCountryName[country_names$NewCountryName == "Caroline Is."  ]  = "Micronesia"
country_names$NewCountryName[country_names$NewCountryName == "Juan Fernández Is." ] = "Chile"
country_names$NewCountryName[country_names$NewCountryName == "Bismarck Archipelago" ] = "Papua New Guinea"
country_names$NewCountryName[country_names$NewCountryName == "Santa Cruz Is." ] = "Solomon Islands"
country_names$NewCountryName[country_names$NewCountryName == "Wallis-Futuna Is."] ="France"
country_names$NewCountryName[country_names$NewCountryName == "Marianas"] = "United States of America"
country_names$NewCountryName[country_names$NewCountryName == "Leeward Is. AB Ant" ] = "United States of America"
country_names$NewCountryName[country_names$NewCountryName == "Southwest Caribbean" ] = "Colombia"
country_names$NewCountryName[country_names$NewCountryName == "Windward Is." ] = "France"
country_names$NewCountryName[country_names$NewCountryName == "Trinidad-Tobago" ] = "Trinidad and Tobago"
country_names$NewCountryName[country_names$NewCountryName == "Netherlands Antilles" ] = "Netherlands"
country_names$NewCountryName[country_names$NewCountryName == "Surinam" ] = "Suriname"
country_names$NewCountryName[country_names$NewCountryName == "Galápagos" ] = "Ecuador"
country_names$NewCountryName[country_names$NewCountryName == "Gambia, The" ] = "Gambia"
country_names$NewCountryName[country_names$NewCountryName == "Line Is." ] = "Kiribati"
country_names$NewCountryName[country_names$NewCountryName == "C. American Pacific Is."] = "France"
country_names$NewCountryName[country_names$NewCountryName == "Venezuelan Antilles" ] = "Venezuela"
country_names$NewCountryName[country_names$NewCountryName == "Burkina" ] = "Burkina Faso"
country_names$NewCountryName[country_names$NewCountryName == "Swaziland" ] = "Eswatini"
country_names$NewCountryName[country_names$NewCountryName == "Gulf States" ] = "United Arab Emirates"
country_names$NewCountryName[country_names$NewCountryName == "South China Sea" ] = "Vietnam"
country_names$NewCountryName[country_names$NewCountryName == "Christmas I." ] = "Australia"
country_names$NewCountryName[country_names$NewCountryName == "Kermadec Is."] = "New Zealand"
country_names$NewCountryName[country_names$NewCountryName == "Gilbert Is."  ] = "Kiribati"
country_names$NewCountryName[country_names$NewCountryName == "Easter Is." ] = "Chile"
country_names$NewCountryName[country_names$NewCountryName == "Prince Edward I." ] = "South Africa"
country_names$NewCountryName[country_names$NewCountryName == "Desventurados Is." ] = "Chile"
country_names$NewCountryName[country_names$NewCountryName == "Antipodean Is." ] = "New Zealand"
country_names$NewCountryName[country_names$NewCountryName == "Chatham Is." ]  = "New Zealand"
country_names$NewCountryName[country_names$NewCountryName == "Macquarie Is." ] = "Australia"
country_names$NewCountryName[country_names$NewCountryName == "Laccadive Is." ] = "India"
country_names$NewCountryName[country_names$NewCountryName == "Cocos (Keeling) I." ] = "Australia"
country_names$NewCountryName[country_names$NewCountryName == "Phoenix Is." ] = "Kiribati"
country_names$NewCountryName[country_names$NewCountryName == "Tokelau-Manihiki" ] = "New Zealand"
country_names$NewCountryName[country_names$NewCountryName == "Marshall Is." ] = "Marshall Islands"
country_names$NewCountryName[country_names$NewCountryName == "Wake I." ] = "United States of America"
# country_names$NewCountryName[country_names$NewCountryName == "Tuvalu" ]
# country_names$NewCountryName[country_names$NewCountryName == "Nauru"  ]
country_names$NewCountryName[country_names$NewCountryName == "Cayman\xa0Islands"] = "Cayman Islands"
country_names$NewCountryName[country_names$NewCountryName == "Northern\xa0Mariana\xa0Islands"] = "Northern Mariana Islands"
country_names$NewCountryName[country_names$NewCountryName == "Turks\xa0and\xa0Caicos\xa0Islands"]= "Turks and Caicos Islands"

iucn_wcvp_matched_countries_tdwg3[which(iucn_wcvp_matched_countries_tdwg3$Gallagher_country == "Wake I."),
                                  c("Gallagher_country","COUNTRY")]

iucn_world = world %>% left_join(iucn_wcvp_matched_countries_tdwg3,
                                 by = c( "NAME_0" = "NewCountryName" ))

#########################################################################################################
##### WHAT IS IN THE BANK? ##############################################################################
#########################################################################################################

brahms_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_wcvp_matched_full_name_infra.csv"), encoding = "UTF-8")

####### FORMAT COUNTRY NAMES #############################################################################
# add a new column with the reformatted name
brahms_wcvp_matched$NewCountryName = brahms_wcvp_matched$COUNTRY

# spelling errors
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Auckland Island"] = "New Zealand"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Bosnia-Herzegovina"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "UK"] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "USA"] = "United States"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Congo, DRC"] = "Democratic Republic of the Congo"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "St. Helena, Ascension & Tristan da Cunha"] = "Saint Helena"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Ivory Coast"] = "Côte d'Ivoire"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Heard Isl"] = "Heard Island and McDonald Islands"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Sao Tome & Principe"] = "São Tomé and Príncipe"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Swaziland"] = "Eswatini"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Cape Verde"] = "Cabo Verde"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Palestinian Territory, Occupied"] = "Palestine"

brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Korea, South"] = "South Korea"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "South Georgia and the Islands"] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Trinidad & Tobago"] = "Trinidad and Tobago"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "British Virgin Islands" ] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "North Macedonia" ] = "Macedonia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "\tMacedonia"] = "Macedonia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Saint Helena, Ascension, and Tristan da Cunha"] = "Saint Helena"


#putting territories with their countries
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Christmas I."] = "Australia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Norfolk Island"] = "Australia"

brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "British Virgin Is."] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Bermuda"] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Gibraltar"] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Pitcairn"] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "British Indian Ocean Territory"] = "United Kingdom"

brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Turks & Caicos Is."] = "Turks and Caicos Islands"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Cayman Is."] = "Cayman Islands"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Anguilla"] = "United Kingdom"



test1 = data.frame(world)[,c("GID_0","NAME_0")]
# the political merges that I feel a little unsure about as there are no lat longs to associate to a country after splitting
# or other political regions e.g. San Marino not being recognised by geodata as it's own country
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Russian Federation"] = "Russia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Yugoslavia"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "San-Marino"] = "Italy"

##### SUBSET THE CR SPECIES ###################################################################

# find the MSB species that are CR endangered
brahms_wcvp_matched$CR = brahms_wcvp_matched$wcvp_accepted_id %in% iucn_wcvp_matched$wcvp_accepted_id
summary(brahms_wcvp_matched$CR)

brahms_CR = brahms_wcvp_matched[which(brahms_wcvp_matched$CR),]
dim(brahms_CR)

# test = data.frame(world)[,c("GID_0","NAME_0")]
country_names = data.frame(unique(brahms_CR[,"NewCountryName"]))
colnames(country_names) = "NewCountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")],
                                            by=c("NewCountryName" = "NAME_0"))

# Fuzzy match the country names that weren't already matched
# unmatched = country_names$NewCountryName[is.na(country_names$GID_0)]
# leftover = c()
# for (country in unmatched){
#   idw = which(fuzzy_match(country,world$NAME_0))
#   if (length(idw) == 1){
#     idc = which(country_names$NewCountryName == country)
#     country_names[idc,2] = data.frame(world)[idw,"GID_0"]
#   } else {
#     leftover = c(leftover, country)
#     print(country)
#   }
# }
# leftover
# note that these names were not matched
# [1] "Unknown"
# [1] "FYROM"



###### ESTIMATE HOW MANY COUNTRIES HAVE ENOUGH SEEDS   ############################################################

### FOR CR SPECIES ################################################################################################

brahms_CR_short <- brahms_CR[which(!is.na(brahms_CR$taxon_name) & !(brahms_CR$NewCountryName %in% c("Unknown","?"))),]

# Get the counts per country per spp
CR_country_spp_seeds = brahms_CR_short %>%
  group_by(NewCountryName, taxon_name, ADJSTCOUNT) %>% #
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         sum_counts = sum(ADJSTCOUNT),
         accessions = n()) %>%
  ungroup()
CR_country_spp_seeds = CR_country_spp_seeds[, c("NewCountryName","taxon_name", "sum_counts", "accessions")]
CR_country_spp_seeds = unique(CR_country_spp_seeds)

# determine how they can be used
CR_country_spp_seeds$collection_utility = NA
CR_country_spp_seeds$collection_utility[CR_country_spp_seeds$sum_counts == 0] = "with_partner"
CR_country_spp_seeds$collection_utility[CR_country_spp_seeds$sum_counts > 0 & CR_country_spp_seeds$sum_counts < 250] = "stay_in_bank"
CR_country_spp_seeds$collection_utility[CR_country_spp_seeds$sum_counts >= 250 & CR_country_spp_seeds$sum_counts < 1500] = "germination_testing"
CR_country_spp_seeds$collection_utility[CR_country_spp_seeds$sum_counts >= 1500] = "restoration_use"

# save the data
write.csv(CR_country_spp_seeds, paste0(basepath, "revision_1/CR_seeds_per_spp_country_with_usage.csv"), fileEncoding = "UTF-8")

# Now count the numbers of species per country with the different uses
CR_country_uses = CR_country_spp_seeds %>%
  group_by(NewCountryName, collection_utility) %>%
  tally() %>%
  # mutate(sum_uses = length(unique(collection_utility)),
  #        uses = n()) %>%
  ungroup()
colnames(CR_country_uses) = c("NewCountryName", "CR_collection_utility","species_number")

# save the data
write.csv(CR_country_uses, paste0(basepath, "revision_1/CR_usage_per_country_with_spp_number.csv"), fileEncoding = "UTF-8")



### FOR ALL SPECIES IN THE BANK ########################################################################

brahms_wcvp_matched_short <- brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$taxon_name) & !(brahms_wcvp_matched$NewCountryName %in% c("Unknown","?"))),]


# Get the counts per country per spp
country_spp_seeds = brahms_wcvp_matched_short %>%
  group_by(NewCountryName, taxon_name, ADJSTCOUNT) %>%
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         sum_counts = sum(ADJSTCOUNT),
         accessions = n()) %>%
  ungroup()
country_spp_seeds = country_spp_seeds[, c("NewCountryName","taxon_name", "sum_counts", "accessions")]
country_spp_seeds = unique(country_spp_seeds)

# determine how they can be used
country_spp_seeds$collection_utility = NA
country_spp_seeds$collection_utility[country_spp_seeds$sum_counts == 0] = "with_partner"
country_spp_seeds$collection_utility[country_spp_seeds$sum_counts > 0 & country_spp_seeds$sum_counts < 250] = "stay_in_bank"
country_spp_seeds$collection_utility[country_spp_seeds$sum_counts >= 250 & country_spp_seeds$sum_counts < 1500] = "germination_testing"
country_spp_seeds$collection_utility[country_spp_seeds$sum_counts >= 1500] = "restoration_use"

# save the data
write.csv(country_spp_seeds, paste0(basepath, "revision_1/seeds_per_spp_country_with_usage.csv"), fileEncoding = "UTF-8")

# Now count the numbers of species per country with the different uses
country_uses = country_spp_seeds %>%
  group_by(NewCountryName, collection_utility) %>%
  tally() %>%
  # mutate(sum_uses = length(unique(collection_utility)),
  #        uses = n()) %>%
  ungroup()
colnames(country_uses) = c("NewCountryName", "collection_utility","species_number")

# save the data
write.csv(country_uses, paste0(basepath, "revision_1/usage_per_country_with_spp_number.csv"), fileEncoding = "UTF-8")


####### ESTIMATE PER COUNTRY THE NUMBERS OF SPECIES, ACCESSIONS AND SEEDS   ############

###### FOR CR SPECIES ##################################################################

# count number of seeds collected per country
country_seeds = brahms_CR_short %>%
  group_by(NewCountryName, ADJSTCOUNT) %>%
  tally() %>%
  mutate(sum_seeds = sum(ADJSTCOUNT),
         accessions = n()) %>%
  ungroup()
country_seeds = country_seeds[, c("NewCountryName","sum_seeds")]
country_seeds = country_seeds[duplicated(country_seeds$NewCountryName) == FALSE, ]

# count species per country
country_spp = brahms_CR_short %>%
  group_by(NewCountryName, taxon_name) %>%
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         accessions = n()) %>%
  ungroup()
country_spp = country_spp[, c("NewCountryName","sum_spp")]
country_spp = country_spp[duplicated(country_spp$NewCountryName) == FALSE, ]

# count accessions per country
country_acc = brahms_CR_short %>%
  group_by(NewCountryName, ACCESSION) %>%
  tally() %>%
  mutate(sum_accessions = length(unique(ACCESSION)),
         accessions = n()) %>%
  ungroup()
country_acc = country_acc[, c("NewCountryName","sum_accessions")]
country_acc = country_acc[duplicated(country_acc$NewCountryName) == FALSE, ]

###### Combine everything
CR_country_counts = country_seeds %>% left_join(country_spp[,c("NewCountryName","sum_spp")],
                                                by = "NewCountryName") %>% left_join(country_acc[,c("NewCountryName","sum_accessions")],
                                                                                     by = "NewCountryName")


# save the data
write.csv(CR_country_counts, paste0(basepath, "revision_1/CR_seeds_accessions_species_per_country.csv"), fileEncoding = "UTF-8")


###### FOR ALL SPECIES IN THE BANK   ##############################################################

# count number of seeds collected per country
country_seeds = brahms_wcvp_matched_short %>%
  group_by(NewCountryName, ADJSTCOUNT) %>%
  tally() %>%
  mutate(sum_seeds = sum(ADJSTCOUNT),
         accessions = n()) %>%
  ungroup()
country_seeds = country_seeds[, c("NewCountryName","sum_seeds")]
country_seeds = country_seeds[duplicated(country_seeds$NewCountryName) == FALSE, ]

# count species per country
country_spp = brahms_wcvp_matched_short %>%
  group_by(NewCountryName, taxon_name) %>%
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         accessions = n()) %>%
  ungroup()
country_spp = country_spp[, c("NewCountryName","sum_spp")]
country_spp = country_spp[duplicated(country_spp$NewCountryName) == FALSE, ]

# count accessions per country
country_acc = brahms_wcvp_matched_short %>%
  group_by(NewCountryName, ACCESSION) %>%
  tally() %>%
  mutate(sum_accessions = length(unique(ACCESSION)),
         accessions = n()) %>%
  ungroup()
country_acc = country_acc[, c("NewCountryName","sum_accessions")]
country_acc = country_acc[duplicated(country_acc$NewCountryName) == FALSE, ]

###### Combine everything
country_counts = country_seeds %>% left_join(country_spp[,c("NewCountryName","sum_spp")],
                                             by = "NewCountryName") %>% left_join(country_acc[,c("NewCountryName","sum_accessions")],
                                                                                  by = "NewCountryName")


# save the data
write.csv(country_counts, paste0(basepath, "seeds_accessions_species_per_country.csv"), fileEncoding = "UTF-8")



####### ADD SPATIAL DATA TO THE NAMES #############################################################################


country_names = data.frame(unique(brahms_wcvp_matched_short[,"NewCountryName"]))
colnames(country_names) = "NewCountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")],
                                            by=c("NewCountryName" = "NAME_0"))

country_counts_map = country_counts %>% left_join(country_names)
country_counts_map = world %>% left_join(country_counts_map)

# CR_country_counts = CR_country_counts %>% left_join(country_names)

####  PROJECT in Mollweide

# create a bounding box - world extent
b.box <- as(raster::extent(-180, 180, -90, 90), "SpatialPolygons")

# assign CRS to box
WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

proj4string(b.box) <- WGS84

# create graticules/grid lines from box
# grid <- gridlines(b.box,
#                   easts  = seq(from=-180, to=180, by=20),
#                   norths = seq(from=-90, to=90, by=10))
grid <- gridlines(b.box,
                  easts  = seq(from=-180, to=180, by=360),
                  norths = seq(from=-90, to=90, by=180))

# give the PORJ.4 string for Eckert IV projection
PROJ <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #

grid.proj <- spTransform(grid,CRS(PROJ))


# transform bounding box
grid.DT <- data.table::data.table(map_data(SpatialLinesDataFrame(sl=grid.proj,
                                                                 data=data.frame(1:length(grid.proj)),
                                                                 match.ID = FALSE)))
grid.DT$X <- grid.DT$long
grid.DT$Y <- grid.DT$lat
grid.DT <- data.table::as.data.table(grid.DT)


sf_use_s2(FALSE)
m = st_buffer(country_counts_map, 0)
country_counts_map.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                           xmax = 180,
                                                           ymin = -90,
                                                           ymax = 90))),
                                      crs = PROJ)


################ ECKERT 4 ########
# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# sf_use_s2(FALSE)
# m = st_buffer(country_counts_map, 0)
# country_counts_map.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
#                                                            xmax = 180,
#                                                            ymin = -90,
#                                                            ymax = 90))),
#                                       crs = PROJ)
#
#
# # write.csv(grid.DT, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
# grid.DT = read.csv(paste0(basepath, "gridDT.csv"))
# grid.DT <- data.table::as.data.table(grid.DT)

#### Format the projected data ready for plotting  ################################

#log the data because it is very
country_counts_map.prj$log_seeds = log(country_counts_map.prj$sum_seeds+1)
country_counts_map.prj$log_spp = log(country_counts_map.prj$sum_spp+1)

# replace NAs with zeros
country_counts_map.prj$log_seeds[which(is.na(country_counts_map.prj$log_seeds))] = 0
country_counts_map.prj$log_spp[which(is.na(country_counts_map.prj$log_spp))] = 0
country_counts_map.prj$sum_seeds[which(is.na(country_counts_map.prj$sum_seeds))] = 0
country_counts_map.prj$sum_spp[which(is.na(country_counts_map.prj$sum_spp))] = 0
country_counts_map.prj$sum_accessions[which(is.na(country_counts_map.prj$sum_accessions))] = 0




# values
dim=3

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#73AE80", #rgb(0,150,235, maxColorValue=255),
                   upperright= "grey80",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#2A5A5B", #"black",
                   bottomright="#6C83B5"# brown3"#rgb(130,0,80, maxColorValue=255)
)

custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))



data <- bi_class(country_counts_map.prj,
                 y=sum_seeds,#sum_accessions,
                 x=sum_spp,
                 style ="quantile", #"jenks",# "equal",#  , "equal", "fisher"""quantile",#"jenks",#"fisher",#
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data = data,
              aes(color =  bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates" ) +
  # geom_sf(data = data, mapping = aes(fill = bi_class),
  #         color = aes(fill = bi_class ),
  #         linewidth = 0.3, show.legend = FALSE) +
  geom_sf(data = data,
          # mapping = aes(fill = bi_class, color = bi_class ),
          mapping = aes(fill = bi_class),
          color = "black",
          linewidth = 0.3, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  bi_scale_fill(pal = custom_pal4, dim=dim,
                na.value="#e8e8e8")+
  bi_scale_color(pal = custom_pal4, dim=dim,
                 na.value="#e8e8e8")+
  guides(color = "none") +
  bi_scale_fill(pal = custom_pal4, dim=dim,
                na.value="#e8e8e8")+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim,
                 na.value="#e8e8e8")+#,flip_axes = TRUE, rotate_pal = TRUE) +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=8), #linewidth = 8),#
        legend.title=element_text(size=10) # linewidth = 10)#
  )

map

legend <- bi_legend(pal = custom_pal4,#"GrPink",
                    dim = dim,
                    xlab = "# species",
                    ylab = "     # seeds",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.82, .7, 0.24, 0.24)


finalPlot
ggsave(paste0(plotpath, "seeds_vs_species.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "seeds_vs_species.png"), width = 30, height = 12, units = "cm", bg="white")



######################################################################################
#####  Estimate CR species number  ###################################################
######################################################################################

country_names = data.frame(unique(brahms_CR_short[,"NewCountryName"]))
colnames(country_names) = "NewCountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")],
                                            by=c("NewCountryName" = "NAME_0"))

country_counts_map = CR_country_counts %>% left_join(country_names)
country_counts_map = world %>% left_join(country_counts_map)

# CR_country_counts = CR_country_counts %>% left_join(country_names)

####  PROJECT in Mollweide

# create a bounding box - world extent
b.box <- as(raster::extent(-180, 180, -90, 90), "SpatialPolygons")

# assign CRS to box
WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

proj4string(b.box) <- WGS84

# create graticules/grid lines from box
# grid <- gridlines(b.box,
#                   easts  = seq(from=-180, to=180, by=20),
#                   norths = seq(from=-90, to=90, by=10))
grid <- gridlines(b.box,
                  easts  = seq(from=-180, to=180, by=360),
                  norths = seq(from=-90, to=90, by=180))

# give the PORJ.4 string for Eckert IV projection
PROJ <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #

grid.proj <- spTransform(grid,CRS(PROJ))


# transform bounding box
grid.DT <- data.table::data.table(map_data(SpatialLinesDataFrame(sl=grid.proj,
                                                                 data=data.frame(1:length(grid.proj)),
                                                                 match.ID = FALSE)))
grid.DT$X <- grid.DT$long
grid.DT$Y <- grid.DT$lat
grid.DT <- data.table::as.data.table(grid.DT)


sf_use_s2(FALSE)
m = st_buffer(country_counts_map, 0)
country_counts_map.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                           xmax = 180,
                                                           ymin = -90,
                                                           ymax = 90))),
                                      crs = PROJ)


################ ECKERT 4 ########
# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# sf_use_s2(FALSE)
# m = st_buffer(country_counts_map, 0)
# country_counts_map.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
#                                                            xmax = 180,
#                                                            ymin = -90,
#                                                            ymax = 90))),
#                                       crs = PROJ)
#
#
# # write.csv(grid.DT, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
# grid.DT = read.csv(paste0(basepath, "gridDT.csv"))
# grid.DT <- data.table::as.data.table(grid.DT)

#### Format the projected data ready for plotting  ################################

#log the data because it is very
country_counts_map.prj$log_seeds = log(country_counts_map.prj$sum_seeds+1)
country_counts_map.prj$log_spp = log(country_counts_map.prj$sum_spp+1)

# replace NAs with zeros
country_counts_map.prj$log_seeds[which(is.na(country_counts_map.prj$log_seeds))] = 0
country_counts_map.prj$log_spp[which(is.na(country_counts_map.prj$log_spp))] = 0
country_counts_map.prj$sum_seeds[which(is.na(country_counts_map.prj$sum_seeds))] = 0
country_counts_map.prj$sum_spp[which(is.na(country_counts_map.prj$sum_spp))] = 0
country_counts_map.prj$sum_accessions[which(is.na(country_counts_map.prj$sum_accessions))] = 0

# values
dim=3

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#73AE80", #rgb(0,150,235, maxColorValue=255),
                   upperright= "grey80",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#2A5A5B", #"black",
                   bottomright="#6C83B5"# brown3"#rgb(130,0,80, maxColorValue=255)
)

custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(country_counts_map.prj,
                 y=sum_seeds,#sum_accessions,
                 x=sum_spp,
                 style = "fisher",#  "quantile",#"equal",#"jenks",#, "equal", "fisher""jenks",#
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data = data,
              aes(color =  bi_class, #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates" ) +
  geom_sf(data = data,
          #mapping = aes(fill = bi_class, color = bi_class),
          mapping = aes(fill = bi_class),
          color = "black",
          linewidth = 0.3, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  bi_scale_fill(pal = custom_pal4, dim=dim,
                na.value="#e8e8e8")+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim,
                 na.value="#e8e8e8")+#,flip_axes = TRUE, rotate_pal = TRUE) +
  guides(color = "none") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=8), #linewidth = 8),#
        legend.title=element_text(size=10) # linewidth = 10)#
  )

map

legend <- bi_legend(pal = custom_pal4,#"GrPink",
                    dim = dim,
                    xlab = "# species",
                    ylab = "     # seeds",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.82, .7, 0.24, 0.24)


finalPlot
ggsave(paste0(plotpath, "seeds_vs_species_CR.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "seeds_vs_species_CR.png"), width = 30, height = 12, units = "cm",bg="white")



