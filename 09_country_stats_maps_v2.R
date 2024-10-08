library(geodata)
library(dplyr)
library(stringdist)
library(sf)
library(biscale)
library(ggplot2)
library(cowplot)
library(vioplot)
library(viridis)
library(biscale)
library(classInt)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(ggpubr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"
plotpath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/"

wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ),
                             sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

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

#~~~~~~~~~~~~~
# Read data
#~~~~~~~~~~~~~
indexes = read.csv(paste0(basepath,"iucn_brahms_indexes_targets.csv"))


permissions = read.csv(paste0(basepath,"ABSCH-Country-List_03_07_24.csv"))

iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))
brahms_wcvp_matched = brahms_wcvp_matched %>% left_join(indexes[,c("ACCESSION","ADJSTCOUNT", "PLANTSAMP",
                                                                   "Cultivated", "Derived.From","Result", "DateStarted", "DateCollected",
                                                                   "DateDonated","DateGermplasmBanked", "geographic_index", "taxonomy_index",
                                                                   "year_index", "information_index", "count_index", "adjcount_index",
                                                                   "germination_index", "viability_index", "cultivation_index",
                                                                   "exsitu_index", "genetic_index", "total_index", "prop_range_banked",
                                                                   "Target_1a", "Target_1b", "Target_1", "Target_2")],
                                                        by = c("AccessionNumber" = "ACCESSION"))



brahms_wcvp_matched$PLANTSAMP[brahms_wcvp_matched$PLANTSAMP == "11-100"] = "50"
brahms_wcvp_matched$PLANTSAMP[brahms_wcvp_matched$PLANTSAMP == "100-1000"] = "100"
brahms_wcvp_matched$PLANTSAMP[brahms_wcvp_matched$PLANTSAMP == "25-50"] = "50"
brahms_wcvp_matched$PLANTSAMP[brahms_wcvp_matched$PLANTSAMP == ">100"] = "100"
brahms_wcvp_matched$PLANTSAMP[brahms_wcvp_matched$PLANTSAMP == "200-500"] = "200"
brahms_wcvp_matched$PLANTSAMP[is.na(brahms_wcvp_matched$PLANTSAMP)] = "0"
brahms_wcvp_matched$plants_sampled = as.numeric(brahms_wcvp_matched$PLANTSAMP)
# brahms_wcvp_matched$ADJSTCOUNT[is.na(brahms_wcvp_matched$ADJSTCOUNT)] = 0

iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "iucn_predictions_wcvp_matched.csv"))
iucn_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]


tdwg3_countries <- read.csv(paste0(basepath, "country_tdwg3_mapping.csv"))
tdwg3_countries$ISO_code[is.na(tdwg3_countries$ISO_code)] ="NA"
spp_banked_recalcitrant <- read.csv(paste0(basepath,"spp_banked_recalcitrant.csv"))

spp_banked_recalcitrant = spp_banked_recalcitrant %>% left_join(brahms_wcvp_matched[,c("taxon_name","wcvp_accepted_id")],
                                      by = "taxon_name")
spp_banked_recalcitrant = spp_banked_recalcitrant %>% left_join(iucn_wcvp_matched[,c("taxon_name","wcvp_accepted_id")],
                                             by = "taxon_name")
spp_banked_recalcitrant = spp_banked_recalcitrant %>% left_join(iucn_predictions_wcvp_matched[,c("taxon_name","wcvp_accepted_id")],
                          by = "taxon_name")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add the ID to the recalcitrant species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

spp_banked_recalcitrant$wcvp_accepted_id <- ifelse(is.na(spp_banked_recalcitrant$wcvp_accepted_id),
                                                   ifelse(is.na(spp_banked_recalcitrant$wcvp_accepted_id.y),
                                                          spp_banked_recalcitrant$wcvp_accepted_id.x,
                                                          spp_banked_recalcitrant$wcvp_accepted_id.y),
                           spp_banked_recalcitrant$wcvp_accepted_id)
spp_banked_recalcitrant$wcvp_accepted_id = as.numeric(spp_banked_recalcitrant$wcvp_accepted_id)
spp_banked_recalcitrant = spp_banked_recalcitrant[ , !(names(spp_banked_recalcitrant) %in% c("wcvp_accepted_id.x","wcvp_accepted_id.y"))]

# make sure variables are in same format
spp_banked_recalcitrant$wcvp_accepted_id <- as.numeric(spp_banked_recalcitrant$wcvp_accepted_id)
wcvp_countries$plant_name_id <- as.numeric(wcvp_countries$plant_name_id)


# put wcvp tdwg3 region data into iucn data
iucn_wcvp_matched_countries = spp_banked_recalcitrant %>% left_join(wcvp_countries[,c("plant_name_id",
                                                                                "area_code_l3",
                                                                                "area")],
                                                              by = c("wcvp_accepted_id" = "plant_name_id"),
                                                              relationship = "many-to-many")

rm(wcvp_countries)

# match with country data
iucn_wcvp_matched_countries_tdwg3 = iucn_wcvp_matched_countries %>% left_join(tdwg3_countries[,c("LEVEL3_COD",
                                                                                                 "Gallagher_country",
                                                                                                 "ISO_code",
                                                                                                 "COUNTRY")],
                                                                              by = c("area_code_l3" = "LEVEL3_COD"))

iucn_wcvp_matched_countries_tdwg3$ISO3 = MazamaSpatialUtils::iso2ToIso3(iucn_wcvp_matched_countries_tdwg3$ISO_code)

iucn_wcvp_matched_countries_tdwg3 <- iucn_wcvp_matched_countries_tdwg3 %>% left_join(data.frame(world)[,c("GID_0","NAME_0")],
                                                                                     by = c("ISO3"="GID_0"))

# match with world data
iucn_wcvp_matched_countries_tdwg3$NewCountryName = iucn_wcvp_matched_countries_tdwg3$NAME_0#iucn_wcvp_matched_countries_tdwg3$Gallagher_country


# identify the duplicates
iucn_wcvp_matched_countries_tdwg3$country_duplicate = duplicated(iucn_wcvp_matched_countries_tdwg3[,c("scientificName",
                                                                                                      "wcvp_accepted_id",
                                                                                                      "NewCountryName")])


#########################################################################################################
##### FORMAT BANKED DATA    ##############################################################################
#########################################################################################################

# add a new column with the reformatted name
brahms_wcvp_matched$NewCountryName = brahms_wcvp_matched$CountryName

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

# or other political regions e.g. San Marino not being recognised by geodata as it's own country
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Russian Federation"] = "Russia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Yugoslavia"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "San-Marino"] = "Italy"


permissions$NewCountryName = permissions$Country


permissions$NewCountryName[which(permissions$NewCountryName == "Bolivia (Plurinational State of)")] = "Bolivia"
permissions$NewCountryName[which(permissions$NewCountryName == "Brunei Darussalam")] = "Brunei"
permissions$NewCountryName[which(permissions$NewCountryName == "Czechia")] = "Czech Republic"
permissions$NewCountryName[which(permissions$NewCountryName == "Democratic People's Republic of Korea" )] = "North Korea"
# permissions$NewCountryName[which(permissions$NewCountryName == "European Union")] = NA
# permissions$NewCountryName[which(permissions$NewCountryName == "Holy See")] = "Vatican"
permissions$NewCountryName[which(permissions$NewCountryName == "Iran (Islamic Republic of)")] = "Iran"
permissions$NewCountryName[which(permissions$NewCountryName == "Lao People's Democratic Republic")] = "Laos"
# permissions$NewCountryName[which(permissions$NewCountryName == "Maldives")] = NA
# permissions$NewCountryName[which(permissions$NewCountryName == "Marshall Islands")] =NA
permissions$NewCountryName[which(permissions$NewCountryName == "Micronesia (Federated States of)" )] = "Micronesia"
# permissions$NewCountryName[which(permissions$NewCountryName == "Monaco")] =NA
# permissions$NewCountryName[which(permissions$NewCountryName == "Nauru")] =NA
permissions$NewCountryName[which(permissions$NewCountryName == "Netherlands (Kingdom of the)")] = "Netherlands"
permissions$NewCountryName[which(permissions$NewCountryName == "North Macedonia")] = "Macedonia"
permissions$NewCountryName[which(permissions$NewCountryName == "Republic of Korea")] = "South Korea"
permissions$NewCountryName[which(permissions$NewCountryName == "Republic of Moldova")] = "Moldova"
permissions$NewCountryName[which(permissions$NewCountryName == "Russian Federation")] = "Russia"
# permissions$NewCountryName[which(permissions$NewCountryName == "San Marino")] =NA
permissions$NewCountryName[which(permissions$NewCountryName == "Sao Tome and Principe")] = "São Tomé and Príncipe"
permissions$NewCountryName[which(permissions$NewCountryName == "State of Palestine")] = "Palestine"
permissions$NewCountryName[which(permissions$NewCountryName == "Syrian Arab Republic")] = "Syria"
permissions$NewCountryName[which(permissions$NewCountryName == "Timor-Leste")] = "East Timor"
# permissions$NewCountryName[which(permissions$NewCountryName == "Tuvalu")] =NA
permissions$NewCountryName[which(permissions$NewCountryName == "Türkiye")]  =  "Turkey"
permissions$NewCountryName[which(permissions$NewCountryName == "United Kingdom of Great Britain and Northern Ireland")] = "United Kingdom"
permissions$NewCountryName[which(permissions$NewCountryName == "United Republic of Tanzania")] = "Tanzania"
permissions$NewCountryName[which(permissions$NewCountryName == "United States of America")] = "United States"
permissions$NewCountryName[which(permissions$NewCountryName == "Venezuela (Bolivarian Republic of)")] = "Venezuela"
permissions$NewCountryName[which(permissions$NewCountryName == "Viet Nam") ] =  "Vietnam"


#-------------------------------------------------------------------------------------------------#
##### SUBSET THE BANKED SPECIES ###################################################################
#-------------------------------------------------------------------------------------------------#
# get rid of duplicated tdwgs (mulitple tdwgs in one country are remove)
iucn_wcvp_matched_countries_tdwg3 = iucn_wcvp_matched_countries_tdwg3[which(iucn_wcvp_matched_countries_tdwg3$country_duplicate == F),]

summary(iucn_wcvp_matched_countries_tdwg3$plants_sampled)

# find the MSB species that are CR endangered
# iucn_wcvp_matched_countries_tdwg3$banked = (iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id %in%
#                                               brahms_wcvp_matched$wcvp_accepted_id)
# (data.frame(banked = iucn_wcvp_matched_countries_tdwg3$banked,
#             id = iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id ))[451:501,]
# CR species
# iucn_wcvp_matched_countries_tdwg3$banked_per_country
iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country = 0
iucn_wcvp_matched_countries_tdwg3$accessions_per_spp_country = 0
iucn_wcvp_matched_countries_tdwg3$seeds_per_spp_country = 0
iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country = 0
for(ID in unique(iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id)){
  # ID = 3259908#2797442 #3144077#
  iucn_row = which(iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id == ID)
  iucn_country = iucn_wcvp_matched_countries_tdwg3$NewCountryName[iucn_row]
  banked_row = which(as.numeric(brahms_wcvp_matched$wcvp_accepted_id) %in% ID)
  banked_country = brahms_wcvp_matched$NewCountryName[banked_row]
  match = iucn_country %in% banked_country
  if(any(match)){
    iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country[iucn_row[match]]= 1
    iucn_wcvp_matched_countries_tdwg3$accessions_per_spp_country[iucn_row[match]]= length(unique(brahms_wcvp_matched$AccessionNumber[banked_row]))
    iucn_wcvp_matched_countries_tdwg3$seeds_per_spp_country[iucn_row[match]]= sum(brahms_wcvp_matched$AdjustedSeedQuantity[banked_row])
    iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country[iucn_row[match]] = sum(brahms_wcvp_matched$plants_sampled[banked_row], na.rm = T)# iucn_wcvp_matched_countries_tdwg3[iucn_row,c("NewCountryName","banked_per_country")]
  }
}



iucn_wcvp_matched_countries_tdwg3$Target_1 = ifelse((iucn_wcvp_matched_countries_tdwg3$seeds_per_spp_country >= 1050) &
                                                      (iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country >= 50), TRUE, FALSE)


#======================================================
# find species listed based on their range criteria
# iucn_wcvp_matched_countries_tdwg3$Target_2a = grepl("B", iucn_wcvp_matched_countries_tdwg3$redlistCriteria, ignore.case=FALSE)
# iucn_wcvp_matched_countries_tdwg3$Target_2 = ifelse((iucn_wcvp_matched_countries_tdwg3$Target_2a == T) &
#                                                       (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1), T, F)
# site_counts$Target_2a = ifelse(site_counts$prop_range_banked == 1,
#                                TRUE,FALSE)
# site_counts$Target_2a[is.na(site_counts$Target_2a)] = FALSE
#
# site_counts$Target_2 = ifelse((site_counts$Target_2a & site_counts$Target_1b),
#                               TRUE,FALSE)



# add the proportion of range collected
iucn_dict = unique(data.frame(cbind(indexes$taxon_name,
                                    indexes$prop_range_banked)))
colnames(iucn_dict) = c("taxon_name","prop_range_banked")

iucn_wcvp_matched_countries_tdwg3 = iucn_wcvp_matched_countries_tdwg3 %>%
  left_join(iucn_dict, by= c("taxon_name"))

iucn_wcvp_matched_countries_tdwg3$prop_range_banked[which(is.na(iucn_wcvp_matched_countries_tdwg3$prop_range_banked))] = 0

head(iucn_wcvp_matched_countries_tdwg3)

# estimate targets

iucn_wcvp_matched_countries_tdwg3$Target_2a = ifelse((iucn_wcvp_matched_countries_tdwg3$prop_range_banked == 1) , TRUE, FALSE)

iucn_wcvp_matched_countries_tdwg3$Target_2b = ifelse((iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country >= 50) , TRUE, FALSE)

iucn_wcvp_matched_countries_tdwg3$Target_2 = ifelse((iucn_wcvp_matched_countries_tdwg3$Target_2a & iucn_wcvp_matched_countries_tdwg3$Target_2b), T, F)


#======================================================

#orthodoxy and recalcitrance
iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
                                                                      iucn_wcvp_matched_countries_tdwg3$banked_category,
                                                                      iucn_wcvp_matched_countries_tdwg3$category)
iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
                                                                    iucn_wcvp_matched_countries_tdwg3$taxonomic_prediction_level,
                                                                    iucn_wcvp_matched_countries_tdwg3$tax.level)
iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
                                                                    iucn_wcvp_matched_countries_tdwg3$banked_recalcitrance.y,
                                                                    iucn_wcvp_matched_countries_tdwg3$probability.of.recalcitrance)

# orthodox banked unbanked
iucn_wcvp_matched_countries_tdwg3$orthodox_banked = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                             iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "orthodox"), 1, 0)
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                              iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "orthodox"), 1, 0)

# recalcitrant banked unbanked
iucn_wcvp_matched_countries_tdwg3$recalcitrant_banked = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                              iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "recalcitrant"), 1, 0)
iucn_wcvp_matched_countries_tdwg3$recalcitrant_unbanked = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "recalcitrant"), 1, 0)

iucn_wcvp_matched_countries_tdwg3$orthodox_banked[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_banked)] = 0
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked)] = 0
iucn_wcvp_matched_countries_tdwg3$recalcitrant_banked[is.na(iucn_wcvp_matched_countries_tdwg3$recalcitrant_banked)] = 0
iucn_wcvp_matched_countries_tdwg3$recalcitrant_unbanked[is.na(iucn_wcvp_matched_countries_tdwg3$recalcitrant_unbanked)] = 0

###### ESTIMATE HOW MANY COUNTRIES HAVE CR species banked   #######################################################


# How many seeds and accessions per species per country
country_stats = iucn_wcvp_matched_countries_tdwg3[,c("NewCountryName",
                                                     "taxon_name",
                                                     "redlistCriteria",
                                                     "banked_per_spp_country",
                                                     "accessions_per_spp_country",
                                                     "seeds_per_spp_country",
                                                     "storage_behaviour_combined",
                                                     "orthodox_banked",
                                                     "orthodox_unbanked",
                                                     "recalcitrant_banked",
                                                     "recalcitrant_unbanked",
                                                     "plants_sampled_per_spp_country",
                                                     "Target_1",
                                                     "Target_2")] %>%
  group_by(NewCountryName) %>%
  mutate(sum_CR_pred = length(unique(taxon_name)),
         sum_CR = length(unique(taxon_name[which(redlistCriteria != "predicted")])),
         sum_pred = length(unique(taxon_name[which(redlistCriteria == "predicted")])),
         sum_CR_banked = sum(banked_per_spp_country),
         sum_Target1 = sum(Target_1),
         sum_Target2 = sum(Target_2),
         sum_orthodox = length(which(storage_behaviour_combined == "orthodox")),
         sum_recalcitrance = length(which(storage_behaviour_combined == "recalcitrant")),
         sum_orthodox_banked = sum(orthodox_banked),
         sum_recalcitrant_banked = sum(recalcitrant_banked),
         sum_orthodox_unbanked = sum(orthodox_unbanked),
         sum_recalcitrant_unbanked = sum(recalcitrant_unbanked),
         sum_accessions = sum(accessions_per_spp_country),
         sum_seeds = sum(seeds_per_spp_country)) %>%
  ungroup()
country_stats = country_stats[, c("NewCountryName",
                                  "sum_CR_pred",
                                  "sum_CR",
                                  "sum_pred",
                                  "sum_CR_banked",
                                  "sum_Target1",
                                  "sum_Target2",
                                  "sum_orthodox",
                                  "sum_recalcitrance",
                                  "sum_orthodox_banked",
                                  "sum_recalcitrant_banked",
                                  "sum_orthodox_unbanked",
                                  "sum_recalcitrant_unbanked",
                                  "sum_accessions",
                                  "sum_seeds")]
country_stats = unique(country_stats)

# save the data
write.csv(country_stats, paste0(basepath, "country_stats.csv"))




####### ADD SPATIAL DATA TO THE NAMES #############################################################################
country_names = data.frame(unique(iucn_wcvp_matched_countries_tdwg3[,"NewCountryName"]))
colnames(country_names) = "NewCountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")],
                                            by=c("NewCountryName" = "NAME_0"))

country_counts_map = country_stats %>% left_join(country_names)
country_counts_map = world %>% left_join(country_counts_map)
country_counts_map$sum_CR[is.na(country_counts_map$sum_CR)] = 0
country_counts_map$sum_CR_banked[is.na(country_counts_map$sum_CR_banked)] =0


country_counts_map$log_CR = log(country_counts_map$sum_CR+1)
country_counts_map$log_CR_banked = log(country_counts_map$sum_CR_banked+1)
country_counts_map$proportion = country_counts_map$sum_CR_banked/country_counts_map$sum_CR
country_counts_map$proportion[is.na(country_counts_map$proportion)] = 0
country_counts_map$log_proportion = log(country_counts_map$proportion+1)



# test = country_counts_map %>% left_join(permissions, by = c("NAME_0" = "Country"))
# add the permit data
country_counts_map = country_counts_map %>% left_join(permissions, by = c("NAME_0" = "NewCountryName"))


# targets per country
country_counts_map$prop_Target_1 = country_counts_map$sum_Target1/(country_counts_map$sum_CR_banked + 0.0001)
country_counts_map$prop_Target_2 = country_counts_map$sum_Target2/(country_counts_map$sum_CR_banked + 0.0001)
country_counts_map$prop_Target_1[is.na(country_counts_map$prop_Target_1)] = 0
country_counts_map$prop_Target_2[is.na(country_counts_map$prop_Target_2)] = 0

# access and benefit sharing
country_counts_map$NFP = ifelse(country_counts_map$ABS_National_Focal_Point_NFP >= 1,1,0)
country_counts_map$CNA = ifelse(country_counts_map$Competent_National_Authority_CNA >= 1,1,0)
country_counts_map$IRCC = ifelse(country_counts_map$Internationally_Recognized_Certificates_Compliance_IRCC >= 1,1,0)
country_counts_map$NR = country_counts_map$Interim_National_Reports_Implementation_Nagoya_Protocol_NR



country_counts_map$NFP[is.na(country_counts_map$NFP)] = 0
country_counts_map$CNA[is.na(country_counts_map$CNA)] = 0
country_counts_map$IRCC[is.na(country_counts_map$IRCC)] = 0
country_counts_map$NR[is.na(country_counts_map$NR)] = 0

country_counts_map$permits = (country_counts_map$NFP  +
                           country_counts_map$CNA  +
                           country_counts_map$IRCC  +
                           country_counts_map$NR)
country_counts_map$permits[is.na(country_counts_map$permits_4)] = 0

# proportion banked
country_counts_map$prop_banked = country_counts_map$sum_CR_banked/(country_counts_map$sum_CR_pred+0.001)
country_counts_map$prop_banked[is.na(country_counts_map$prop_banked)] = 0

country_counts_map$sum_orthodox_banked[is.na(country_counts_map$sum_orthodox_banked)] = 0
country_counts_map$sum_orthodox_unbanked[is.na(country_counts_map$sum_orthodox_unbanked)] = 0
country_counts_map$sum_recalcitrant_banked[is.na(country_counts_map$sum_recalcitrant_banked)] = 0
country_counts_map$sum_recalcitrant_unbanked[is.na(country_counts_map$sum_recalcitrant_unbanked)] = 0


country_counts_map$sum_CR_pred[is.na(country_counts_map$sum_CR_pred)] = 0
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
# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


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




######################################################
####    PLOT          ################################
######################################################
# values
dim=4

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#1E88E5",#509dc2", #rgb(0,150,235, maxColorValue=255),
                   upperright= "grey90",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#004D40",#"darkolivegreen4", #"black",
                   bottomright="#f3b300"# brown3"#rgb(130,0,80, maxColorValue=255)
)


custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

country_counts_map$NewCountryName[which(country_counts_map$prop_Target_2 > 0)]
#  [1] "Australia"                "Chile"                    "Cayman Islands"
# [4] "Guinea"                   "Greece"                   "Mexico"
# [7] "Saint Helena"             "Sierra Leone"             "Turks and Caicos Islands"
# [10] "South Africa"

country_counts_map$NewCountryName[which(country_counts_map$prop_Target_2 > 0.5 &
                                          country_counts_map$prop_Target_1 > 0.5)]
#  "Cayman Islands" "Sierra Leone"


data <- bi_class(country_counts_map.prj,
                 y=prop_Target_1,
                 x=prop_Target_2,
                 style = "fisher",#"jenks",#""quantile", #"equal",#
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data = data,
              aes(color =  bi_class, #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates" ) +
  geom_sf(data = data, mapping = aes(fill = bi_class),
          color = "black",#aes(fill = bi_class ),#NA,"black",#
          size = 0.8, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  bi_scale_fill(pal = custom_pal4, dim=dim,
                na.value="#e8e8e8")+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim,
                 na.value="#e8e8e8")+#,flip_axes = TRUE, rotate_pal = TRUE) +
  guides(color = "none") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", linewidth = .3) +
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
                    xlab = "% Target 2 (0-100)",
                    ylab = "     % Target 1 (0-100)",
                    size = 10)
# combine map with legend
finalPlota <- ggdraw() +
  theme(plot.background = element_rect(fill="white", color = NA))+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.73, .65, 0.35, 0.35)


finalPlota
ggsave(paste0(plotpath, "targets.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "targets.png"), width = 30, height = 12, units = "cm")


#############################
### PERMIT LEVELS
#############################

dim = 4

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#596ea7",#509dc2", #rgb(0,150,235, maxColorValue=255),
                   upperright= "grey90",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#214748",#"darkolivegreen4", #"black",
                   bottomright="#62a06d"# brown3"#rgb(130,0,80, maxColorValue=255)
)
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#1E88E5",#509dc2", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "grey90",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#004D40",#"darkolivegreen4", #"black",
#                    bottomright="#f3b300"# brown3"#rgb(130,0,80, maxColorValue=255)
# )



custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))


data <- bi_class(country_counts_map.prj,
                 y=permits,
                 x=prop_banked,#'sum_CR_banked,
                 style = "equal",#"jenks",#""quantile", #"fisher",#
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data = data,
              aes(color =  bi_class, #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates" ) +
  geom_sf(data = data, mapping = aes(fill = bi_class),
          color = "black",#aes(fill = bi_class ),#NA,"black",#
          size = 0.8, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  bi_scale_fill(pal = custom_pal4, dim=dim,
                na.value="#e8e8e8")+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim,
                 na.value="#e8e8e8")+#,flip_axes = TRUE, rotate_pal = TRUE) +
  guides(color = "none") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", linewidth = .3) +
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
                    xlab = "CR banked (0-72.4%)",
                    ylab = "   ABS (0-4)",
                    size = 10)
# combine map with legend
finalPlotb <- ggdraw() +
  theme(plot.background = element_rect(fill="white", color = NA))+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.73, .65, 0.35, 0.35)


finalPlotb
ggsave(paste0(plotpath, "ABS_CRbanked.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "ABS_CRbanked.png"), width = 30, height = 12, units = "cm")


#############################
### orthodoxy
#############################

dim = 4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#1E88E5",#509dc2", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "grey90",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#004D40",#"darkolivegreen4", #"black",
#                    bottomright="#f3b300"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#3970b3",#6eabbd", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft= "#301d39",#"#554249", #"black",
                   bottomright="#9c263c"#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
)

# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#1E88E5",#509dc2", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "grey90",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#004D40",#"darkolivegreen4", #"black",
#                    bottomright="#f3b300"# brown3"#rgb(130,0,80, maxColorValue=255)
# )


custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(country_counts_map.prj,
                 y=sum_orthodox_unbanked,
                 x=sum_recalcitrant_unbanked,
                 style = "fisher",#"equal",#"jenks",#""quantile", #
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data = data,
              aes(color =  bi_class, #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates" ) +
  geom_sf(data = data, mapping = aes(fill = bi_class),
          color = "black",#aes(fill = bi_class ),#NA,"black",#
          size = 0.8, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  bi_scale_fill(pal = custom_pal4, dim=dim,
                na.value="#e8e8e8")+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim,
                 na.value="#e8e8e8")+#,flip_axes = TRUE, rotate_pal = TRUE) +
  guides(color = "none") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", linewidth = .3) +
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
                    xlab = paste0("# recalcitrant (0-",max(data$sum_recalcitrant_unbanked),")"),
                    ylab = paste0("    # orthodox (0-",max(data$sum_orthodox_unbanked),")"),
                    size = 10)

# combine map with legend
finalPlotc <- ggdraw() +
  theme(plot.background = element_rect(fill="white", color = NA))+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.73, .65, 0.35, 0.35)

finalPlotc

ggsave(paste0(plotpath, "unbanked_CR_orth_recalc.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "unbanked_CR_orth_recalc.png"), width = 30, height = 12, units = "cm")


#############################
### banked vs unbanked
#############################

dim = 4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#1E88E5",#509dc2", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "grey90",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#004D40",#"darkolivegreen4", #"black",
#                    bottomright="#f3b300"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#3970b3",#6eabbd", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft= "#301d39",#"#554249", #"black",
#                    bottomright="#9c263c"#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
# )

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#1E88E5",#509dc2", #rgb(0,150,235, maxColorValue=255),
                   upperright= "grey90",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#004D40",#"darkolivegreen4", #"black",
                   bottomright="#f3b300"# brown3"#rgb(130,0,80, maxColorValue=255)
)


custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(country_counts_map.prj,
                 y = sum_CR_pred ,
                 x = prop_banked,
                 style = "fisher",#"quantile", #"equal",#"fisher",#
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data = data,
              aes(color =  bi_class, #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates" ) +
  geom_sf(data = data, mapping = aes(fill = bi_class),
          color = "black",#aes(fill = bi_class ),#NA,"black",#
          size = 0.8, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  bi_scale_fill(pal = custom_pal4, dim=dim,
                na.value="#e8e8e8")+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim,
                 na.value="#e8e8e8")+#,flip_axes = TRUE, rotate_pal = TRUE) +
  guides(color = "none") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", linewidth = .3) +
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
                    xlab = "    CR banked (0-72.4%)",
                    ylab = " CR (0-536)",
                    size = 10)

# combine map with legend
finalPlotd <- ggdraw() +
  theme(plot.background = element_rect(fill="white", color = NA))+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.73, .65, 0.35, 0.35)

finalPlotd
ggsave(paste0(plotpath, "CR_banked_prop.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "CR_banked_prop.png"), width = 30, height = 12, units = "cm")


############################
# Tiled plots
############################
ggarrange(finalPlotc,finalPlotb, finalPlota,
          labels = c("a.", "b.", "c."),
          font.label = list(size = 30),
          ncol = 1, nrow = 3)

ggsave(paste0(plotpath, "maps_abc.pdf"),  width = 30, height = 36, units = "cm")
ggsave(paste0(plotpath, "maps_abc.png"),  width = 30, height = 36, units = "cm",bg="white")




ggarrange(finalPlotd,finalPlota, finalPlotb,finalPlotc,
          labels = c("a.", "b.", "c.", "d."),
          font.label = list(size = 30),
          ncol = 2, nrow = 2)

ggsave(paste0(plotpath, "maps_abcd.pdf"),  width = 60, height = 24, units = "cm")
ggsave(paste0(plotpath, "maps_abcd.png"),  width = 60, height = 24, units = "cm",bg="white")



##########################################################
##### PLOT CR SPECIES
##########################################################

data = country_counts_map.prj

# number of CR species
map1 <- ggplot() +
  geom_point( data= data,
              aes(color =  log_CR,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates") +
  geom_sf(data = data, mapping = aes(fill = log_CR),
          color = "black", #aes(fill = log_CR),
          size = 0.4, show.legend = FALSE) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+

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
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )


map1 + guides(color = "none")

legend <- cowplot::get_legend(map1 +
                                guides(color = "none",
                                       fill=guide_legend(title="% CR banked"),
                                       override.aes = list(size = 0.5))
)
finalPlot1 <- ggdraw() +
  draw_plot(map1, 0, 0, 1, 1) +
  draw_plot(legend, 0.9, .65, 0.24, 0.24)


finalPlot1
ggsave(paste0(plotpath, "map_CR.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "map_CR.png"), width = 30, height = 12, units = "cm")



#####################################
### PROPORTION BANKED
#-------------------------------------------------------


map2 <- ggplot() +
  geom_point( data= data,
              aes(color =  proportion,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates") +
  geom_sf(data = data, mapping = aes(fill = proportion),
          color = "black",#aes(fill = proportion),
          size = 0.4, show.legend = FALSE) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Greens"))+
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Greens"))+
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
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )


map2 + guides(color = "none")

legend <- cowplot::get_legend(map2 +
                                guides(color = "none",
                                       fill=guide_legend(title="% CR banked"),
                                       override.aes = list(size = 0.5))
)
finalPlot2 <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend, 0.9, .65, 0.24, 0.24)


finalPlot2


ggsave(paste0(plotpath, "map_proportion_banked.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "map_proportion_banked.pdf"), width = 30, height = 12, units = "cm")




ggarrange(finalPlot1, finalPlot2,
          labels = c("a.", "b."),
          font.label = list(size = 30),
          ncol = 1, nrow = 2)

ggsave(paste0(plotpath, "maps.pdf"),  width = 30, height = 30, units = "cm")
ggsave(paste0(plotpath, "maps.png"),  width = 30, height = 30, units = "cm",bg="white")




#####################################
### PERMISSIONS
#-------------------------------------------------------

data = country_counts_map.prj


# Create the main map plot
map1 <- ggplot() +
  geom_point(data = data,
             aes(color = as.factor(ABS_National_Focal_Point_NFP),
                 geometry = geometry),
             size = 2,
             stat = "sf_coordinates") +
  geom_sf(data = data, mapping = aes(fill = as.factor(ABS_National_Focal_Point_NFP)),
          color = "black",
          size = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = c("grey80", "deepskyblue4"), na.value = "grey50") +
  scale_color_manual(values = c("grey80", "deepskyblue4"), na.value = "grey50") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))

# Add the guides for color and fill with the new title
map1 <- map1 +
  guides(color = guide_legend(title = "NFP"),
         fill = guide_legend(title = "NFP"))

# Extract the legend
legend <- cowplot::get_legend(map1)

# Combine the map plot and the legend
finalPlot1 <- ggdraw() +
  draw_plot(map1 + guides(color = "none", fill = "none"), 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot1


ggsave(paste0(plotpath, "NFP.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "NFP.png"), width = 30, height = 12, units = "cm", bg= "white")


############
# CNA
############

# Create the main map plot
map2 <- ggplot() +
  geom_point(data = data,
             aes(color = as.factor(ifelse(Competent_National_Authority_CNA >= 1,1,0)),
                 geometry = geometry),
             size = 2,
             stat = "sf_coordinates") +
  geom_sf(data = data, mapping = aes(fill =as.factor(ifelse(Competent_National_Authority_CNA >= 1,1,0))),
          color = "black",
          size = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = c("grey80", "deepskyblue4"), na.value = "grey50") +
  scale_color_manual(values = c("grey80", "deepskyblue4"), na.value = "grey50") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))

# Add the guides for color and fill with the new title
map2 <- map2 +
  guides(color = guide_legend(title = "CNA"),
         fill = guide_legend(title = "CNA"))

# Extract the legend
legend <- cowplot::get_legend(map2)

# Combine the map plot and the legend
finalPlot2 <- ggdraw() +
  draw_plot(map2 + guides(color = "none", fill = "none"), 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot2


ggsave(paste0(plotpath, "CNA.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "CNA.png"), width = 30, height = 12, units = "cm", bg= "white")

############
# IRCC
############

# Create the main map plot
map3 <- ggplot() +
  geom_point(data = data,
             aes(color = as.factor(ifelse(Internationally_Recognized_Certificates_Compliance_IRCC >=1,1,0)),
                 geometry = geometry),
             size = 2,
             stat = "sf_coordinates") +
  geom_sf(data = data, mapping = aes(fill = as.factor(ifelse(Internationally_Recognized_Certificates_Compliance_IRCC >=1,1,0))),
          color = "black",
          size = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = c("grey80", "deepskyblue4"), na.value = "grey50") +
  scale_color_manual(values = c("grey80", "deepskyblue4"), na.value = "grey50") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))

# Add the guides for color and fill with the new title
map3 <- map3 +
  guides(color = guide_legend(title = "IRCC"),
         fill = guide_legend(title = "IRCC"))

# Extract the legend
legend <- cowplot::get_legend(map3)

# Combine the map plot and the legend
finalPlot3 <- ggdraw() +
  draw_plot(map3 + guides(color = "none", fill = "none"), 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot3


ggsave(paste0(plotpath, "IRCC.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "IRCC.png"), width = 30, height = 12, units = "cm", bg= "white")

############
# IRCC
############

# Create the main map plot
map4 <- ggplot() +
  geom_point(data = data,
             aes(color = as.factor(Interim_National_Reports_Implementation_Nagoya_Protocol_NR),
                 geometry = geometry),
             size = 2,
             stat = "sf_coordinates") +
  geom_sf(data = data, mapping = aes(fill = as.factor(Interim_National_Reports_Implementation_Nagoya_Protocol_NR)),
          color = "black",
          size = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = c("grey80", "deepskyblue4"), na.value = "grey50") +
  scale_color_manual(values = c("grey80", "deepskyblue4"), na.value = "grey50") +
  bi_theme() +
  geom_path(data = grid.DT,
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))

# Add the guides for color and fill with the new title
map4 <- map4 +
  guides(color = guide_legend(title = "NR"),
         fill = guide_legend(title = "NR"))

# Extract the legend
legend <- cowplot::get_legend(map4)

# Combine the map plot and the legend
finalPlot4 <- ggdraw() +
  draw_plot(map4 + guides(color = "none", fill = "none"), 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot4


ggsave(paste0(plotpath, "NR.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "NR.png"), width = 30, height = 12, units = "cm", bg= "white")


ggarrange(finalPlot1, finalPlot2,finalPlot3,finalPlot4,
          labels = c("a.", "b.", "c.","d."),
          font.label = list(size = 30),
          ncol = 2, nrow = 2)

ggsave(paste0(plotpath, "permits_seperate.pdf"),  width = 60, height = 30, units = "cm")
ggsave(paste0(plotpath, "permits_seperate.png"),  width = 60, height = 30, units = "cm",bg="white")




###################################################################################################################
#######  ADD STATS   ##############################################################################################
###################################################################################################################


# number of CR species
length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name[which(iucn_wcvp_matched_countries_tdwg3$redlistCriteria != "prediction")]))
length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name[which(iucn_wcvp_matched_countries_tdwg3$redlistCriteria == "prediction")]))
length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name))

# number of countries
length(unique(iucn_wcvp_matched_countries_tdwg3$NewCountryName[which(iucn_wcvp_matched_countries_tdwg3$redlistCriteria != "prediction")]))
length(unique(iucn_wcvp_matched_countries_tdwg3$NewCountryName[which(iucn_wcvp_matched_countries_tdwg3$redlistCriteria == "prediction")]))
length(unique(iucn_wcvp_matched_countries_tdwg3$NewCountryName))

# countries with more than 10 CR species
country_data2 = data.frame(country_counts_map.prj)
country_data2 = country_data2[,1:(ncol(country_data2)-1)]
country_data2$NewCountryName[country_data2$sum_CR_pred >= 10]
length(unique(country_data2$NewCountryName[country_data2$sum_CR_pred < 10]))

# countries with more than 100CR species
country_data2$NewCountryName[country_data2$sum_CR_pred >= 150]
country_data2$NewCountryName[country_data2$sum_CR_pred >= 200]

# countries with the highest % of CR species
country_stats$prop_CR = country_stats$sum_CR_pred/length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name))
test = country_stats %>%
  arrange(desc(prop_CR)) %>%  # arrange in descending order
  slice(1:12)
test[,c("NewCountryName","prop_CR","sum_CR_pred", "sum_CR")]

# countries with over 50% of spp banked
country_data2$NewCountryName[country_data2$prop_banked >= 0.5]
country_data2$sum_CR_banked[country_data2$NewCountryName %in% country_data2$NewCountryName[country_data2$prop_banked >= 0.5]]

#countries with highest proportion meeting target 1
country_data2$NewCountryName[country_data2$prop_Target_1 >= 0.9]
length(unique(country_data2$NewCountryName[country_data2$prop_Target_1 <= 0.1]))

#countries with highest proportion meeting target 2
country_data2$NewCountryName[country_data2$prop_Target_2 >= 0.9]
length(unique(country_data2$NewCountryName[country_data2$prop_Target_2 <= 0.1]))

# countries that meet both targets
country_data2$NewCountryName[country_data2$prop_Target_2 >= 0.9 & country_data2$prop_Target_1 >= 0.9]

# nunmber pf countries with each permission level
sum(country_data2$NFP)
sum(country_data2$CNA)
sum(country_data2$IRCC)
sum(country_data2$NR)

# countries with all four ABS
unique(country_data2$NewCountryName[country_data2$permits == 4])


#####################################################################################################################
#####################################################################################################################
##      STATS FOR SOTAGE BEHAVIOUR
#####################################################################################################################
#####################################################################################################################

### For unbanked

test = iucn_wcvp_matched_countries_tdwg3[which(iucn_wcvp_matched_countries_tdwg3$category != "banked"),]
unbanked_names = length(unique(test$taxon_name))

length(unique(test$taxon_name[which(test$storage_behaviour_combined == "orthodox")]))
length(unique(test$taxon_name[which(test$storage_behaviour_combined == "intermediate")]))
length(unique(test$taxon_name[which(test$storage_behaviour_combined == "recalcitrant")]))
length(unique(test$taxon_name[which(test$storage_behaviour_combined == "exceptional")]))

length(unique(test$taxon_name[which(test$storage_behaviour_combined == "orthodox")]))/ unbanked_names
length(unique(test$taxon_name[which(test$storage_behaviour_combined == "intermediate")]))/ unbanked_names
length(unique(test$taxon_name[which(test$storage_behaviour_combined == "recalcitrant")]))/ unbanked_names
length(unique(test$taxon_name[which(test$storage_behaviour_combined == "exceptional")]))/ unbanked_names


length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name))
# number of species in each category
summary(as.factor(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined))

#taxonomic level that predictions are made at for orthodox, intermediate and recalcitrant
summary(as.factor(iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined[which(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "orthodox")]))
summary(as.factor(iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined[which(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "intermediate")]))
summary(as.factor(iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined[which(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "recalcitrant")]))
summary(as.factor(iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined[which(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "exceptional")]))

hist(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Family")])
summary(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Family")])
length(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Family")])

hist(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Genus")])
summary(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Genus")])
length(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Genus")])


hist(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Order")])
summary(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Order")])
length(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Order")])

hist(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Species")])
summary(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Species")])
length(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which (iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Species")])


# how many species are classed as intermediate (uncertain) at different taxonomic levels
length(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which(iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Order" &
                                                                             iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "intermediate")])

length(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which(iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Genus" &
                                                                             iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "intermediate")])

length(iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined[which(iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined == "Family" &
                                                                             iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "intermediate")])

## exceptional species characteristics
length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name[which(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "exceptional" &
                                                            iucn_wcvp_matched_countries_tdwg3$EF1_seed_unavailable == "Yes")]))

length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name[which(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "exceptional" &
                                                                   iucn_wcvp_matched_countries_tdwg3$EF2_desiccation_sensitive == "Yes")]))


length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name[which(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "exceptional" &
                                                                   iucn_wcvp_matched_countries_tdwg3$EF3_short.lived == "Yes")]))

length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name[which(iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "exceptional" &
                                                                   iucn_wcvp_matched_countries_tdwg3$EF4_deep_dormancy == "Yes")]))


########################################################################################################
########################################################################################################
# plot a tree with all orthodoxy by family
########################################################################################################
########################################################################################################


# Load packages
library(ape)
library(ggtree)
library(tidyverse)
library(ggtreeExtra)
library(phyloseq)
library(dplyr)
library(ggplot2)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"
plotpath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code"
# load iucn data
# iucn_banked_recalitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
iucn_wcvp_matched_countries_tdwg3


# Read the phylogenetic tree from Zuntini
tree <- read.tree("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Trees/Trees/2_global_family_level.tre")

# load tree data
tr <- tree
numtip <- length(tr$tip.label)

# Get number of species per family that are CR and that are banked
fam_count = iucn_wcvp_matched_countries_tdwg3[,c("family", "storage_behaviour_combined", "taxonomic_level_combined")] %>%
  group_by(family) %>%
  summarize(
    n_orthodox = length(which(storage_behaviour_combined == "orthodox")),
    n_recalc = length(which(storage_behaviour_combined == "recalcitrant")),
    n_inter = length(which(storage_behaviour_combined == "intermediate")),
    n_exceptional = length(which(storage_behaviour_combined == "exceptional")),
    n_unknown = length(which(is.na(storage_behaviour_combined))),
    p_orthodox = length(which(storage_behaviour_combined == "orthodox"))/n(),
    p_recalc = length(which(storage_behaviour_combined == "recalcitrant"))/n(),
    p_inter = length(which(storage_behaviour_combined == "intermediate"))/n(),
    p_exceptional = length(which(storage_behaviour_combined == "exceptional"))/n(),
    p_unknown = length(which(is.na(storage_behaviour_combined)))/n(),
    tot = n()
  )

test = data.frame(tr$tip.label) %>% left_join(fam_count,
                                              by = c("tr.tip.label" = "family"))


colorz  = ifelse(test$p_orthodox >= 0.5, "#648FFF",
                 ifelse(test$p_recalc >= 0.5, "#DC267F",
                        ifelse(test$p_inter >= 0.5,"#785EF0",
                               ifelse(test$p_exceptional >= 0.5,"#FFB000", "grey85"))))
colorz[is.na(colorz)] = "grey85"



dat = rbind(data.frame(id = test$tr.tip.label,
                       group = "Orthodox",
                       value = test$p_orthodox,
                       colour = colorz),
            data.frame(id = test$tr.tip.label,
                       group = "Recalcitrant",
                       value = test$p_recalc,
                       colour = colorz),
            data.frame(id = test$tr.tip.label,
                       group = "Intermediate",
                       value = test$p_inter,
                       colour = colorz),
            data.frame(id = test$tr.tip.label,
                       group = "Exceptional",
                       value = test$p_exceptional,
                       colour = colorz),
            data.frame(id = test$tr.tip.label,
                       group = "No CR species in family",
                       value = ifelse(is.na(test$p_unknown),1,test$p_unknown),
                         # ifelse(is.na(test$tot),1,0),
                       colour = colorz))



##########################################
### GET STATS
##########################################
#
length(test$tr.tip.label[which(test$p_orthodox >=0.5)])/length(which(test$p_unknown != 1))
length(test$tr.tip.label[which(test$p_recalc >=0.5)])
length(test$tr.tip.label[which(test$p_inter >=0.5)])
length(test$tr.tip.label[which(test$p_exceptional >=0.5)])
test$tr.tip.label[which(test$p_exceptional >=0.5)]


# proportions = dat$value[dat$group == "Banked CR species in family"]
#
# # % families with CR
# length(which(!is.na(proportions)))/length(proportions) *100
#
# # % families with banked CR
# length(which(proportions > 0))/length(proportions) *100
#
# # % CR that have some banked
# length(which(proportions > 0))/length(which(!is.na(proportions))) *100
#
# # % CR that have 50% banked
# length(which(proportions > 0.50))/length(which(!is.na(proportions))) *100
#
# # % CR that have 99% banked
# length(which(proportions > 0.99))/length(which(!is.na(proportions))) *100
#
# # Names of fanmilies with CR that are 99% banked
# dat$id[which(dat$group == "Banked CR species in family" & dat$value >0.99)]
#
# # Stats for big families
# test$CR_species[test$tr.tip.label == "Rubiaceae"]
# test$CR_species[test$tr.tip.label == "Myrtaceae"]
# test$CR_species[test$tr.tip.label == "Lauraceae"]
# test$CR_species[test$tr.tip.label == "Fabaceae"]
# test$CR_species[test$tr.tip.label == "Orchidaceae"]
# test$CR_species[test$tr.tip.label == "Asteraceae"]

#############################################
# Plot the phylogenetic tree
#############################################

p <- ggtree(tr, layout = "circular") +
  xlim(-10, 70) +
  geom_fruit(data = dat,
             geom = geom_bar,
             mapping = aes(y = id, x = value, fill = group),
             pwidth = 0.5,
             stat = "identity",
             orientation = "y",
             offset = 0.05) +
  scale_fill_manual(values = c( "#FFB000","#785EF0","grey85","#648FFF", "#DC267F"),
    # "darkolivegreen", "grey", "#FFA500"),
                    name = "")

# Extract tip labels from the tree data
tip_data <- p$data %>% filter(isTip) %>% left_join(dat, by = c("label" = "id"))

p <- p %<+% tip_data +
  aes(color = colour) +
  geom_tiplab(aes(label=label), offset=24, size=2) +
  scale_color_identity() +
  # scale_colour_manual(values = c("darkolivegreen", "grey", "#FFA500", "black"))
  scale_colour_manual(values = c("#648FFF","#785EF0","#DC267F","#FFB000","grey85"),
                        #c("#648FFF","#DC267F","grey","#FFB000", "#785EF0", "black"),
                      labels = c("Exceptional", "Intermediate", "Unknown", "Orthodox", "Recalcitrant", "NA")) +
  guides(colour = "none")

print(p)

ggsave(paste0(plotpath, "/phylo_orthodoxy_recalc.pdf"),
       width = 20,
       height = 20,
       units = "cm")

ggsave(paste0(plotpath, "/phylo_orthodoxy_recalc.png"),
       width = 25,
       height = 25,
       units = "cm")


