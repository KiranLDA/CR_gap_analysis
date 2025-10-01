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
# plotpath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/"
plotpath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/plots/revision_1/"


wcvp_countries <- read.table(paste0(basepath, "revision_1/wcvp_downloaded_17_09_2025/wcvp_distribution.csv" ),
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

indexes = read.csv(paste0(basepath,"revision_1/iucn_brahms_indexes_targets.csv"), encoding = "UTF-8")

permissions = read.csv(paste0(basepath,"ABSCH-Country-List_03_07_24.csv"))

iucn_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched.csv"), encoding = "UTF-8")


brahms_wcvp_matched = read.csv(paste0(basepath, "revision_1/brahms_wcvp_matched_full_name_infra.csv"), encoding = "UTF-8")
brahms_wcvp_matched <- brahms_wcvp_matched[which(!is.na(brahms_wcvp_matched$taxon_name)),]

brahms_wcvp_matched = brahms_wcvp_matched %>% left_join(indexes[,c("ACCESSION",
                                                                   # "full_name", #"ADJSTCOUNT", "PLANTSAMP",
                                                                   # "Cultivated", #CULTIVATED
                                                                   # "Derived.From",
                                                                   # "Result",
                                                                   # "DateStarted", # ENTRYDATE
                                                                   # "DateCollected",
                                                                   # "DateDonated", # DONORDATE
                                                                   # "DateGermplasmBanked",
                                                                   "geographic_index", "taxonomy_index",
                                                                   "year_index", "information_index", "count_index", "adjcount_index",
                                                                   "germination_index", "viability_index", "cultivation_index",
                                                                   "exsitu_index", "genetic_index", "total_index",
                                                                   "collections",
                                                                   "collections_per_tdwg","collections_per_country",
                                                                   "collections_per_native_country",  "proportion_native_country",
                                                                   "proportion_collections_native_country",
                                                                   # "prop_range_banked",
                                                                   "Target_1a", "Target_1b", "Target_1",
                                                                   "Target_2a", "Target_2b", "Target_2")],
                                                        by = "ACCESSION")


brahms_wcvp_matched$plants_sampled = as.numeric(brahms_wcvp_matched$PLANTSAMP)
unique(brahms_wcvp_matched$PLANTSAMP[which(is.na(brahms_wcvp_matched$plants_sampled) & brahms_wcvp_matched$PLANTSAMP != "")])


brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("Ldg" ,"many","unknown","Many","`" ,"?","Hard to te"))] = NA
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("Single", "A clump","1?","10 berries","1 patch","1 ale","Ramet 1","Ale 1","1 clump"))]=1
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">2","2 patches","~2","2 total","2 ale"))] = 2
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("Up to 3","3 clumps","2-5?", "a few","3 patches","2 to 5","to 0.3","3?","3 ale",
                                                                     "3 (erraboi","~3","2 or 4?" ))] = 3
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("5+",  "5-1 0", ">5", ">3", "5- 1 0" ,"1-10??","c. 5",". 5","4 to 7",">5?", "~ 5","6 minimum",
  "<5","5 minimum","4 minimum",">4","~5",">  5", "5 ale","4 or 5" ))] = 5
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("6+","6 mats",">6","At least 6","8 patches","~6","7 flower s",
                                                                     "6 clumps","6 colonies","6 (3 from","c.8","~7"))] = 6
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("~10",  ">10", "10+" , "<10", "c8","~10?" ,">10?","ca. 10","< 10","~8","Several","> 10","11+",". 10","10 to 12","10?","ca 10","c 10","Approx. 8",
  "c. 10","13 in 4 su","10 in 4 su","10 minimum","7 minimum","8 minimum","c10",">  10",">7","10x","8 ale","9 ale","10 ale","~11","10s",
  "10 heads","8+","9 patches"))] = 10
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("~15", ">15","10-20?","15+","12+",">12","c. 15","<15","~15?","~12","Minimum 12","c 15","16>","15>","~18","ca 15","4-20?", "14 ejempla","16+2berde",
  "14 (errabo","12 ale","13 eskapo",">14","<16","~17","15 clumps","12 (see no")) ]= 15
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("~20", ">20", "20+" , "20-30" ,"1 0-25", "5-50?","20 minimum","c.20","20 to 30","15-25","<20",">21","> 20",">18","18>","~21",". 20","0-25","22 clumps",
  "12-24??","18 to 20","15-20","20-25","ca. 20","~25" ,"20clumps",">20?","c. 20","20 clumps","c20",">22","20?","?20","Approx. 20",
  "2-40?","c 20","20 ale","21 ale","25 gara","23 (errabo", "22 (errabo","17 ejempla","20 gara","26 (errabo","21 minimum","15 to 25",
  "approx. 25","20 spots","20 patches",">24"))] =20
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">30","> 30","<30","~30", "25 -50","30-40", "10-50?","30 to 40","ca 30",". 30","25- 50","35 shoots","25-49","29 minimum","26 minimum",
  "~35","30+","1-50?",">25","c. 30" ,"25-30","?>30","20-40","25+","c30","30 clumps","30 minimum","27 minimum","25 minimum","30 ale" ,
  "~26","~33","15-30","25-51","30>","28>","25>","15-40","Approx. 30","25-40","20-35","c.28","15-50","31+","30?",">36","36 hazi 1",
  "35 (errabo",  "25 ale azh","c.30","ca. 30","ca 35", ">23","30-35","c.35","~30 patche"))] =30
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">40", "31-45", "30-45", "40+", "25 - 50", "20-50", "40-50",">35","30-50","35-47", "~45","40 minimum","<40",
  "c. 40","40 shoots","40>","42 clumps","30-45 ale","c 40","c40+","c40","35-40","35+","25-50"))] = 40
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">50", ". 50","~50", "10-100","<50", "25-99" ,"ca. 50","c. 50","50 female","c.50","5-100",">50?","2-100", "45>","50>","25-100","35-75","45-60","20-70",
  "50+" ,"11-100", "40-90","40-60","30-60","40-100","> 50","c50",">45","50 minimum","30-70","~55","Approx. 50","50?","24-49","c 50",">50 ale",
  "50 ale","51 (errabo","50 gara", "50 eskapo", "ca 50","45+","20-60"))] = 50
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("50-70",  "62 (57+5)", ">60", "60+","50-60","50-75","~60","50-80","60 minimum","30-90",">55","55>",">58",">56","60>",">54",
  "c. 60","57 total 6", "c60","c.60" ))] = 60
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("50-100", "50 - 100", "50 -100", "60-70", "50-99", ">75", "~70",">70","~76" ,"50 to 80","70-80","70+","<70",">64","43 & 27 sp","60-80","~75","65+",
  "76 (errabo","50-101","Approx. 75",">68"))] = 70
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">80","75+","~80","ca. 80","80+", "78/65",">78",">76","80>",
                                                                           "70-100","75>","75-80","<80","75-100",">50-100","~70-90","ca 80","c80"))] = 80
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("~90","85+","80-100", "c. 88",">83","90+","82>","85>","92 eskapo","<90","70/20","c90","c.90"))] = 90
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("100+", ">100", "50-170" , "100-", ">90" ,"30-150?","~100","100-135","<100","100?","c100","91-100","~120 spike",">97",">124",">115",">128","100+?",
  ">100.","100's","> 100","~130", "100-99","100s",">95","<115","110>","100>",">>100","100<100 -","100 - 100","c. 100", "c.110","75-150","110 gara",
  "100 eskapo","100-120",">100?","~120","<140", "ca 100","ca. 100","cca. 100","c.100","30 & 100",">110","99+")) ] = 100
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("100 - 200", "100-200","100 -200", ">150", ">130", ">140","120+","~150","150-200","ca. 150","150+","100-150" ,"100 -150",">150<200","130-140",">120",
  "> 150","145+",">160","150 gara","~160"))] = 150
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">200", "c.200", "100 - 300", ">180","210+","200+","~200","~220","~180",
                                                                              "> 200","50-300",">>200","200 inflor","176-200","100-300","approx 200",
                                                                              "189 patche","aprox.200","~170 spike","200 minimu","100 -300","c200"))]  = 200
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">100<500", ">250", ">260","250+","250-300","100-500","100 - 500","~250","200-300","ca 250","~240") )] = 250
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("300+", "100 -500", ">300","~300","c. 300","300-400",">270","200-400","200-500","300-400 st",
                                                                           "> 300","330+","c300","300 minimu"))] = 300
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("c.400", ">400", "300 -500" ,"400+","~40","350+","~400",">350","350-500","400-500"))] = 400
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">500","100-1000","100-999", "500-600", "100 - 1000" ,"500+", "~500","c. 500","ca 500","50 - 1000","480+","520+"))] = 500
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">600" , "600+","~600","550+"))] = 600
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">660" ))] = 660
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("700+", ">500<1000","500-1000",">700","Several hu"))] = 700
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("500-1,000",">790","750+",">750",">800"))] = 750
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("800+"))] = 800
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("1000+",">1000",">999",">909","1000 +","thousands", ">1000 infr"))] = 1000
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("1200-1500","~1200", "999+","1500+","~1500"))]=1500
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("1500-2000",">2000","2000+"))] =2000
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c("3000+"))] = 3000
brahms_wcvp_matched$plants_sampled[which(brahms_wcvp_matched$PLANTSAMP %in% c(">5000"))] = 5000



iucn_predictions_wcvp_matched = read.csv(paste0(basepath, "revision_1/iucn_predictions_wcvp_matched.csv"),encoding = "UTF-8")
iucn_predictions_wcvp_matched = iucn_predictions_wcvp_matched[which(iucn_predictions_wcvp_matched$category == "CR"),]


tdwg3_countries <- read.csv(paste0(basepath, "country_tdwg3_mapping.csv"))
tdwg3_countries$ISO_code[is.na(tdwg3_countries$ISO_code)] ="NA"





spp_banked_recalcitrant <- read.csv(paste0(basepath,"revision_1/spp_banked_recalcitrant.csv"), encoding = "UTF-8")

spp_banked_recalcitrant = spp_banked_recalcitrant %>% left_join(brahms_wcvp_matched[,c("taxon_name","wcvp_accepted_id")],
                                                                by = c("taxon_name","wcvp_accepted_id"))
spp_banked_recalcitrant = spp_banked_recalcitrant %>% left_join(iucn_wcvp_matched[,c("taxon_name","wcvp_accepted_id")],
                                                                by = c("taxon_name","wcvp_accepted_id"))
spp_banked_recalcitrant = spp_banked_recalcitrant %>% left_join(iucn_predictions_wcvp_matched[,c("taxon_name","wcvp_accepted_id")],
                                                                by = c("taxon_name","wcvp_accepted_id"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add the ID to the recalcitrant species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# spp_banked_recalcitrant$wcvp_accepted_id <- ifelse(is.na(spp_banked_recalcitrant$wcvp_accepted_id),
#                                                    ifelse(is.na(spp_banked_recalcitrant$wcvp_accepted_id.y),
#                                                           spp_banked_recalcitrant$wcvp_accepted_id.x,
#                                                           spp_banked_recalcitrant$wcvp_accepted_id.y),
#                                                    spp_banked_recalcitrant$wcvp_accepted_id)
# spp_banked_recalcitrant$wcvp_accepted_id <- ifelse(is.na(spp_banked_recalcitrant$wcvp_accepted_id.y),
#                                                           spp_banked_recalcitrant$wcvp_accepted_id.x,
#                                                           spp_banked_recalcitrant$wcvp_accepted_id.y)

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
# brahms_wcvp_matched$NewCountryName = brahms_wcvp_matched$CountryName
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
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Trinidad & Tobago"] = "Trinidad and Tobago"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "North Macedonia" ] = "Macedonia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "\tMacedonia"] = "Macedonia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Saint Helena, Ascension, and Tristan da Cunha"] = "Saint Helena"


#putting territories with their countries
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Christmas I."] = "Australia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Norfolk Island"] = "Australia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "British Virgin Is."] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "South Georgia and the Islands"] = "United Kingdom"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "British Virgin Islands" ] = "United Kingdom"
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
permissions$NewCountryName[which(permissions$NewCountryName == "Iran (Islamic Republic of)")] = "Iran"
permissions$NewCountryName[which(permissions$NewCountryName == "Lao People's Democratic Republic")] = "Laos"
permissions$NewCountryName[which(permissions$NewCountryName == "Micronesia (Federated States of)" )] = "Micronesia"
permissions$NewCountryName[which(permissions$NewCountryName == "Netherlands (Kingdom of the)")] = "Netherlands"
permissions$NewCountryName[which(permissions$NewCountryName == "North Macedonia")] = "Macedonia"
permissions$NewCountryName[which(permissions$NewCountryName == "Republic of Korea")] = "South Korea"
permissions$NewCountryName[which(permissions$NewCountryName == "Republic of Moldova")] = "Moldova"
permissions$NewCountryName[which(permissions$NewCountryName == "Russian Federation")] = "Russia"
permissions$NewCountryName[which(permissions$NewCountryName == "Sao Tome and Principe")] = "São Tomé and Príncipe"
permissions$NewCountryName[which(permissions$NewCountryName == "State of Palestine")] = "Palestine"
permissions$NewCountryName[which(permissions$NewCountryName == "Syrian Arab Republic")] = "Syria"
permissions$NewCountryName[which(permissions$NewCountryName == "Timor-Leste")] = "East Timor"
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
iucn_wcvp_matched_countries_tdwg3$banked = (iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id %in%
                                              brahms_wcvp_matched$wcvp_accepted_id)
# (data.frame(banked = iucn_wcvp_matched_countries_tdwg3$banked,
#             id = iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id ))[451:501,]
# CR species
# iucn_wcvp_matched_countries_tdwg3$banked_per_country


# # # save the data
#
# iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country = 0
# iucn_wcvp_matched_countries_tdwg3$accessions_per_spp_country = 0
# iucn_wcvp_matched_countries_tdwg3$seeds_per_spp_country = 0
# iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country = 0
#
# for(ID in unique(iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id)){
#   # ID = 3259908#2797442 #3144077#
#   iucn_row = which(iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id == ID)
#   iucn_country = iucn_wcvp_matched_countries_tdwg3$NewCountryName[iucn_row]
#   banked_row = which(as.numeric(brahms_wcvp_matched$wcvp_accepted_id) %in% ID)
#   banked_country = brahms_wcvp_matched$NewCountryName[banked_row]
#   match = iucn_country %in% banked_country
#   if(any(match)){
#     iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country[iucn_row[match]]= 1
#     iucn_wcvp_matched_countries_tdwg3$accessions_per_spp_country[iucn_row[match]]= length(unique(brahms_wcvp_matched$ACCESSION[banked_row]))
#     iucn_wcvp_matched_countries_tdwg3$seeds_per_spp_country[iucn_row[match]]= sum(brahms_wcvp_matched$ADJSTCOUNT[banked_row])
#     iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country[iucn_row[match]] = sum(brahms_wcvp_matched$plants_sampled[banked_row], na.rm = T)# iucn_wcvp_matched_countries_tdwg3[iucn_row,c("NewCountryName","banked_per_country")]
#   }
# }
# # # # save the data
# write.csv(iucn_wcvp_matched_countries_tdwg3, paste0(basepath, "revision_1/iucn_wcvp_matched_countries_tdwg3_maps.csv"))
iucn_wcvp_matched_countries_tdwg3 <- read.csv(paste0(basepath, "revision_1/iucn_wcvp_matched_countries_tdwg3_maps.csv"))


iucn_wcvp_matched_countries_tdwg3$Target_1 = ifelse((iucn_wcvp_matched_countries_tdwg3$seeds_per_spp_country >= 1050) &
                                                      (iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country >= 50), 1, 0)


#======================================================
# find species listed based on their range criteria
# iucn_wcvp_matched_countries_tdwg3$Target_2a = grepl("B", iucn_wcvp_matched_countries_tdwg3$redlistCriteria, ignore.case=FALSE)
# iucn_wcvp_matched_countries_tdwg3$Target_2 = ifelse((iucn_wcvp_matched_countries_tdwg3$Target_2a == T) &
#                                                       (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1), T, F)
# site_counts$Target_2a = ifelse(site_counts$proportion_native_country == 1,
#                                TRUE,FALSE)
# site_counts$Target_2a[is.na(site_counts$Target_2a)] = FALSE
#
# site_counts$Target_2 = ifelse((site_counts$Target_2a & site_counts$Target_1b),
#                               TRUE,FALSE)



# add the proportion of range collected
iucn_dict = unique(data.frame(cbind(indexes$taxon_name,
                                    indexes$proportion_native_country)))
colnames(iucn_dict) = c("taxon_name","proportion_native_country")

iucn_wcvp_matched_countries_tdwg3 = iucn_wcvp_matched_countries_tdwg3 %>%
  left_join(iucn_dict, by= c("taxon_name"))

iucn_wcvp_matched_countries_tdwg3$proportion_native_country[which(is.na(iucn_wcvp_matched_countries_tdwg3$proportion_native_country))] = 0

head(iucn_wcvp_matched_countries_tdwg3)

# estimate targets
iucn_wcvp_matched_countries_tdwg3$proportion_native_country = as.numeric(iucn_wcvp_matched_countries_tdwg3$proportion_native_country)
iucn_wcvp_matched_countries_tdwg3$Target_2a = ifelse((iucn_wcvp_matched_countries_tdwg3$proportion_native_country == 1) , 1, 0)


# iucn_wcvp_matched_countries_tdwg3$Target_2b = ifelse((iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country >= 50) , TRUE, FALSE)
iucn_wcvp_matched_countries_tdwg3$Target_2b = ifelse((iucn_wcvp_matched_countries_tdwg3$accessions_per_spp_country >= 5) , 1, 0)
# collections_per_native_country
iucn_wcvp_matched_countries_tdwg3$Target_2 = ifelse((iucn_wcvp_matched_countries_tdwg3$Target_2a & iucn_wcvp_matched_countries_tdwg3$Target_2b &
                                                       iucn_wcvp_matched_countries_tdwg3$plants_sampled_per_spp_country >= 50), 1, 0)


#======================================================
# orthodoxy and recalcitrance OLD
#======================================================
#
# iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
#                                                                       iucn_wcvp_matched_countries_tdwg3$banked_category,
#                                                                       iucn_wcvp_matched_countries_tdwg3$category)
# iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
#                                                                     iucn_wcvp_matched_countries_tdwg3$taxonomic_prediction_level,
#                                                                     iucn_wcvp_matched_countries_tdwg3$tax.level)
# iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
#                                                                        iucn_wcvp_matched_countries_tdwg3$banked_recalcitrance.y,
#                                                                        iucn_wcvp_matched_countries_tdwg3$probability.of.recalcitrance)
#
# # orthodox banked unbanked
# iucn_wcvp_matched_countries_tdwg3$orthodox_banked = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
#                                                               iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "orthodox"), 1, 0)
# iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
#                                                                 iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "orthodox"), 1, 0)
#
# # recalcitrant banked unbanked
# iucn_wcvp_matched_countries_tdwg3$recalcitrant_banked = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
#                                                                   iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "recalcitrant"), 1, 0)
# iucn_wcvp_matched_countries_tdwg3$recalcitrant_unbanked = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
#                                                                     iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined == "recalcitrant"), 1, 0)
#
# iucn_wcvp_matched_countries_tdwg3$orthodox_banked[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_banked)] = 0
# iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked)] = 0
# iucn_wcvp_matched_countries_tdwg3$recalcitrant_banked[is.na(iucn_wcvp_matched_countries_tdwg3$recalcitrant_banked)] = 0
# iucn_wcvp_matched_countries_tdwg3$recalcitrant_unbanked[is.na(iucn_wcvp_matched_countries_tdwg3$recalcitrant_unbanked)] = 0
#

#======================================================
#orthodoxy and recalcitrance CERTAIN/UNCERTAIN
#======================================================

# iucn_wcvp_matched_countries_tdwg3$storage_behaviour_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
#                                                                       iucn_wcvp_matched_countries_tdwg3$banked_category,
#                                                                       iucn_wcvp_matched_countries_tdwg3$category)
# iucn_wcvp_matched_countries_tdwg3$taxonomic_level_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
#                                                                     iucn_wcvp_matched_countries_tdwg3$taxonomic_prediction_level,
#                                                                     iucn_wcvp_matched_countries_tdwg3$tax.level)
# iucn_wcvp_matched_countries_tdwg3$recalcitrance_prob_combined = ifelse(iucn_wcvp_matched_countries_tdwg3$category == "banked",
#                                                                     iucn_wcvp_matched_countries_tdwg3$banked_recalcitrance.y,
#                                                                     iucn_wcvp_matched_countries_tdwg3$probability.of.recalcitrance)

# orthodox banked unbanked certain
iucn_wcvp_matched_countries_tdwg3$orthodox_banked_certain = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                      iucn_wcvp_matched_countries_tdwg3$category_certain == "orthodox"), 1, 0)
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_certain = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                        iucn_wcvp_matched_countries_tdwg3$category_certain == "orthodox"), 1, 0)

# orthodox banked unbanked uncertain
iucn_wcvp_matched_countries_tdwg3$orthodox_banked_uncertain = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                        iucn_wcvp_matched_countries_tdwg3$category_uncertain == "orthodox"), 1, 0)
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_uncertain = ifelse((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                          iucn_wcvp_matched_countries_tdwg3$category_uncertain == "orthodox"), 1, 0)

# recalcitrant/exceptional banked unbanked certain
iucn_wcvp_matched_countries_tdwg3$exceptional_banked_certain = ifelse(((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                          iucn_wcvp_matched_countries_tdwg3$category_certain == "recalcitrant") |
                                                                         (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                            iucn_wcvp_matched_countries_tdwg3$category_certain == "exceptional")), 1, 0)

iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_certain = ifelse(((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                            iucn_wcvp_matched_countries_tdwg3$category_certain == "recalcitrant") |
                                                                           (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                              iucn_wcvp_matched_countries_tdwg3$category_certain == "exceptional")), 1, 0)

# recalcitrant/exceptional banked unbanked uncertain
iucn_wcvp_matched_countries_tdwg3$exceptional_banked_uncertain = ifelse(((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                            iucn_wcvp_matched_countries_tdwg3$category_uncertain == "recalcitrant") |
                                                                           (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country == 1 &
                                                                              iucn_wcvp_matched_countries_tdwg3$category_uncertain == "exceptional")), 1, 0)

iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_uncertain = ifelse(((iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                              iucn_wcvp_matched_countries_tdwg3$category_uncertain == "recalcitrant") |
                                                                             (iucn_wcvp_matched_countries_tdwg3$banked_per_spp_country != 1 &
                                                                                iucn_wcvp_matched_countries_tdwg3$category_uncertain == "exceptional")), 1, 0)


iucn_wcvp_matched_countries_tdwg3$orthodox_banked_certain[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_banked_certain)] = 0
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_certain[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_certain)] = 0
iucn_wcvp_matched_countries_tdwg3$exceptional_banked_certain[is.na(iucn_wcvp_matched_countries_tdwg3$exceptional_banked_certain)] = 0
iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_certain[is.na(iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_certain)] = 0

iucn_wcvp_matched_countries_tdwg3$orthodox_banked_uncertain[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_banked_uncertain)] = 0
iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_uncertain[is.na(iucn_wcvp_matched_countries_tdwg3$orthodox_unbanked_uncertain)] = 0
iucn_wcvp_matched_countries_tdwg3$exceptional_banked_uncertain[is.na(iucn_wcvp_matched_countries_tdwg3$exceptional_banked_uncertain)] = 0
iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_uncertain[is.na(iucn_wcvp_matched_countries_tdwg3$exceptional_unbanked_uncertain)] = 0



###### ESTIMATE HOW MANY COUNTRIES HAVE CR species banked   #######################################################


# How many seeds and accessions per species per country
country_stats = iucn_wcvp_matched_countries_tdwg3[,c("NewCountryName",
                                                     "taxon_name",
                                                     "redlistCriteria",
                                                     "banked_per_spp_country",
                                                     "accessions_per_spp_country",
                                                     "seeds_per_spp_country",
                                                     # "storage_behaviour_combined",
                                                     # "orthodox_banked",
                                                     # "orthodox_unbanked",
                                                     # "recalcitrant_banked",
                                                     # "recalcitrant_unbanked",
                                                     "category_certain",
                                                     "orthodox_banked_certain",
                                                     "exceptional_banked_certain",
                                                     "orthodox_unbanked_certain",
                                                     "exceptional_unbanked_certain",
                                                     "category_uncertain",
                                                     "orthodox_banked_uncertain",
                                                     "exceptional_banked_uncertain",
                                                     "orthodox_unbanked_uncertain",
                                                     "exceptional_unbanked_uncertain",
                                                     "plants_sampled_per_spp_country",
                                                     "Target_1",
                                                     "Target_2")] %>%
  group_by(NewCountryName) %>%
  mutate(sum_CR_pred = length(unique(taxon_name)),
         sum_CR = length(unique(taxon_name[which(redlistCriteria != "predicted")])),
         sum_pred = length(unique(taxon_name[which(redlistCriteria == "predicted")])),
         sum_CR_banked = sum(banked_per_spp_country,na.rm=T),
         sum_Target1 = sum(Target_1,na.rm=T),
         sum_Target2 = sum(Target_2,na.rm=T),
         sum_orthodox_certain = length(which(category_certain == "orthodox")),
         sum_exceptional_certain = length(which(category_certain == "recalcitrant" | category_certain == "exceptional")),
         sum_orthodox_banked_certain = sum(orthodox_banked_certain,na.rm=T),
         sum_exceptional_banked_certain = sum(exceptional_banked_certain,na.rm=T),
         sum_orthodox_unbanked_certain = sum(orthodox_unbanked_certain,na.rm=T),
         sum_exceptional_unbanked_certain = sum(exceptional_unbanked_certain,na.rm=T),
         sum_orthodox_uncertain = length(which(category_uncertain == "orthodox")),
         sum_exceptional_uncertain = length(which(category_uncertain == "recalcitrant"| category_uncertain == "exceptional")),
         sum_orthodox_banked_uncertain = sum(orthodox_banked_uncertain,na.rm=T),
         sum_exceptional_banked_uncertain = sum(exceptional_banked_uncertain,na.rm=T),
         sum_orthodox_unbanked_uncertain = sum(orthodox_unbanked_uncertain,na.rm=T),
         sum_exceptional_unbanked_uncertain = sum(exceptional_unbanked_uncertain,na.rm=T),
         sum_accessions = sum(accessions_per_spp_country,na.rm=T),
         sum_seeds = sum(seeds_per_spp_country,na.rm=T)) %>%
  ungroup()
country_stats = country_stats[, c("NewCountryName",
                                  "sum_CR_pred",
                                  "sum_CR",
                                  "sum_pred",
                                  "sum_CR_banked",
                                  "sum_Target1",
                                  "sum_Target2",
                                  "sum_orthodox_certain",
                                  "sum_exceptional_certain",
                                  "sum_orthodox_banked_certain",
                                  "sum_exceptional_banked_certain",
                                  "sum_orthodox_unbanked_certain",
                                  "sum_exceptional_unbanked_certain",
                                  "sum_orthodox_uncertain",
                                  "sum_exceptional_uncertain",
                                  "sum_orthodox_banked_uncertain",
                                  "sum_exceptional_banked_uncertain",
                                  "sum_orthodox_unbanked_uncertain",
                                  "sum_exceptional_unbanked_uncertain",
                                  "sum_accessions",
                                  "sum_seeds")]
country_stats = unique(country_stats)

# save the data
write.csv(country_stats, paste0(basepath, "revision_1/country_stats.csv"))#,encoding = "UTF-8")




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

country_counts_map$sum_orthodox_banked_certain[is.na(country_counts_map$sum_orthodox_banked_certain)] = 0
country_counts_map$sum_orthodox_unbanked_certain[is.na(country_counts_map$sum_orthodox_unbanked_certain)] = 0
country_counts_map$sum_exceptional_banked_certain[is.na(country_counts_map$sum_exceptional_banked_certain)] = 0
country_counts_map$sum_exceptional_unbanked_certain[is.na(country_counts_map$sum_exceptional_unbanked_certain)] = 0

country_counts_map$sum_orthodox_banked_uncertain[is.na(country_counts_map$sum_orthodox_banked_uncertain)] = 0
country_counts_map$sum_orthodox_unbanked_uncertain[is.na(country_counts_map$sum_orthodox_unbanked_uncertain)] = 0
country_counts_map$sum_exceptional_banked_uncertain[is.na(country_counts_map$sum_exceptional_banked_uncertain)] = 0
country_counts_map$sum_exceptional_unbanked_uncertain[is.na(country_counts_map$sum_exceptional_unbanked_uncertain)] = 0

country_counts_map$sum_CR_pred[is.na(country_counts_map$sum_CR_pred)] = 0
# CR_country_counts = CR_country_counts %>% left_join(country_names)


# get some stats
country_counts_map$NAME_0[ which(country_counts_map$prop_banked > 0.5)]

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


data <- bi_class(country_counts_map.prj,
                 y=prop_Target_1,
                 x=prop_Target_2,
                 style = "jenks",#"equal",#"fisher",#"""quantile", #
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
          linewidth = 0.4, show.legend = FALSE) +
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
ggsave(paste0(plotpath, "targets.png"), width = 30, height = 12, units = "cm", bg="white")


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
          linewidth = 0.4, show.legend = FALSE) +
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
ggsave(paste0(plotpath, "ABS_CRbanked.png"), width = 30, height = 12, units = "cm", bg="white")


#############################
### orthodoxy certain
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
                 y=sum_orthodox_unbanked_certain,
                 x=sum_exceptional_unbanked_certain,
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
          linewidth = 0.4, show.legend = FALSE) +
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
                    xlab = paste0("# exceptional (0-",max(data$sum_exceptional_unbanked_certain),")"),
                    ylab = paste0("    # orthodox (0-",max(data$sum_orthodox_unbanked_certain),")"),
                    size = 10)

# combine map with legend
finalPlotc <- ggdraw() +
  theme(plot.background = element_rect(fill="white", color = NA))+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.73, .65, 0.35, 0.35)

finalPlotc

ggsave(paste0(plotpath, "unbanked_CR_orth_exceptional_certain.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "unbanked_CR_orth_exceptional_certain.png"), width = 30, height = 12, units = "cm", bg="white")

#############################
### orthodoxy uncertain
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
                 y=sum_orthodox_unbanked_uncertain,
                 x=sum_exceptional_unbanked_uncertain,
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
          linewidth = 0.4, show.legend = FALSE) +
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
                    xlab = paste0("# exceptional (0-",max(data$sum_exceptional_unbanked_uncertain),")"),
                    ylab = paste0("    # orthodox (0-",max(data$sum_orthodox_unbanked_uncertain),")"),
                    size = 10)

# combine map with legend
finalPlote <- ggdraw() +
  theme(plot.background = element_rect(fill="white", color = NA))+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.73, .65, 0.35, 0.35)

finalPlote

ggsave(paste0(plotpath, "unbanked_CR_orth_exceptional_uncertain.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "unbanked_CR_orth_exceptional_uncertain.png"), width = 30, height = 12, units = "cm", bg="white")

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
          linewidth = 0.4, show.legend = FALSE) +
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
ggsave(paste0(plotpath, "CR_banked_prop.png"), width = 30, height = 12, units = "cm", bg="white")


############################
# Tiled plots
############################
ggarrange(finalPlotb, finalPlota,finalPlotc,
          labels = c("A", "B", "C"),
          font.label = list(size = 30),
          ncol = 1, nrow = 3)

ggsave(paste0(plotpath, "maps_abc.pdf"),  width = 30, height = 36, units = "cm")
ggsave(paste0(plotpath, "maps_abc.png"),  width = 30, height = 36, units = "cm",bg="white")




ggarrange(finalPlotd,finalPlota, finalPlotb,finalPlotc,
          labels = c("A", "B", "C", "D"),
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
ggsave(paste0(plotpath, "map_CR.png"), width = 30, height = 12, units = "cm", bg="white")



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
ggsave(paste0(plotpath, "map_proportion_banked.pdf"), width = 30, height = 12, units = "cm", bg="white")




ggarrange(finalPlot1, finalPlot2,
          labels = c("A", "B"),
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
          labels = c("A", "B", "C","D"),
          font.label = list(size = 30),
          ncol = 2, nrow = 2)

ggsave(paste0(plotpath, "permits_seperate.pdf"),  width = 60, height = 30, units = "cm")
ggsave(paste0(plotpath, "permits_seperate.png"),  width = 60, height = 30, units = "cm",bg="white")




###################################################################################################################
#######  ADD STATS   ##############################################################################################
###################################################################################################################


# number of CR species
length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name[which(iucn_wcvp_matched_countries_tdwg3$redlistCriteria != "prediction")]))
# 5654
length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name[which(iucn_wcvp_matched_countries_tdwg3$redlistCriteria == "prediction")]))
# 96
length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name))
# 5750


# number of countries
length(unique(iucn_wcvp_matched_countries_tdwg3$NewCountryName[which(iucn_wcvp_matched_countries_tdwg3$redlistCriteria != "prediction")]))
# 166
length(unique(iucn_wcvp_matched_countries_tdwg3$NewCountryName[which(iucn_wcvp_matched_countries_tdwg3$redlistCriteria == "prediction")]))
# 38
length(unique(iucn_wcvp_matched_countries_tdwg3$NewCountryName))
# 166


# countries with more than 10 CR species
country_data2 = data.frame(country_counts_map.prj)
country_data2 = country_data2[,1:(ncol(country_data2)-1)]
country_data2$NewCountryName[country_data2$sum_CR_pred >= 10]
length(unique(country_data2$NewCountryName[country_data2$sum_CR_pred < 10]))

# countries with more than 100CR species
country_data2$NewCountryName[country_data2$sum_CR_pred >= 150]
# [1]  "Brazil"           "Cameroon"         "Colombia"         "Ecuador"          "Indonesia"
# [6]  "Madagascar"       "Mexico"           "New Caledonia"    "Philippines"      "Papua New Guinea"
# [11] "United States"
country_data2$NewCountryName[country_data2$sum_CR_pred >= 200]
# [1] "Brazil"           "Colombia"         "Ecuador"          "Indonesia"        "Madagascar"
# [6] "Mexico"           "Philippines"      "Papua New Guinea" "United States"

# countries with the highest % of CR species
country_stats$prop_CR = country_stats$sum_CR_pred/length(unique(iucn_wcvp_matched_countries_tdwg3$taxon_name))
test = country_stats %>%
  arrange(desc(prop_CR)) %>%  # arrange in descending order
  slice(1:12)
test[,c("NewCountryName","prop_CR","sum_CR_pred", "sum_CR")]
# NewCountryName      prop_CR sum_CR_pred sum_CR
# 1 Madagascar         0.0896         515    515
# 2 United States      0.0650         374    374
# 3 Philippines        0.0626         360    360
# 4 Indonesia          0.0595         342    342
# 5 Mexico             0.0468         269    269
# 6 Ecuador            0.0466         268    268
# 7 Colombia           0.0463         266    266
# 8 Brazil             0.0454         261    261
# 9 Papua New Guinea   0.0388         223    223
# 10 NA                0.0303         174    174
# 11 Cameroon          0.0303         174    174
# 12 New Caledonia     0.0261         150    150

# countries with over 50% of spp banked
country_data2$NewCountryName[country_data2$prop_banked >= 0.5]
# "Australia"      "Botswana"       "Spain"
# "United Kingdom" "Namibia"        "Saint Helena"

country_data2$sum_CR_banked[country_data2$NewCountryName %in% country_data2$NewCountryName[country_data2$prop_banked >= 0.5]]
#  74  2 66 22  4 22


#countries with highest proportion meeting target 1
country_data2$NewCountryName[country_data2$prop_Target_1 >= 0.9]
# "United Arab Emirates" "Austria"              "Botswana"
# "Cayman Islands"       "Oman"                 "Saudi Arabia"
# "Sierra Leone"         "Yemen"                "Zambia"
# "Zimbabwe"

length(unique(country_data2$NewCountryName[country_data2$prop_Target_1 <= 0.1]))
# 134

#countries with highest proportion meeting target 2
country_data2$NewCountryName[country_data2$prop_Target_2 >= 0.9]
#0
length(unique(country_data2$NewCountryName[country_data2$prop_Target_2 <= 0.1]))
#160

# countries that meet both targets
country_data2$NewCountryName[country_data2$prop_Target_2 >= 0.9 & country_data2$prop_Target_1 >= 0.9] #0

# nunmber pf countries with each permission level
sum(country_data2$NFP)  #173
sum(country_data2$CNA)  #79
sum(country_data2$IRCC) #27
sum(country_data2$NR)   #99

# countries with all four ABS
unique(country_data2$NewCountryName[country_data2$permits == 4])
# [1] "Argentina"          NA                   "Belgium"
# [4] "Benin"              "Bulgaria"           "Belarus"
# [7] "Bhutan"             "Côte d'Ivoire"      "Cameroon"
# [10] "Dominican Republic" "Spain"              "Ethiopia"
# [13] "France"             "Guatemala"          "Guyana"
# [16] "India"              "Kenya"              "Laos"
# [19] "Mexico"             "Panama"             "Peru"
# [22] "Uruguay"            "Vietnam"            "South Africa"


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
library(V.PhyloMaker2)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"
# plotpath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code"
plotpath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/plots/revision_1/"

# load iucn data
# iucn_banked_recalitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
iucn_wcvp_matched_countries_tdwg3



##########################################
# orthodox certain
##########################################

# # Read the phylogenetic tree from Zuntini
# tree <- read.tree("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/Trees/Trees/2_global_family_level.tre")
#
# # load tree data
# tr <- tree
# numtip <- length(tr$tip.label)

# Keep one representative species per family
family_reps <- tips.info.TPL[!duplicated(tips.info.TPL$family), ]

# Create the species list in the format required by phylo.maker()
sp.list <- data.frame(
  species = family_reps$species,
  genus = family_reps$genus,
  family = family_reps$family,
  stringsAsFactors = FALSE
)

# Build the tree (using scenario S3 for best interpolation)
result <- phylo.maker(
  sp.list = sp.list,
  tree = GBOTB.extended.TPL,
  nodes = nodes.info.1.TPL,
  scenarios = "S3",
  output.tree = TRUE
)

family_tree <- result$scenario.3

# renamte the tips by family not species
for (tipi in 1:length(family_tree$tip.label)){
  family_tree$tip.label[tipi] <- sp.list$family[which(sp.list$species %in% family_tree$tip.label[tipi])]
}

########################################################
# Plot tree data
########################################################
tr <- family_tree
numtip <- length(tr$tip.label)


# Get number of species per family that are CR and that are banked
fam_count = iucn_wcvp_matched_countries_tdwg3[,c("family", "category_certain")] %>%
  group_by(family) %>%
  summarize(
    n_orthodox = length(which(category_certain == "orthodox")),
    n_inter = length(which(category_certain == "intermediate")),
    n_exceptional = (length(which(category_certain == "exceptional")) +
                       length(which(category_certain == "recalcitrant"))),
    n_unknown = (length(which(is.na(category_certain))) +
                   length(which(category_certain == "unknown"))),
    p_orthodox = length(which(category_certain == "orthodox"))/n(),
    p_inter = length(which(category_certain == "intermediate"))/n(),
    p_exceptional = (length(which(category_certain == "exceptional")) +
                       length(which(category_certain == "recalcitrant")))/n(),
    p_unknown = (length(which(is.na(category_certain))) +
                   length(which(category_certain == "unknown")))/n(),
    tot = n()
  )

test = data.frame(tr$tip.label) %>% left_join(fam_count,
                                              by = c("tr.tip.label" = "family"))


# colorz  = ifelse(test$p_orthodox >= 0.5, "#648FFF",
#                 ifelse(test$p_exceptional >= 0.5, "#FFB000",
#                        ifelse(test$p_inter >= 0.5,"darkorange2", "#DC267F")))

df = test[,c("p_orthodox","p_exceptional", "p_inter","p_unknown")]
df_with_max_col <- df %>%
  mutate(Max_Column = apply(., 1, function(x) names(.)[which.max(x)]))
print(df_with_max_col)
colorz = ifelse(df_with_max_col$Max_Column == "p_orthodox", "#648FFF",
                ifelse(df_with_max_col$Max_Column == "p_exceptional", "#FFB000",
                       ifelse(df_with_max_col$Max_Column == "p_inter","darkorange2",
                              ifelse(df_with_max_col$Max_Column == "p_unknown","#DC267F","grey85"))))

test$tr.tip.label[which(df_with_max_col$Max_Column == "p_inter")]
# "Nartheciaceae"    "Aspleniaceae"     "Dennstaedtiaceae"
# "Pteridaceae"      "Lindsaeaceae"     "Ophioglossaceae"
# "Marattiaceae"     "Lycopodiaceae"    "Isoetaceae"


test$tr.tip.label[which(df_with_max_col$Max_Column == "p_exceptional")]
# [1] "Fagaceae"         "Elaeocarpaceae"   "Dipterocarpaceae"
# [4] "Thymelaeaceae"    "Meliaceae"        "Myrtaceae"
# [7] "Myristicaceae"    "Araucariaceae"    "Hymenophyllaceae"

length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_orthodox")])/
  length(which(df_with_max_col$p_orthodox > 0))
# 0.5867769 # 0.5666667



# colorz[is.na(colorz)] = "grey85"

test$no_CR = ifelse(colorz == "grey85",1,0)

dat = rbind(data.frame(id = test$tr.tip.label,
                       group = "Orthodox",
                       value = test$p_orthodox,
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
                       group = "Unknown",
                       value = test$p_unknown,
                       colour = colorz),
            data.frame(id = test$tr.tip.label,
                       group = "No CR in family",
                       value = test$no_CR,
                       colour = colorz))


# Plot the phylogenetic tree
p <- ggtree(tr, layout = "circular") +
  xlim(-100, 700) +
  geom_fruit(data = dat,
             geom = geom_bar,
             mapping = aes(y = id, x = value, fill = group),
             pwidth = 0.5,
             stat = "identity",
             orientation = "y",
             offset = 0.05) +
  scale_fill_manual(values = c("darkorange2","#FFB000", "grey85","#648FFF", "#DC267F"),
                    name = "")

# Extract tip labels from the tree data
tip_data <- p$data %>% filter(isTip) %>% left_join(dat, by = c("label" = "id"))

p <- p %<+% tip_data +
  aes(color = colour) +
  geom_tiplab(aes(label=label), offset=240, size=2) +
  scale_color_identity() +
  scale_colour_manual(values =  c("#648FFF", "#DC267F","darkorange2","#FFB000", "grey85"),
                      labels = c("Exceptional", "Intermediate", "no CR in family", "Orthodox", "Unknown")) +
  guides(colour = "none")

print(p)

ggsave(paste0(plotpath, "/phylo_orthodoxy_certain.pdf"),
       width = 20,
       height = 20,
       units = "cm")

ggsave(paste0(plotpath, "/phylo_orthodoxy_certain.png"),
       width = 25,
       height = 25,
       units = "cm",bg="white")



###########################################
# orthodox uncertain
###########################################

tr <- family_tree
numtip <- length(tr$tip.label)

# Get number of species per family that are CR and that are banked
fam_count = iucn_wcvp_matched_countries_tdwg3[,c("family", "category_uncertain")] %>%
  group_by(family) %>%
  summarize(
    n_orthodox = length(which(category_uncertain == "orthodox")),
    n_inter = length(which(category_uncertain == "intermediate")),
    n_exceptional = (length(which(category_uncertain == "exceptional")) +
                       length(which(category_uncertain == "recalcitrant"))),
    n_unknown = (length(which(is.na(category_uncertain))) +
                   length(which(category_uncertain == "unknown"))),
    p_orthodox = length(which(category_uncertain == "orthodox"))/n(),
    p_inter = length(which(category_uncertain == "intermediate"))/n(),
    p_exceptional = (length(which(category_uncertain == "exceptional")) +
                       length(which(category_uncertain == "recalcitrant")))/n(),
    p_unknown = (length(which(is.na(category_uncertain))) +
                   length(which(category_uncertain == "unknown")))/n(),
    tot = n()
  )

test = data.frame(tr$tip.label) %>% left_join(fam_count,
                                              by = c("tr.tip.label" = "family"))


# colorz  = ifelse(test$p_orthodox >= 0.5, "#648FFF",
#                 ifelse(test$p_exceptional >= 0.5, "#FFB000",
#                        ifelse(test$p_inter >= 0.5,"darkorange2", "#DC267F")))

df = test[,c("p_orthodox","p_exceptional", "p_inter","p_unknown")]
df_with_max_col <- df %>%
  mutate(Max_Column = apply(., 1, function(x) names(.)[which.max(x)]))
print(df_with_max_col)
colorz = ifelse(df_with_max_col$Max_Column == "p_orthodox", "#648FFF",
                ifelse(df_with_max_col$Max_Column == "p_exceptional", "#FFB000",
                       ifelse(df_with_max_col$Max_Column == "p_inter","darkorange2",
                              ifelse(df_with_max_col$Max_Column == "p_unknown","#DC267F","grey85"))))




# colorz[is.na(colorz)] = "grey85"

test$no_CR = ifelse(colorz == "grey85",1,0)

test$tr.tip.label[which(df_with_max_col$Max_Column == "p_inter")]
# [1] "Rutaceae"         "Nartheciaceae"    "Aspleniaceae"
# [4] "Dennstaedtiaceae" "Pteridaceae"      "Lindsaeaceae"
# [7] "Ophioglossaceae"  "Marattiaceae"     "Lycopodiaceae"
# [10] "Isoetaceae"


test$tr.tip.label[which(df_with_max_col$Max_Column == "p_exceptional")]
# [1] "Sapotaceae"       "Fagaceae"         "Rhizophoraceae"
# [4] "Chrysobalanaceae" "Peraceae"         "Elaeocarpaceae"
# [7] "Dipterocarpaceae" "Thymelaeaceae"    "Meliaceae"
# [10] "Myrtaceae"        "Arecaceae"        "Myristicaceae"
# [13] "Lauraceae"        "Araucariaceae"    "Hymenophyllaceae"

length(test$tr.tip.label[which(df_with_max_col$Max_Column == "p_orthodox")])/
  length(which(df_with_max_col$p_orthodox > 0))
# 0.8983957 # 0.8930481


dat = rbind(data.frame(id = test$tr.tip.label,
                       group = "Orthodox",
                       value = test$p_orthodox,
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
                       group = "Unknown",
                       value = test$p_unknown,
                       colour = colorz),
            data.frame(id = test$tr.tip.label,
                       group = "No CR in family",
                       value = test$no_CR,
                       colour = colorz))


# Plot the phylogenetic tree
p <- ggtree(tr, layout = "circular") +
  xlim(-100, 700) +
  geom_fruit(data = dat,
             geom = geom_bar,
             mapping = aes(y = id, x = value, fill = group),
             pwidth = 0.5,
             stat = "identity",
             orientation = "y",
             offset = 0.05) +
  scale_fill_manual(values = c("darkorange2","#FFB000", "grey85","#648FFF", "#DC267F"),
                    name = "")

# Extract tip labels from the tree data
tip_data <- p$data %>% filter(isTip) %>% left_join(dat, by = c("label" = "id"))

p <- p %<+% tip_data +
  aes(color = colour) +
  geom_tiplab(aes(label=label), offset=240, size=2) +
  scale_color_identity() +
  scale_colour_manual(values =  c("#648FFF", "#DC267F","darkorange2","#FFB000", "grey85"),
                      labels = c("Exceptional", "Intermediate", "no CR in family", "Orthodox", "Unknown")) +
  guides(colour = "none")

print(p)

ggsave(paste0(plotpath, "/phylo_orthodoxy_uncertain.pdf"),
       width = 20,
       height = 20,
       units = "cm")

ggsave(paste0(plotpath, "/phylo_orthodoxy_uncertain.png"),
       width = 25,
       height = 25,
       units = "cm",bg="white")


