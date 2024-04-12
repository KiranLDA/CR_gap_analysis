# library("rnaturalearth")
# library("rnaturalearthdata")
library(geodata)
library(dplyr)
library(stringdist)
library(sf)
library(biscale)
library(ggplot2)
library(cowplot)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"



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
library(classInt)
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

#########################################################################################################


############# GET THE WORLD MAP  DATA #######################################################################

world <- sf::st_as_sf(world(path="."))


#########################################################################################################
##### WHAT IS IN THE BANK? ##############################################################################
#########################################################################################################
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched.csv"))

####### FORMAT COUNTRY NAMES #############################################################################
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

# the political merges that I feel a little unsure about as there are no lat longs to associate to a country after splitting
# or other political regions e.g. San Marino not being recognised by geodata as it's own country
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Russian Federation"] = "Russia"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "Yugoslavia"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$NewCountryName[brahms_wcvp_matched$NewCountryName == "San-Marino"] = "Italy"




# test = data.frame(world)[,c("GID_0","NAME_0")]


country_names = data.frame(unique(brahms_CR[,"NewCountryName"]))
colnames(country_names) = "NewCountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")], by=c("NewCountryName" = "NAME_0"))


# Fuzzy match the country names that weren't already matched
unmatched = country_names$NewCountryName[is.na(country_names$GID_0)]
leftover = c()
for (country in unmatched){
  idw = which(fuzzy_match(country,world$NAME_0))
  if (length(idw) == 1){
    idc = which(country_names$NewCountryName == country)
    country_names[idc,2] = data.frame(world)[idw,"GID_0"]
  } else {
    leftover = c(leftover, country)
    print(country)
  }
}

# note that these names were not matched
# [1] "Unknown"
# [1] "FYROM"


##### SUBSET THE CR SPECIES ###################################################################

# find the MSB species that are CR endangered
brahms_wcvp_matched$CR = brahms_wcvp_matched$wcvp_accepted_id %in% iucn_wcvp_matched$wcvp_accepted_id
summary(brahms_wcvp_matched$CR)

brahms_CR = brahms_wcvp_matched[brahms_wcvp_matched$CR,]
dim(brahms_CR)

###### ESTIMATE HOW MANY COUNTRIES HAVE ENOUGH SEEDS   ############################################################

### FOR CR SPECIES ################################################################################################
# Get the counts per country per spp
CR_country_spp_seeds = brahms_CR %>%
  group_by(NewCountryName, taxon_name, AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         sum_counts = sum(AdjustedSeedQuantity),
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
write.csv(CR_country_spp_seeds, paste0(basepath, "CR_seeds_per_spp_country_with_usage.csv"))

# Now count the numbers of species per country with the different uses
CR_country_uses = CR_country_spp_seeds %>%
  group_by(NewCountryName, collection_utility) %>%
  tally() %>%
  # mutate(sum_uses = length(unique(collection_utility)),
  #        uses = n()) %>%
  ungroup()
colnames(CR_country_uses) = c("NewCountryName", "CR_collection_utility","species_number")

# save the data
write.csv(CR_country_uses, paste0(basepath, "CR_usage_per_country_with_spp_number.csv"))



### FOR ALL SPECIES IN THE BANK ########################################################################

# Get the counts per country per spp
country_spp_seeds = brahms_wcvp_matched %>%
  group_by(NewCountryName, taxon_name, AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         sum_counts = sum(AdjustedSeedQuantity),
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
write.csv(country_spp_seeds, paste0(basepath, "seeds_per_spp_country_with_usage.csv"))

# Now count the numbers of species per country with the different uses
country_uses = country_spp_seeds %>%
  group_by(NewCountryName, collection_utility) %>%
  tally() %>%
  # mutate(sum_uses = length(unique(collection_utility)),
  #        uses = n()) %>%
  ungroup()
colnames(country_uses) = c("NewCountryName", "collection_utility","species_number")

# save the data
write.csv(country_uses, paste0(basepath, "usage_per_country_with_spp_number.csv"))


####### ESTIMATE PER COUNTRY THE NUMBERS OF SPECIES, ACCESSIONS AND SEEDS   ############

###### FOR CR SPECIES ##################################################################

# count number of seeds collected per country
country_seeds = brahms_CR %>%
  group_by(NewCountryName, AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(sum_seeds = sum(AdjustedSeedQuantity),
         accessions = n()) %>%
  ungroup()
country_seeds = country_seeds[, c("NewCountryName","sum_seeds")]
country_seeds = country_seeds[duplicated(country_seeds$NewCountryName) == FALSE, ]

# count species per country
country_spp = brahms_CR %>%
  group_by(NewCountryName, taxon_name) %>%
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         accessions = n()) %>%
  ungroup()
country_spp = country_spp[, c("NewCountryName","sum_spp")]
country_spp = country_spp[duplicated(country_spp$NewCountryName) == FALSE, ]

# count accessions per country
country_acc = brahms_CR %>%
  group_by(NewCountryName, AccessionNumber) %>%
  tally() %>%
  mutate(sum_accessions = length(unique(AccessionNumber)),
         accessions = n()) %>%
  ungroup()
country_acc = country_acc[, c("NewCountryName","sum_accessions")]
country_acc = country_acc[duplicated(country_acc$NewCountryName) == FALSE, ]

###### Combine everything
country_counts = country_seeds %>% left_join(country_spp[,c("NewCountryName","sum_spp")],
                            by = "NewCountryName") %>% left_join(country_acc[,c("NewCountryName","sum_accessions")],
                                                              by = "NewCountryName")


# save the data
write.csv(country_counts, paste0(basepath, "seeds_accessions_species_per_country.csv"))


###### FOR ALL SPECIES IN THE BANK   ##############################################################

# count number of seeds collected per country
country_seeds = brahms_CR %>%
  group_by(NewCountryName, AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(sum_seeds = sum(AdjustedSeedQuantity),
         accessions = n()) %>%
  ungroup()
country_seeds = country_seeds[, c("NewCountryName","sum_seeds")]
country_seeds = country_seeds[duplicated(country_seeds$NewCountryName) == FALSE, ]

# count species per country
country_spp = brahms_CR %>%
  group_by(NewCountryName, taxon_name) %>%
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         accessions = n()) %>%
  ungroup()
country_spp = country_spp[, c("NewCountryName","sum_spp")]
country_spp = country_spp[duplicated(country_spp$NewCountryName) == FALSE, ]

# count accessions per country
country_acc = brahms_CR %>%
  group_by(NewCountryName, AccessionNumber) %>%
  tally() %>%
  mutate(sum_accessions = length(unique(AccessionNumber)),
         accessions = n()) %>%
  ungroup()
country_acc = country_acc[, c("NewCountryName","sum_accessions")]
country_acc = country_acc[duplicated(country_acc$NewCountryName) == FALSE, ]

###### Combine everything
country_counts = country_seeds %>% left_join(country_spp[,c("NewCountryName","sum_spp")],
                                             by = "NewCountryName") %>% left_join(country_acc[,c("NewCountryName","sum_accessions")],
                                                                                  by = "NewCountryName")


# save the data
write.csv(country_counts, paste0(basepath, "seeds_accessions_species_per_country.csv"))




### !!!!!! start probably delete!!!!!! ###########################################################
# ##### add spatial information
# country_counts = country_counts %>% left_join(world, by=c("CountryName" = "admin"))
#
# unmatched = country_counts$CountryName[is.na(country_counts$scalerank)]
#
#
# # Function to perform fuzzy matching
# # loop over the leftover names and add them
# leftover = c()
# for (country in unmatched){
#   idw = which(fuzzy_match(country,world$admin))
#   if (length(idw != 0)){
#     idc = which(country_counts$CountryName == country)
#     country_counts[idc,5:ncol(country_counts)] = world[idw,which(colnames(world) != "admin")]
#   } else {
#     leftover = c(leftover, country)
#     print(country)
#   }
# }
#
#
# # now try a different column
# unmatched = c()
# for (country in leftover){
#   idw = which(fuzzy_match(country, world$formal_en))
#   if (length(idw != 0)){
#     idc = which(country_counts$CountryName == country)
#     country_counts[idc,5:ncol(country_counts)] = world[idw,which(colnames(world) != "admin")]
#   } else {
#     unmatched = c(unmatched, country)
#     print(country)
#   }
# }
#
# world$name_sort
# world$geounit
# # name_sort/name_long/name/geounit
# # loop over the leftover names and add them
# leftover = c()
# for (country in unmatched){
#   idw = which(fuzzy_match(country, world$admin))
#   if (length(idw != 0)){
#     idc = which(country_counts$CountryName == country)
#     country_counts[idc,5:ncol(country_counts)] = world[idw,which(colnames(world) != "admin")]
#   } else {
#     leftover = c(leftover, country)
#     print(country)
#   }
# }
#
#
#
# fuzzy_match("Azerbaijan",world$admin)
#   country_counts$CountryName[is.na(country_counts$scalerank)], world$admin)
#
#
# length(which(bank$summed_count >= 250)) # 180
# length(which(bank$summed_count >= 1500)) # 98
# length(which(bank$summed_count >= 250 & bank$summed_count <= 1500)) #82

### !!!!!! end probably delete!!!!!! ###########################################################

####### ADD SPATIAL DATA TO THE NAMES #############################################################################

country_counts_map = country_counts %>% left_join(country_names)

country_counts_map = world %>% left_join(country_counts_map)



####  PROJECT in ECKERT IV

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #

sf_use_s2(FALSE)
m = st_buffer(country_counts_map, 0)
country_counts_map.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                  xmax = 180,
                                                  ymin = -90,
                                                  ymax = 90))),
                             crs = PROJ)


# write.csv(grid.DT, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
grid.DT = read.csv(paste0(spatial_path, "gridDT.csv"))
grid.DT <- data.table::as.data.table(grid.DT)

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
dim=4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#6eabbd", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#554249", #"black",
#                    bottomright="#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
#
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#f3b300", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#f3f3f3",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#000000", #"black",
#                    bottomright="#509dc2"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
#
#
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#4fadd0", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#f3f3f3",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#2a1a8a", #"black",
#                    bottomright="#de4fa6"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
#
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#efd100", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#fffdef",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#007fc4", #"black",
#                    bottomright="#d2e4f6"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
#
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "gold", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#2a1a8a", #"black",
#                    bottomright="#007fc4"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
#
#
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#A35F9A", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#D3D3D3",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#474E84", #"black",
#                    bottomright="#6DB5B4"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
#
#
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#C8B35A", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#E8E8E8",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#804D36", #"black",
#                    bottomright="#9972AF"# brown3"#rgb(130,0,80, maxColorValue=255)
# )

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
                 style ="quantile",#"fisher",#"quantile",#"equal",# "jenks",# , "equal", "fisher""jenks",#
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data= data,
              aes(color =  bi_class, #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates" ) +
  geom_sf(data = data, mapping = aes(fill = bi_class),
          color = aes(fill = bi_class ),#NA,"black",#
          size = 0.8, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  bi_scale_fill(pal = custom_pal4, dim=dim,
                na.value="#e8e8e8")+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim,
                 na.value="#e8e8e8")+#,flip_axes = TRUE, rotate_pal = TRUE) +
  guides(color = "none") +
  bi_theme() +
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
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
ggsave(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "seeds_vs_species.pdf"), width = 30, height = 12, units = "cm")


#####  Estimate CR species number  ###################################################


spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
spp_banked_recalcitrant$category[which(is.na(spp_banked_recalcitrant$category))] = "unknown"

# Add the country information to spp_banked_recalcitrant



# return a species list per tdwg
country_counts_map.prj$total_species = NA

i=2
country = country_counts_map.prj$NAME_0[i] #country_counts_map.prj$NewCountryName[i]
tdwg_code <- get_wgsrpd3_codes(country)
checklist <- wcvp_checklist(area_codes = tdwg_code, taxon_rank = "species",
                            synonyms = FALSE)
# only keep the country data
checklist = checklist[checklist$area_code_l3 == tdwg_code,]

# see how many of those are CR endangered
country_counts_map.prj$total_species[i] = nrow(checklist)
country_counts_map.prj$total_banked_somewhere[i] = length(which(checklist$taxon_name %in% brahms_wcvp_matched$taxon_name))
country_counts_map.prj$total_banked_in_country[i] =
country_counts_map.prj$CR_species[i] = length(which(checklist$taxon_name %in% spp_banked_recalcitrant$taxon_name))
country_counts_map.prj$CR_banked_somewhere[i] = length(which(checklist$taxon_name %in% brahms_CR$taxon_name))
country_counts_map.prj$CR_banked_from_country[i] =


#####  PIE CHART    ######################################################################################

spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
spp_banked_recalcitrant$category[which(is.na(spp_banked_recalcitrant$category))] = "unknown"



bank = spp_banked_recalcitrant[spp_banked_recalcitrant$banked == T,]


# Some are replicated but they are species that have bee split
length(unique(spp_banked_recalcitrant$taxon_name)) # 5707
length(spp_banked_recalcitrant$taxon_name) # 5717

spp_banked_recalcitrant$predictions = ifelse(spp_banked_recalcitrant$redlistCriteria == "prediction", "prediction", "IUCN")
spp_banked_recalcitrant$labels = paste( spp_banked_recalcitrant$category, "-", spp_banked_recalcitrant$predictions)
pie_data = spp_banked_recalcitrant %>% count(labels)
pie_data = pie_data[c(1,2,6,7,4,5,8,9,3,10),]
par(mar = c(0,0,1,4))
pie(pie_data$n, pie_data$labels, cex=.75,
    col=c("darkolivegreen3","darkolivegreen4",
          "darkgoldenrod1","darkgoldenrod3",
          "chocolate1","chocolate3",
          "brown3","brown4",
          "black", "grey"))

