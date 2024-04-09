# library("rnaturalearth")
# library("rnaturalearthdata")
library(geodata)
library(dplyr)
library(stringdist)

#######   FUNCTION   ##################################################################################

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

#########################################################################################################

world <- sf::st_as_sf(world(path="."))



spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
spp_banked_recalcitrant$category[which(is.na(spp_banked_recalcitrant$category))] = "unknown"

# Some are replicated but they are species that have bee split
length(unique(spp_banked_recalcitrant$taxon_name)) # 5707
length(spp_banked_recalcitrant$taxon_name) # 5717


#########################################################################################################
##### WHAT IS IN THE BANK? ##############################################################################
#########################################################################################################
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched.csv"))


####### FORMAT COUNTRY DATA #############################################################################
# URL <- "https://github.com/nvkelso/natural-earth-vector/raw/master/geojson/ne_50m_admin_0_countries.geojson.gz"
# fil <- basename(URL)
# R.utils::gunzip("https://github.com/nvkelso/natural-earth-vector/raw/master/geojson/ne_50m_admin_0_countries.geojson.gz")
# world <- readOGR(dsn="ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")

# world <- ne_countries(scale = "small", returnclass = "sf")
# world <- ne_countries(scale = "large",  returnclass = "sf")
# states <- ne_states(returnclass = "sf")
# test <- rnaturalearth::ne_coastline(returnclass = "sf")
# test <- world(path=".") #data(countryExData)

# Install and load the worldmap package


#
# # Create a basic world map including dependencies
# data("worldCountries")
#
# # Plot the map
# mapCountryData(mapRegion=worldCountries, nameColumnToPlot="REGION")

brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Auckland Island"] = "New Zealand"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Bosnia-Herzegovina"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Yugoslavia"] = "Bosnia and Herzegovina"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "UK"] = "United Kingdom"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "USA"] = "United States"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Congo, DRC"] = "Democratic Republic of the Congo"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "St. Helena, Ascension & Tristan da Cunha"] = "Saint Helena"
# brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Congo, DRC"] = "Democratic Republic of the Congo"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Russian Federation"] = "Russia"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Ivory Coast"] = "Côte d'Ivoire"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Heard Isl"] = "Heard Island and McDonald Islands"
brahms_wcvp_matched$CountryName[brahms_wcvp_matched$CountryName == "Sao Tome & Principe"] = "São Tomé and Príncipe"


bank = spp_banked_recalcitrant[spp_banked_recalcitrant$banked == T,]


##### add spatial information
country_names = data.frame(unique(brahms_wcvp_matched[,"CountryName"]))
colnames(country_names) = "CountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")], by=c("CountryName" = "NAME_0"))


# Function to perform fuzzy matching
# loop over the leftover names and add them
unmatched = country_names$CountryName[is.na(country_names$GID_0)]
leftover = c()
for (country in unmatched){
  idw = which(fuzzy_match(country,world$NAME_0))
  if (length(idw) == 1){
    idc = which(country_counts$CountryName == country)
    country_names[idc,2] = data.frame(world)[idw,"GID_0"]
  } else {
    leftover = c(leftover, country)
    print(country)
  }
}

View(data.frame(leftover))

country_names



# count number of seeds collected per country
country_seeds = brahms_wcvp_matched %>%
  group_by(CountryName, AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(sum_seeds = sum(AdjustedSeedQuantity),
         accessions = n()) %>%
  ungroup()
country_seeds = country_seeds[, c("CountryName","sum_seeds")]
country_seeds = country_seeds[duplicated(country_seeds$CountryName) == FALSE, ]

# count species per country
country_spp = brahms_wcvp_matched %>%
  group_by(CountryName, taxon_name) %>%
  tally() %>%
  mutate(sum_spp = length(unique(taxon_name)),
         accessions = n()) %>%
  ungroup()
country_spp = country_spp[, c("CountryName","sum_spp")]
country_spp = country_spp[duplicated(country_spp$CountryName) == FALSE, ]

# count accessions per country
country_acc = brahms_wcvp_matched %>%
  group_by(CountryName, AccessionNumber) %>%
  tally() %>%
  mutate(sum_accessions = length(unique(AccessionNumber)),
         accessions = n()) %>%
  ungroup()
country_acc = country_acc[, c("CountryName","sum_accessions")]
country_acc = country_acc[duplicated(country_acc$CountryName) == FALSE, ]

###### Combine everything
country_counts = country_seeds %>% left_join(country_spp[,c("CountryName","sum_spp")],
                            by = "CountryName") %>% left_join(country_acc[,c("CountryName","sum_accessions")],
                                                              by = "CountryName")



##### add spatial information
country_counts = country_counts %>% left_join(world, by=c("CountryName" = "admin"))

unmatched = country_counts$CountryName[is.na(country_counts$scalerank)]


# Function to perform fuzzy matching
# loop over the leftover names and add them
leftover = c()
for (country in unmatched){
  idw = which(fuzzy_match(country,world$admin))
  if (length(idw != 0)){
    idc = which(country_counts$CountryName == country)
    country_counts[idc,5:ncol(country_counts)] = world[idw,which(colnames(world) != "admin")]
  } else {
    leftover = c(leftover, country)
    print(country)
  }
}


# now try a different column
unmatched = c()
for (country in leftover){
  idw = which(fuzzy_match(country, world$formal_en))
  if (length(idw != 0)){
    idc = which(country_counts$CountryName == country)
    country_counts[idc,5:ncol(country_counts)] = world[idw,which(colnames(world) != "admin")]
  } else {
    unmatched = c(unmatched, country)
    print(country)
  }
}

world$name_sort
world$geounit
# name_sort/name_long/name/geounit
# loop over the leftover names and add them
leftover = c()
for (country in unmatched){
  idw = which(fuzzy_match(country, world$admin))
  if (length(idw != 0)){
    idc = which(country_counts$CountryName == country)
    country_counts[idc,5:ncol(country_counts)] = world[idw,which(colnames(world) != "admin")]
  } else {
    leftover = c(leftover, country)
    print(country)
  }
}



fuzzy_match("Azerbaijan",world$admin)
  country_counts$CountryName[is.na(country_counts$scalerank)], world$admin)


length(which(bank$summed_count >= 250)) # 180
length(which(bank$summed_count >= 1500)) # 98
length(which(bank$summed_count >= 250 & bank$summed_count <= 1500)) #82


#####  Prep the spatial data ############################################################################

library(tidyverse)
library(rworldmap)
library(sf)

# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/"

# functions
normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}


#Load the tdwg shp which is the base for everything
tdwg3 <- st_read(dsn = paste0(basepath, "level3"),
                 layer = "level3")

# map country codes to tdwg
wgsrpd_mapping = read.csv(paste0(basepath,"country_tdwg3_map_KD.csv"))
wgsrpd_mapping$ISO_code[is.na(wgsrpd_mapping$ISO_code)] <-"NA"


tdwg3 <- tdwg3 %>%
  left_join(wgsrpd_mapping[c("LEVEL3_COD","COUNTRY","ISO_code")])





####  PROJECT A in ECKERT IV

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"#"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #

sf_use_s2(FALSE)
m = st_buffer(darkspots, 0)
darkspots.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                  xmax = 180,
                                                  ymin = -90,
                                                  ymax = 90))),
                             crs = PROJ)


# write.csv(grid.DT, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
grid.DT = read.csv( "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
grid.DT <- data.table::as.data.table(grid.DT)

# values needed once (don't recalculate each time)
areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "twdg3_land_area.csv"))





dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#6eabbd", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#554249", #"black",
                   bottomright="#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))


data <- bi_class(darkspots.prj,y=linnean, x=wallacean,
                 style ="fisher",#"quantile",#"equal",# "jenks",# , "equal", "fisher"
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data= data,
              aes(color =  bi_class, #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates" ) +
  geom_sf(data = data, mapping = aes(fill = bi_class),
          color = "black",#aes(fill = bi_class ),#NA,
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
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )

map




# number of seeds
# per_tdwg
# per country

# number of species



#####  PIE CHART    ######################################################################################

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

