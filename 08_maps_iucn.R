# library("rnaturalearth")
# library("rnaturalearthdata")
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

#read in iucn data and wcvp data
iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched_full_name.csv"))

wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|",
                             header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
tdwg3_countries <- read.csv(paste0(basepath, "country_tdwg3_mapping.csv"))
tdwg3_countries$ISO_code[is.na(tdwg3_countries$ISO_code)] ="NA"

# make sure variables are in same format
iucn_wcvp_matched$wcvp_accepted_id <- as.numeric(iucn_wcvp_matched$wcvp_accepted_id)
wcvp_countries$plant_name_id <- as.numeric(wcvp_countries$plant_name_id)

spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
indexes = read.csv(paste0(basepath,"iucn_brahms_indexes_targets.csv"))

spp_banked_recalcitrant %>% left_join(wcvp_accepted_id)


# put wcvp tdwg3 region data into iucn data
iucn_wcvp_matched_countries = iucn_wcvp_matched %>% left_join(wcvp_countries[,c("plant_name_id",
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
iucn_wcvp_matched_countries_tdwg3$country_duplicate = duplicated(iucn_wcvp_matched_countries_tdwg3[,c("internalTaxonId",
                                                                                                      "wcvp_accepted_id",
                                                                                                      "NewCountryName")])

#########################################################################################################
##### FORMAT BAKED DATA    ##############################################################################
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

#-------------------------------------------------------------------------------------------------#
##### SUBSET THE BANKED SPECIES ###################################################################
#-------------------------------------------------------------------------------------------------#
#get rid of duplicated tdwgs (mulitple tdwgs in one country are remove)
iucn_wcvp_matched_countries_tdwg3 = iucn_wcvp_matched_countries_tdwg3[which(iucn_wcvp_matched_countries_tdwg3$country_duplicate == F),]


# find the MSB species that are CR endangered
iucn_wcvp_matched_countries_tdwg3$banked = (iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id %in%
                                              brahms_wcvp_matched$wcvp_accepted_id)
(data.frame(banked = iucn_wcvp_matched_countries_tdwg3$banked,
                id = iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id ))[451:501,]
# CR species
# iucn_wcvp_matched_countries_tdwg3$banked_per_country
iucn_wcvp_matched_countries_tdwg3$banked_per_country = 0
for(ID in unique(iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id)){
  # ID = 3259908#2797442 #3144077#
  iucn_row = which(iucn_wcvp_matched_countries_tdwg3$wcvp_accepted_id == ID)
  iucn_country = iucn_wcvp_matched_countries_tdwg3$NewCountryName[iucn_row]
  banked_row = which(as.numeric(brahms_wcvp_matched$wcvp_accepted_id) %in% ID)
  banked_country = brahms_wcvp_matched$NewCountryName[banked_row]
  match = iucn_country %in% banked_country
  if(any(match)){
    iucn_wcvp_matched_countries_tdwg3$banked_per_country[iucn_row[match]]= 1
    # iucn_wcvp_matched_countries_tdwg3[iucn_row,c("NewCountryName","banked_per_country")]
  }
}

###### ESTIMATE HOW MANY COUNTRIES HAVE CR species banked   #######################################################


# Get the counts per country per spp
country_CR_spp_banked = iucn_wcvp_matched_countries_tdwg3[,c("NewCountryName", "taxon_name", "banked_per_country")] %>%
  group_by(NewCountryName) %>%
  mutate(sum_CR = length(unique(taxon_name)),
         sum_CR_banked = sum(banked_per_country)) %>%
  ungroup()
country_CR_spp_banked = country_CR_spp_banked[, c("NewCountryName","sum_CR", "sum_CR_banked")]
country_CR_spp_banked = unique(country_CR_spp_banked)


# save the data
write.csv(country_CR_spp_banked, paste0(basepath, "country_CR_spp_banked.csv"))



####### ADD SPATIAL DATA TO THE NAMES #############################################################################
country_names = data.frame(unique(iucn_wcvp_matched_countries_tdwg3[,"NewCountryName"]))
colnames(country_names) = "NewCountryName"
country_names = country_names %>% left_join(data.frame(world)[,c("GID_0","NAME_0")],
                                            by=c("NewCountryName" = "NAME_0"))

country_counts_map = country_CR_spp_banked %>% left_join(country_names)
country_counts_map = world %>% left_join(country_counts_map)
country_counts_map$sum_CR[is.na(country_counts_map$sum_CR)] = 0
country_counts_map$sum_CR_banked[is.na(country_counts_map$sum_CR_banked)] =0

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

country_counts_map.prj$log_CR = log(country_counts_map.prj$sum_CR+1)
country_counts_map.prj$log_CR_banked = log(country_counts_map.prj$sum_CR_banked+1)
country_counts_map.prj$proportion = country_counts_map.prj$sum_CR_banked/country_counts_map.prj$sum_CR
country_counts_map.prj$proportion[is.na(country_counts_map.prj$proportion)] = 0
country_counts_map.prj$log_proportion = log(country_counts_map.prj$proportion+1)

# values
dim=3

# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#73AE80", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "grey80",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#2A5A5B", #"black",
#                    bottomright="#6C83B5"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#c15e5c", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#554249", #"black",
#                    bottomright="#6eabbd"# brown3"#rgb(130,0,80, maxColorValue=255)
# )

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#ffb300", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="darkolivegreen", # "#554249",
                   bottomright="#349e9e"# brown3"#rgb(130,0,80, maxColorValue=255)
)


# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#c73c0a", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#c9c9c9",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#411002", #"black",
#                    bottomright="#432474"# brown3"#rgb(130,0,80, maxColorValue=255)
# )
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft= "#509dc2", #rgb(0,150,235, maxColorValue=255),
#                    upperright= "#c9c9c9",  #"grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#000000", #"black",
#                    bottomright="#f3b300"# brown3"#rgb(130,0,80, maxColorValue=255)
# )


custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))



data <- bi_class(country_counts_map.prj,
                 y=log_CR,
                 x=log_CR_banked,
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
                    xlab = "# banked",
                    ylab = "     # species",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  theme(plot.background = element_rect(fill="white", color = NA))+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.82, .7, 0.24, 0.24)


finalPlot
ggsave(paste0(plotpath, "bivariate_spp_vs_banked_GRYlBu.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "bivariate_spp_vs_banked_GRYlBu.png"), width = 30, height = 12, units = "cm")



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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# values
dim=3


col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#ffb300", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="darkolivegreen", # "#554249",
                   bottomright="#349e9e"# brown3"#rgb(130,0,80, maxColorValue=255)
)



custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))




data <- bi_class(country_counts_map.prj,
                 y=log_CR,
                 x=log_proportion,
                 style = "fisher",#"jenks",#"quantile", #"equal",#
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
                    xlab = "% banked",
                    ylab = "     # species",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  theme(plot.background = element_rect(fill="white", color = NA))+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.82, .7, 0.24, 0.24)


finalPlot

ggsave(paste0(plotpath, "bivariate_spp_vs_prop_banked_GRYlBu.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(plotpath, "bivariate_spp_vs_prop_banked_GRYlBu.png"), width = 30, height = 12, units = "cm")
















######################################################################################
#####  Estimate CR species number  ###################################################
######################################################################################

country_names = data.frame(unique(brahms_CR[,"NewCountryName"]))
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

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#c15e5c", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#554249", #"black",
                   bottomright="#6eabbd"# brown3"#rgb(130,0,80, maxColorValue=255)
)


custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(country_counts_map.prj,
                 y=sum_seeds,#sum_accessions,
                 x=sum_spp,
                 style = "quantile",#"jenks",#"fisher",#"equal",#  , "equal", "fisher""jenks",#
                 dim = dim)

# create map
map <- ggplot() +
  geom_point( data = data,
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
ggsave(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/code/", "seeds_vs_species_CR.pdf"), width = 30, height = 12, units = "cm")



#
# spp_banked_recalcitrant = read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))
# spp_banked_recalcitrant$category[which(is.na(spp_banked_recalcitrant$category))] = "unknown"
#
# # Add the country information to spp_banked_recalcitrant
#
#
#
# # return a species list per tdwg
# country_counts_map.prj$total_species = NA
#
# i=2
# country = country_counts_map.prj$NAME_0[i] #country_counts_map.prj$NewCountryName[i]
# tdwg_code <- get_wgsrpd3_codes(country)
# checklist <- wcvp_checklist(area_codes = tdwg_code, taxon_rank = "species",
#                             synonyms = FALSE)
# # only keep the country data
# checklist = checklist[checklist$area_code_l3 == tdwg_code,]
#
# # see how many of those are CR endangered
# country_counts_map.prj$total_species[i] = nrow(checklist)
# country_counts_map.prj$total_banked_somewhere[i] = length(which(checklist$taxon_name %in% brahms_wcvp_matched$taxon_name))
# country_counts_map.prj$total_banked_in_country[i] =
#   country_counts_map.prj$CR_species[i] = length(which(checklist$taxon_name %in% spp_banked_recalcitrant$taxon_name))
# country_counts_map.prj$CR_banked_somewhere[i] = length(which(checklist$taxon_name %in% brahms_CR$taxon_name))
# country_counts_map.prj$CR_banked_from_country[i] =
#

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#6eabbd", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#554249", #"black",
                   bottomright="#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
)

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#f3b300", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#f3f3f3",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#000000", #"black",
                   bottomright="#509dc2"# brown3"#rgb(130,0,80, maxColorValue=255)
)


col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#4fadd0", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#f3f3f3",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#2a1a8a", #"black",
                   bottomright="#de4fa6"# brown3"#rgb(130,0,80, maxColorValue=255)
)

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#efd100", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#fffdef",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#007fc4", #"black",
                   bottomright="#d2e4f6"# brown3"#rgb(130,0,80, maxColorValue=255)
)

col.matrix<-colmat(nquantiles=dim,
                   upperleft= "gold", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#2a1a8a", #"black",
                   bottomright="#007fc4"# brown3"#rgb(130,0,80, maxColorValue=255)
)


col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#A35F9A", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#D3D3D3",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#474E84", #"black",
                   bottomright="#6DB5B4"# brown3"#rgb(130,0,80, maxColorValue=255)
)


col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#C8B35A", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#E8E8E8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#804D36", #"black",
                   bottomright="#9972AF"# brown3"#rgb(130,0,80, maxColorValue=255)
)

#############################################################################################
# IUCN
#############################################################################################


