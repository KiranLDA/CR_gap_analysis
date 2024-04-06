#R version 3.2.2

library(dismo) #version 1.0-1.2
library(foreach) #version 1.4.3
library(doParallel) #version 1.0.10
library(maptools) #version 0.8-37
library(dplyr)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

#----------------------------------------------------------------------------------------------------------------
#get location data from GBIF for as many seed plant species as available (maximum 100 records per species)
#----------------------------------------------------------------------------------------------------------------
#read in a dataframe containing all seed plant species listed as accepted in The Plant List
#Dataframe contains three character columns: "Species" (latin binomial), "Genus", and "Family"
all.spp = read.csv(paste0(basepath,"iucn_wcvp.csv"))

wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),
                   sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")


all.spp = all.spp %>% left_join(wcvp[,c("plant_name_id","genus","family")],
                                by = c("wcvp_accepted_id" = "plant_name_id"))

all.spp = all.spp[,c("taxon_name", "genus", "family")]
colnames(all.spp) = c("Species", "Genus", "Family")


#this may need to be done in batches of 10s of thousands of rows at a time,
# as if GBIF is over-loaded it may fail
cl = makeCluster(16)
registerDoParallel(cl)


   GBIF = suppressWarnings(foreach(n = 1:nrow(all.spp), .combine = rbind) %dopar% {
  library(dismo)
  genus = as.character(all.spp[n,"Genus"])
  species =strsplit(as.character(all.spp[n,1]), " ")[[1]][2:length(strsplit(as.character(all.spp[n,1]), " ")[[1]])]
  species = paste(species, collapse = " ")

  data = try(gbif(genus, species, geo = TRUE, removeZeros = TRUE, args = "/occurrence/search", ntries = 10, end = 100), silent = TRUE)

  if(is.data.frame(data)){
    if(!is.null(data$lat) & !is.null(data$lon)){
      if(!is.null(data$species)){
        data = na.omit(subset(data, basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN" | basisOfRecord == "LIVING_SPECIMEN" | basisOfRecord == "OBSERVATION", select = c("species", "basisOfRecord", "lat", "lon")))
      } else{
        species = rep(paste(genus, species), nrow(data))
        data = cbind(data, species)
        data = na.omit(subset(data, basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN" | basisOfRecord == "LIVING_SPECIMEN" | basisOfRecord == "OBSERVATION", select = c("species", "basisOfRecord", "lat", "lon")))
        data = unique(data)
      }
      data[,1] = rep(as.character(all.spp[n,1]), nrow(data))

      result = data.frame(all.spp[rep(n, nrow(data)),], lat = data$lat, long = data$lon, row.names = NULL, stringsAsFactors = FALSE)
      result
    }
  }
})

stopCluster(cl)

GBIF = GBIF[order(GBIF$Family, GBIF$Genus, GBIF$Species),]


#----------------------------------------------------------------------------------------------------------------
#Acquire the biome type for each location (Olson et al. 2001) and write out for use by Python script
#----------------------------------------------------------------------------------------------------------------
#read in shapefile polygon layer of the terrestrial biomes from Olson el al. 2001
ecoregions = readShapePoly()

points = GBIF[, c("long", "lat")]
points = SpatialPoints(points)

#baileys = over(points, ecoregions)
biomes = over(points, ecoregions)

GBIF = data.frame(GBIF, ecoregion = biomes$Biome_name)

#write out text file of the GBIF dataframe (i.e. a ecoregion for each recorded location for each species)
write.table(GBIF, "spp_OlsonBiomes.txt", sep = "\t", row.names = F, col.names = T, quote = F)

##Python script ("part 2") then organises this file into more usable files: a single biome type for each
#species where it was most commonly recorded(to then be used by R script "part 3")
