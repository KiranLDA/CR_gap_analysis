library(dplyr)
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

###### LOAD DATA ###############################################################

# load brahms data
brahms <- read.csv(paste0(basepath,"2024-03-21_164953044-BRAHMSOnlineData.csv"))

# load IUCN
iucn <- read.csv(paste0(basepath, "redlist/assessments.csv" ))
length(iucn$scientificName) # 5702

# Load POWO/wcvp
wcvp <- read.csv(paste0(basepath, "wcvp__2_/wcvp_names.csv" ), sep="|")

# remove duplicates
brahms <- brahms[duplicated(brahms$AccessionNumber)==FALSE,] # removes 441 duplicates
#Get rid of end of species names so that they are easier to match
brahms$species <- word(brahms$Taxon, 1,2)

# create summary table where adjusted seed counts per species are added together
spp_count = brahms %>%
  group_by(species,AdjustedSeedQuantity) %>%
  tally() %>%
  mutate(summed_count = sum(AdjustedSeedQuantity),
         count = n()) %>%
  ungroup()

# bit hacky, but get rid of adjusted count and duplicates
spp_count = spp_count[, c("species", "count", "summed_count")]
spp_count = spp_count[duplicated(spp_count$species)==FALSE,]
spp_count$CR = ifelse(spp_count$species %in% iucn$scientificName,1,0)

# get ipni IDs

# quick crossref
length(which(unique(brahms$species) %in% iucn$scientificName)) #293 species

##############################################################################
###### rWCVP #################################################################
##############################################################################
if (!require(rWCVPdata)) {
  install.packages("rWCVPdata",
                   repos = c(
                     "https://matildabrown.github.io/drat",
                     "https://cloud.r-project.org"
                   )
  )
}

