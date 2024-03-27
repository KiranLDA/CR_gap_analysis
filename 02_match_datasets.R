
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"


###### Find the CR species in the dataset ##################################################################

#load data from previous session
iucn_wcvp_matched = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))
brahms_wcvp_matched = read.csv(paste0(basepath, "brahms_wcvp_matched.csv"))
brahms_unique_wcvp_matched = read.csv(paste0(basepath, "brahms_unique_wcvp_matched.csv"))
wcvp <- read.table(paste0(basepath, "wcvp__2_/wcvp_names.csv" ),sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
wcvp_countries <- read.table(paste0(basepath, "wcvp__2_/wcvp_distribution.csv" ), sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")


# find the MSB species that are CR endangered
brahms_wcvp_matched$CR = brahms_wcvp_matched$wcvp_accepted_id %in% iucn_wcvp_matched$wcvp_accepted_id
summary(brahms_wcvp_matched$CR)
#   Mode   FALSE    TRUE
# logical  195713    2222

brahms_CR = brahms_wcvp_matched[brahms_wcvp_matched$CR,]
dim(brahms_CR)


# Find the IUCN species that are banked
iucn_wcvp_matched$banked = iucn_wcvp_matched$wcvp_accepted_id %in% brahms_wcvp_matched$wcvp_accepted_id
summary(iucn_wcvp_matched$banked)
#    Mode   FALSE    TRUE
# logical    5249     372

# Attach the genus data
brahms_CR = brahms_CR  %>% left_join(wcvp[, c("plant_name_id", "family", "genus")], by=c("wcvp_accepted_id" = "plant_name_id"))
iucn_wcvp_matched = iucn_wcvp_matched %>% left_join(wcvp[, c("plant_name_id", "family", "genus")], by=c("wcvp_accepted_id" = "plant_name_id"))


# determine whether they are easy to match
