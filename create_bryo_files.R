basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/SEEDS/GAP_analysis/20_03_24_data/"

# iucn_new_pred = read.csv(paste0(basepath, "iucn_predictions_wcvp_matched.csv"))
# iucn_new_pred = iucn_new_pred[which(iucn_new_pred$category == "CR"),]
# #they are all angiosperms, so not needed

bryo_old = read.csv(paste0(basepath, "Bryophytes CR.csv"))
fern_lyco_old = read.csv(paste0(basepath, "Ferns and Lycophytes CR_spores characteristics.csv"))


iucn_new = read.csv(paste0(basepath, "iucn_wcvp_matched.csv"))
unique(iucn_new$higher)

#subset the data for ferns and bryophytes
iucn_bryo = iucn_new[which(iucn_new$higher %in% c("Bryophyta","Marchantiophyta")),]
iucn_fern_lyco = iucn_new[which(iucn_new$higher %in% c("Ferns","Lycophytes")),]


# subset the missing values
write.csv(iucn_fern_lyco[!(iucn_fern_lyco$scientificName %in% fern_lyco_old$scientificName_IUCN),],
          paste0(basepath, "Ferns and Lycophytes missing.csv"))
write.csv(iucn_bryo[!(iucn_bryo$scientificName %in% bryo_old$scientificName_IUCN),],
          paste0(basepath, "Bryophytes missing.csv"))



