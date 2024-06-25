# combined IUCN data with banked and unbanked and CR categories
iucn_banked_recalitrance <- read.csv(paste0(basepath, "spp_banked_recalcitrant.csv"))

# Banked IUCN
iucn_banked <- iucn_banked_recalitrance[which(iucn_banked_recalitrance$category == "banked"),]
iucn_banked$taxon_name


sp_code <- read.csv(paste0(basepath, "name_match_spcode.csv"))
sp_code$Taxon = paste(sp_code[,3],sp_code[,4],sp_code[,5],sp_code[,6],
                      sp_code[,7],sp_code[,8],sp_code[,9],sp_code[,10],sp_code[,11])
sp_code$Taxon2 = paste(sp_code[,3],sp_code[,4])


for (rowi in 1:nrow(sp_code)){
  tryCatch({
    sp_code$Taxon[rowi] = trimws(sp_code$Taxon[rowi])
  }, error = function(e){})
}

# sp_code$Taxon[rowi] = trimws(sp_code$Taxon[rowi])
# strips <- "^\\s+|\\s+$"
# sp_code$Taxon = gsub(strips,"", sp_code$Taxon)

new_table = iucn_banked %>% left_join(brahms_wcvp_matched[,c("taxon_name","Taxon")],
                                      by = "taxon_name")

new_table = new_table %>% left_join(sp_code[,c("SPNUMBER","Taxon")],
                                    by = "Taxon")

new_table = new_table %>% left_join(sp_code[,c("SPNUMBER","Taxon2")],
                                    by = c("Taxon"="Taxon2"))

new_table$Taxon22 = sub("(\\w+\\s+\\w+).*", "\\1", new_table$Taxon)

new_table = new_table %>% left_join(sp_code[,c("SPNUMBER","Taxon2")],
                                    by = c("Taxon22"="Taxon2"))

# remaining = new_table[is.na(new_table$SPNUMBER.x) & is.na(new_table$SPNUMBER.y),]
# remaining$Taxon22 = sub("(\\w+\\s+\\w+).*", "\\1", remaining$Taxon)
unique(new_table$SPNUMBER)
new_table

codes = c(unique(new_table$SPNUMBER.x),unique(new_table$SPNUMBER.y))


new_table$code = ifelse(!is.na(new_table$SPNUMBER.x), new_table$SPNUMBER.x,
                        ifelse(!is.na(new_table$SPNUMBER.y),new_table$SPNUMBER.y, new_table$SPNUMBER))

write.csv(unique(new_table$code),paste0(basepath, "codes4Naomi.csv"))
