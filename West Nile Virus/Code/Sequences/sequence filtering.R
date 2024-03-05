##Sequence filtering


####--------------------------
#only include location names that have a colon as that indicates past just country level
wn<- readDNAStringSet("/Users/nitarawijayatilake/Documents/UCL /Masters Project/Files/West Nile/west nile virus fasta.fasta")
west_nile_filtered <-wn [grep(": ", names(wn))]
writeXStringSet(wn , file = "/Users/nitarawijayatilake/Documents/UCL /Masters Project/Files/West Nile/west_nile_geo_filtered")
library(stringr)

####--------------------------

#convert the information in fasta headers eg. geo_location into metadata dataframe
west_nile_fasta<-readLines("west_nile_geo_filtered")
west_nile_headers<- west_nile_fasta[str_detect(west_nile_fasta, "^>")] 
west_nile_fasta <- str_replace(west_nile_headers, "^>", "") 
west_nile_metadata <- str_split(west_nile_fasta , "\\|") 
west_nile_metadata_df <- data.frame(matrix(unlist(west_nile_metadata), nrow = length(west_nile_metadata), byrow = TRUE), stringsAsFactors = FALSE)
write.csv(west_nile_metadata_df, "/Users/nitarawijayatilake/Documents/UCL /Masters Project/Files/West Nile/west_nile_metadata.csv", row.names = FALSE)
