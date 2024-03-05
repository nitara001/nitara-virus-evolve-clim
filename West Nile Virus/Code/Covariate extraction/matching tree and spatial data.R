#building a dataframe with tree data and environmental data together
wn_ed <- (caper::ed.calc(west_nile_tree))$spp

wn_ed_unique <- wn_ed %>%
  distinct(species, .keep_all = TRUE)

west_nile_data <- wn_ed_unique %>%
  left_join(west_nile_data, by = c("species" = "Accession_number"), suffix = c("", "_wn"))

# Create a key data frame with AccessionNumber and corresponding indices
key_data <- data.frame(
  AccessionNumber = west_nile_tree$tip.label,
  indices = seq_along(west_nile_tree$tip.label)
)

# Use the key_data to find matching indices
matching_indices <- stringdist::amatch(west_nile_coords$AccesionNumber, key_data$AccessionNumber, maxDist = 2)
west_nile_coords$Geo_location <- paste(west_nile_coords$Country, west_nile_coords$City, sep = ": ")

# Subset west_nile_coords using matching indices
west_nile_heterogeneity <- west_nile_coords$heterogeneity[matching_indices]
location <- west_nile_coords$Geo_location[matching_indices]
sum_extracted_values <- west_nile_coords$sum_extracted_values[matching_indices]
west_nile_extracted_temp <- west_nile_coords$extracted_temp[matching_indices]
west_nile_extracted_precip <- west_nile_coords$extracted_precip[matching_indices]
lat <- west_nile_coords$lat[matching_indices]
lon <- west_nile_coords$lon[matching_indices]

# Create west_nile_data without branch length
west_nile_data <- data.frame(
  Accession_number = west_nile_coords$AccesionNumber[matching_indices],
  location = location,
  sum_extracted_values = sum_extracted_values,
  extracted_temp = west_nile_extracted_temp,
  extracted_precip = west_nile_extracted_precip,
  X = lat,
  Y = lon,
  heterogeneity = west_nile_heterogeneity
)

# Adding number of occurrences based on location
west_nile_data$num_occurrences <- ave(seq(nrow(west_nile_data)), west_nile_data$location, FUN = length)
