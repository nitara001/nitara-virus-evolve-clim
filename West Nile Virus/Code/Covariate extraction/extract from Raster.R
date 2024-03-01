library(raster)
library(sf)
library(sp)
library(rgdal)
land_data <- raster("HILDAplus_vGLOB-1.0_luc_change-freq_1960-2019_wgs84.tif")
#average size of each administrative level (from gadm script)
states_area<- st_area(states)
avg_state_area<- mean(states_area)

#land use changes
west_nile_coords<- read.csv("west_nile_coords.csv") #from geocoded script
west_nile_coords$lon <- as.numeric(west_nile_coords$lon)
west_nile_coords$lat <- as.numeric(west_nile_coords$lat)
west_nile_sf <- st_as_sf(west_nile_coords, coords = c("lon", "lat"), crs = 4326)
# Extract buffer sizes
west_nile_buffer_sizes <- ifelse(grepl(",", west_nile_coords$City),mean(city_areas), mean(states_area)) # Apply buffer sizes to west_nile_sf
west_nile_buffered_sf <- st_buffer(west_nile_sf, dist= sqrt(west_nile_buffer_sizes/pi))
# Extract values from land_data within the buffered polygons
west_nile_extracted_values <- extract(land_data, west_nile_buffered_sf)
# Convert the extracted_values column to a list of numeric vectors
west_nile_extracted_values <- lapply(west_nile_extracted_values, function(x) as.numeric(trimws(x)))
# Calculate the sum of extracted values within each polygon
sum_extracted_values <- sapply(west_nile_extracted_values, sum)
# Add the sum of extracted values as a new column
west_nile_coords$sum_extracted_values <- sum_extracted_values

#####---------------------------------------------------------------------------------------
temp_raster<-raster("temperature_meanchange_baselinetopresent.tif")

##temp
west_nile_extracted_temp <- raster::extract(temp_raster, west_nile_buffered_sf)
west_nile_extracted_temp <- lapply(west_nile_extracted_temp , function(x) as.numeric(trimws(x)))
west_nile_coords$extracted_temp <- sapply(west_nile_extracted_temp, function(x) mean(as.numeric(x), na.rm = TRUE))
#####---------------------------------------------------------------------------------------
##precipitation
precip_raster<- raster("precip_meanchange_baselinetopresent.tif")

west_nile_extracted_precip <- raster::extract(precip_raster, west_nile_buffered_sf)
west_nile_extracted_precip <- lapply(west_nile_extracted_precip , function(x) as.numeric(trimws(x)))
west_nile_coords$extracted_precip <- sapply(west_nile_extracted_precip, function(x) mean(as.numeric(x), na.rm = TRUE))

#####---------------------------------------------------------------------------------------
#heterogeneity
heterogeneity_tiff<- raster("Dissimilarity_01_05_25km_uint32.tif")

wn_hetero<- raster::extract(heterogeneity_tiff, west_nile_buffered_sf)
west_nile_extracted_hetero <- lapply(wn_hetero , function(x) as.numeric(trimws(x)))
west_nile_coords$heterogeneity  <- sapply(west_nile_extracted_hetero, function(x) mean(as.numeric(x), na.rm = TRUE))
