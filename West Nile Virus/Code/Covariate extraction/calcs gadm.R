library(sf)
library(rgdal)
library(raster)
#average size of each administrative level
gadm <- st_read("/Users/nitarawijayatilake/Documents/UCL /Masters Project/gadm_410.gpkg")
cities <- gadm[gadm$ENGTYPE_1 == "City", ]
city_areas <- st_area(cities)
mean(city_areas)
states<- gadm[gadm$ENGTYPE_1 == "State", ]
state_areas<- st_area(states)
mean(state_areas)
