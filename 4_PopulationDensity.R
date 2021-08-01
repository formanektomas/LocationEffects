#####################################################################
# Packages
#
library(RCzechia)
library(ggplot2)
library(dplyr)
library(czso)
library(sf)
#
rm(list=ls())
#
#
#####################################################################
# Spatial data for Czech Republic
# .. analogous data may be retrieved from OSM
districts <- RCzechia::casti() # city districs (for cities with districs)
cities <- obce_polygony() # cities & villages - polygons (districts ignored)

# population numbers from last country-wide survey 
# .. table structure in: https:/www.czso.cz/documents/10180/25233177/sldb2011_vou.xls
Inhabitants <- czso::czso_get_table("SLDB-VYBER") %>% 
  select(uzkod, obyvatel = vse1111) %>% # select total number of population
  mutate(obyvatel = as.numeric(obyvatel)) # convert from text to numeric

# join spatial and population data
districts <- inner_join(districts, Inhabitants, by = c("KOD" = "uzkod")) 
cities <- inner_join(cities, Inhabitants, by = c("KOD_OBEC" = "uzkod")) 
# 
# calculate inhabitants per km2
districts$density <- districts$obyvatel / (st_area(districts)/1000000) # m2 to km2
units(districts$density) <- NULL
#
cities$density <- cities$obyvatel / (st_area(cities)/1000000)
units(cities$density) <- NULL
#
rm(Inhabitants)
#
#
#####################################################################
# 
# load new store location data (illustrative)
Stores <- data.frame(id=c(1,2,3),
                     long=c(15.05733,16.49632,14.43549),
                     lat=c(50.76612,50.03646,50.08356))
Stores <- st_as_sf(Stores, coords = c('long', 'lat'), crs=4326)
#
#
StoreInCity <- Stores %>% 
  mutate(intersection = as.integer(st_intersects(geometry, cities)),
         CityName = if_else(is.na(intersection), '', cities$NAZ_OBEC[intersection]),
         CityDensity = if_else(is.na(intersection), -1, cities$density[intersection]))
         # "-1" doesn't happen in example; also -1 is easier to implement than "NA" 
         # this density information is available for cities/villages
#
StoreInCity
#
#
#
StoreInDistrict <- Stores %>% 
  mutate(intersection = as.integer(st_intersects(geometry, districts)),
         DistricName = if_else(is.na(intersection), '', districts$NAZEV[intersection]),
         DistrictDensity = if_else(is.na(intersection), -1, districts$density[intersection]))
         # "-1" for stores out of town / in villages
         # district density only available in cities, not in villages (administrative distinction)
# 
StoreInDistrict
#
#
#
#####################################################################
# If district information (more precise) is available, we use district information
# for population density.
# .. district information is not available for most smaller cities and villages.
#
Stores$City_density <- ifelse(StoreInDistrict$DistrictDensity == -1, StoreInCity$CityDensity,StoreInDistrict$DistrictDensity )
#
Stores