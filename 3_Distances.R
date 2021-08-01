#####################################################################
# Packages
# 
library(dplyr)
library(RCzechia)
library(sf)
library(units)
#
rm(list = ls())
#
# load sample data generated at step 1
load("SampleData.RData")
#
#####################################################################
# Load PoIs or map elements to measure distances to stores
# .. here, we use distances from stores to main highways
#
MainRoads <- RCzechia::silnice() # downloads main roads in Czech Republic
# .. analogous information may be retrieved using osmdata package
#
MainRoads <- MainRoads %>% 
  filter(TRIDA %in% c("Dálnice I. tr.","Dálnice II. tr.")) %>%  # keyword for selected main road classs
  mutate(Road = "main") %>% # new variable, included for summarizing
  group_by(Road) %>% 
  dplyr::summarise() %>% # unites geometries, operation can take some time
  ungroup()
#
#
distances <- sf::st_distance(Stores, MainRoads) %>%
  units::set_units("meters") # aerial distances are simple, other options exist
# 
#
#
Stores$distToHW <- distances
Stores # here, highways are not close (20 km approx.)
