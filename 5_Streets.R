#####################################################################
# Packages
# 
library(mapview)
library(ggplot2)
library(RCzechia)
library(tidyverse)
library(tidygeocoder)
library(osmdata)
library(hereR)
rm(list = ls())
#
#####################################################################
# 
# 
#
cities <- c("Liberec","Beroun")
# .. add names of any cities you need
# .. note that different cities (in two or more countries) can have the same name
# .. hence, some filtering and verification is advisable
#
#
# uses OSM to retrieve street data
for (i in 1:2){ # 2 cities in example
    X <- opq(bbox = iconv(cities[i],to = "UTF-8")) %>%
      add_osm_feature(key = "highway", value = "residential") %>% # different types can be retrieved
      osmdata_sf(quiet = F)  # 
    X <- X$osm_lines[,"osm_id"]
    if (i==1) {
      Streets <<- X
    } else {
      Streets <- rbind(Streets,X)
    }
    print(i)
    print(X$geometry)
    print(nrow(X))
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<3){}
}
#
plot(Streets[1:2880,]) # Liberec
#
#
#######################################################################
#
#
#
#