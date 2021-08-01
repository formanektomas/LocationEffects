#####################################################################
# Packages
# 
library(dplyr)
library(sf)
library(gstat) 
library(RCzechia)
library(sfnetworks)
library(tidygraph)
#
#
rm(list=ls())
load("SampleData.RData")
rm(isochrones) # not necessary here
# 
load("Streets.RData") # OSM streets for Liberec - local streets
# .. from previous script.
#
#####################################################################
# 
#
Stores$bc <- 0 # introduce betweenness centrality variable
Stores$hc <- 0 # introduce hub centrality variable
Stores$cc <- 0 # introduce closeness centrality variable
#
cities <- obce_polygony() # city polygons from RCzechia package
roads <- silnice() # main roads from RCzechia
cities4loop <- c("Liberec") # add a cities to "cover" all stores (no need to repeat entries)
#
#
for (i in 1){ # instead of "1", use 1:X where X is the number of cities used.
  # select roads,streets and stores for i-th city
  # .. in general - this exapmle has one city only
  X_road <- cities %>% 
    filter(NAZ_OBEC == cities4loop[i]) %>% 
    st_intersection(roads) %>% 
    select(KOD_OBEC) 
  #
  X_street <- cities %>% 
    filter(NAZ_OBEC == cities4loop[i]) %>% 
    st_intersection(Streets) %>% 
    select(KOD_OBEC) 
  #
  X_store <- cities %>% 
    filter(NAZ_OBEC == cities4loop[i]) %>% 
    st_intersection(Stores) %>% 
    select(id)
  #
  X_village <- rbind(X_road,X_street)
  xx <- st_cast(X_village,"LINESTRING") # combine both street types into 1 set
  #
  # centrality indices for graph nodes are calculated here
  net <- xx %>% 
    st_transform(5514) %>% # flat "Krovak-type" projection & distances in meters
    as_sfnetwork() %>%
    activate("nodes") %>%
    #
    mutate(bc = centrality_betweenness(directed=F,cutoff=3000)) %>% # betweenness
    mutate(hc = centrality_hub()) %>%                               # hub
    mutate(cc = centrality_closeness(mode = "all",cutoff=3000)) %>% # closeness
    filter(is.na(bc)==F,is.na(hc)==F,is.na(cc)==F) %>% 
    st_as_sf()
  #
  Kr_net <- st_transform(net, 5514) 
  Kr_Store <- st_transform(X_store, 5514) 
  # interpolate store centrality from street centrality by IDW
  # for each centrality indicat at a time
  idwmodel = idw(bc ~1, Kr_net,Kr_Store,
                 maxdist = 2000, idp = 1)
  Kr_Store$bc <- idwmodel$var1.pred
  idwmodel = idw(hc ~1, Kr_net,Kr_Store,
                 maxdist = 2000, idp = 1)
  Kr_Store$hc <- idwmodel$var1.pred
  idwmodel = idw(cc ~1, Kr_net,Kr_Store,
                 maxdist = 2000, idp = 1)
  Kr_Store$cc <- idwmodel$var1.pred
  #
  row_Indx <- which(Stores$id %in% Kr_Store$id)
  #
  Stores$bc[row_Indx] <- Kr_Store$bc
  Stores$hc[row_Indx] <- Kr_Store$hc
  Stores$cc[row_Indx] <- Kr_Store$cc
  #
  print("----------")
  print(i)
  print(cities[i])
}
# View centrality values
Stores
# Scaled closeness centrality (here, used for one city)
Stores$cc <- scale(Stores$cc)
#
Stores
