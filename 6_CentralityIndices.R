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
cities4loop <- c("Liberec") # add more cities to "cover" all stores (no need for duplicate entries)
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
                 maxdist = 1000, idp = 2)
  Kr_Store$bc <- idwmodel$var1.pred
  idwmodel = idw(hc ~1, Kr_net,Kr_Store,
                 maxdist = 1000, idp = 2)
  Kr_Store$hc <- idwmodel$var1.pred
  idwmodel = idw(cc ~1, Kr_net,Kr_Store,
                 maxdist = 1000, idp = 2)
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
# Scaled closeness centrality (here, scaling is applied to one city)
Stores$cc <- scale(Stores$cc)
#
Stores
#
#####################################################################
# 
# Illustrative plot - shows Closeness centrality heatmap  
# 
# activate map elemets (streets)
edges <- net %>% 
  st_as_sf("edges") %>% 
  st_transform(4326)
# activate map elemets (junctions)
nodes <- net %>% 
  st_as_sf("nodes") %>% 
  st_transform(4326)
nodes$cc <- scale(nodes$cc)[,1]
# set map area
bboxLiberec <- st_bbox(c(xmin = 15.045, xmax = 15.075, ymax = 50.78, ymin = 50.757), crs = st_crs(4326))
bboxLiberec <- st_as_sfc(bboxLiberec)
# remove elements outside plotted area (simplified data handling)
LibCentNodes <- nodes %>% 
  dplyr::select(cc) %>% 
  sf::st_intersection(bboxLiberec)
#
LibCentEdges <- edges %>% 
  dplyr::select(cc) %>% 
  sf::st_intersection(bboxLiberec)
#
# Use ggmap package - you need to provide your own Google key
# .. generous amount of free "queries" is provided (monthly)
library(ggmap)
register_google(key = "Your own Google key goes here") # follow ?register_google
#
#
Lib_map <- get_map(c(left = 15.045, bottom = 50.757, right = 15.075, top = 50.78), 
                   maptype = "terrain", crop = T, language = "en-EN",
                   color="color")
#
ggmap(Lib_map)
#
# prepare heatmap (interpolate map elements with cc values over the whole map)
#
library(sp)
# get the min/max range for lat/long to make an empty grid 
x.range <- c(15.045,15.075) # min/max longitude of the interpolation area
y.range <- c(50.757,50.78)  # min/max latitude of the interpolation area  
# from the range, exapnd the coordinates to make a regular grid
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00025), 
                   y = seq(from = y.range[1], to = y.range[2], by = 0.00025))
coordinates(grd) <- ~x + y
sp::gridded(grd) <- TRUE
proj4string(grd) <- CRS("+init=epsg:4326") # convert to the epsg:4326
#
LB <- LibCentNodes %>% 
  as("Spatial") 
proj4string(LB) <- CRS("+init=epsg:4326") 
# perform the interpolation of cc values
idw <- gstat::idw(cc ~ 1, locations = LB, newdata = grd)  # apply idw model for the data
idw.output = as.data.frame(idw)
idw.output$Closeness <- idw.output$var1.pred
# Plot2
ggmap(Lib_map) +
  geom_sf(data = LibCentEdges, inherit.aes = F, colour="darkgrey",size = 0.5)+
  geom_tile(data = idw.output, aes(x = x, y = y, fill = Closeness), alpha = 0.45) + 
  scale_fill_distiller(palette = "RdYlGn", direction = 1) 
#
#
#
#
#
