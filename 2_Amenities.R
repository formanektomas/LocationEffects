#####################################################################
# Packages
# 
library(ggplot2)
library(mapview)
library(RCzechia)
library(tidyverse)
library(tidygeocoder)
library(osmdata)
#
rm(list = ls())
#
# load sample data generated at step 1
load("SampleData.RData")
#
#####################################################################
# Post offices retrieved from OSM
#
#
Posts <- osmdata::opq(bbox = iconv("Liberec",to = "UTF-8")) %>% # choose city/region
  add_osm_feature(key = "amenity", 
                  value = "post_office") %>%
  osmdata_sf(quiet = F)  # 
#
str(Posts) # note the different geometry information types
#

ISO <- mapview::mapview(isochrones)
mapview::mapview(Posts$osm_points, label = "name", map = ISO@map )
#
#####################################################################
# Intersect Posts and 10-minute isochrones 
# .. i.e. calculate the number of Posts 
# within 10 minute walking distances from each shop
#
# .. "pipe" syntax is used
#
nPosts <- isochrones %>% 
  st_intersection(Posts$osm_points) %>% 
  st_drop_geometry() %>%  
  group_by(id) %>% 
  tally() %>%  
  arrange(desc(n)) %>% 
  as.data.frame()
#
nPosts
#
#
#
#
#####################################################################
#
# For Pharmacies, use 
# add_osm_feature(key = "amenity", value = "pharmacy")
# on line 21,22 above
#
#
# For Restaurants, use
# add_osm_feature(key = "amenity", 
#                 value = c("bar","restaurant","pub"))
#
#
# For Public transport, use
# add_osm_feature(key = "public_transport", 
#                 value = "stop_position")
#
#
# To select other features, refer to OSM wiki at
# https://en.wikipedia.org/wiki/OpenStreetMap
#
#
#
#
#
#
#
#####################################################################
#
# If your region of interest is large and in the expected number
# of Point of interest is large (high thousands of rows),
# OSM may not handle your request properly. You may consider
# splitting your area into a grid (say, 10 x 10) and running
# the query in a loop. While this can be time consuming, it typically
# is only done once for a given analysis.
#
# Bounding box - bbox for OSM retrieval across whole Czech Republic:
# bbox = c(12.13083,48.26188,19.06064,51.05579)
#         c(xmin, ymin, xmax, ymax)
#
# Prepare the grid to search for Posts (a 10 by 10 grid is used)
a <- (19.06-12.11)/10
x_axis <- 12.11+a*seq(from=0, to=10, by=1)
b <- (51.06-48.26)/10
y_axis <- 48.26+b*seq(from=0, to=10, by=1)
rm(a,b)
#
# .. search for post offices across the whole bbox 
# .. (includes some observations from AT,DE,PL,SK )
#
for (i in 1:10){
  for(j in 1:10){
    X <- opq(bbox = c(x_axis[i],y_axis[j],x_axis[i+1],y_axis[j+1])) %>%
      add_osm_feature(key = "amenity", 
                      value = "post_office") %>%
      osmdata_sf(quiet = F)  # 
    X <- X$osm_points[,c("osm_id","amenity")]
    if (i==1 & j == 1) { # .. this is a clumsy approach, but works here
      posts <<- X 
    } else {
      posts <- rbind(posts,X)
    }
  print(c(i,j))
  print(X$geometry)
  print(nrow(X))
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<2){} #dummy while loop
  }
}
mapview::mapview(posts)
#
# save as csv
coords <- sf::st_coordinates(posts$geometry)
colnames(coords) <- c("long","lat")
posts2<- cbind(posts,coords)
posts2 <- st_drop_geometry(posts2)
write.csv(posts2,"PostsGPS.csv",row.names = F)
#
#
#