library(data.table)
library(tidyverse)
library(leaflet)
library(sf)
library(osmdata)
library(mapview)
library(RColorBrewer)
library(traveltime)
library(sp)
library(purrr)
library(osrm)
library(rmapzen)
library(rgdal)
library(rgeos)

parks <- st_read("./data/original/arlington_parks/Park_Polygons.shp")
parks = parks %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

#
# Centroids ---------------------------------------
#

centroid <- st_centroid(parks)
centroid = centroid %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")

coords <- centroid %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(long = X, lat = Y)

centroid <- cbind(centroid, coords)

#
# Circle Polygons ---------------------------------------
#

spCircle(radius,
         spUnits = CRS(projargs = as.character(NA)),
         centerPoint = c(x = 0, y = 0),
         nptsPerimeter = 100,
         spID = paste("circle", .StemEnv$randomID(), sep = ":"),
         ...)

#
# TravelTime Polygon Maps --------------------------------
#

readRenviron("~/.Renviron")
traveltime_api <- Sys.getenv("TRAVELAPI")
traveltime_id <- Sys.getenv("TRAVELID")

for(i in 1:nrow(centroid)){
  park_iso5 <- traveltime_map(appId= traveltime_id,
                             apiKey = traveltime_api,
                             location=c(centroid$lat[i],centroid$long[i]),
                             traveltime=300,
                             type="walking",
                             departure="2021-08-07T08:00:00+01:00")
  
  park_iso10 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(centroid$lat[i],centroid$long[i]),
                              traveltime=600,
                              type="walking",
                              departure="2021-08-07T08:00:00+01:00")
  
  park_iso15 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(centroid$lat[i],centroid$long[i]),
                              traveltime=900,
                              type="walking",
                              departure="2021-08-07T08:00:00+01:00")
  
  saveRDS(park_iso5, file = paste0('./data/working/traveltime_isochrones/park_iso_5_',i,'.RDS'))
  saveRDS(park_iso10, file = paste0('./data/working/traveltime_isochrones/park_iso_10_',i,'.RDS'))
  saveRDS(park_iso15, file = paste0('./data/working/traveltime_isochrones/park_iso_15_',i,'.RDS'))
  
  m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
  m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
  m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
  
  m1 = m1 + m2 + m3
  
  #this isn't working and I'm mad
  mapshot(m1, file = paste0("./output/leaflet/park_tt_map_",i, ".png", sep = ""))
}


