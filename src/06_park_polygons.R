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
parks = parks %>% filter(Ownership == "Arlington County Park") # 148

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

residential <- read_sf("./data/working/corelogic/residential.csv")
residential_sf <- st_as_sf(residential, coords = c("parcel_level_longitude", "parcel_level_latitude"))
st_crs(residential_sf) <- "+proj=longlat +datum=WGS84"

#
# Create Centroids ---------------------------------------
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

st_crs(centroid)$units

circles <- st_buffer(centroid, dist = .007)

park_map = mapview(st_geometry(parks), cex =.5, layer.name = "parks", color = colors[3])
circle_map = mapview(st_geometry(circles), cex =.5, layer.name = "1/2 mile circles", color = colors[4])
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])

map = park_map + circle_map + residential
map

write_rds(circles, "./data/working/circle_poly.Rds")

#
# TravelTime Polygon Maps --------------------------------
#

readRenviron("~/.Renviron")
traveltime_api <- Sys.getenv("TRAVELAPI")
traveltime_id <- Sys.getenv("TRAVELID")

# just go by 10s I guess to save the polygons bc limitations lmao
for(i in 1:10){
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

  # residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
  # 
  # m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
  # m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
  # m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
  # 
  # m1 = m1 + m2 + m3 + residential
}


