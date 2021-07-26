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

orig_parks <- st_read("./data/original/arlington_parks/Park_Polygons.shp")
orig_parks <- orig_parks %>% filter(Ownership == "Arlington County Park") # 148

parks <- orig_parks %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")
parks <- parks %>% filter(Ownership == "Arlington County Park") # 148

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

residential <- read_sf("./data/working/corelogic/residential.csv")
residential_sf <- st_as_sf(residential, coords = c("parcel_level_longitude", "parcel_level_latitude"))
st_crs(residential_sf) <- "+proj=longlat +datum=WGS84"

#
# Create Centroids ---------------------------------------
#

# temp fix for sf 1.0 s2 issue
sf_use_s2(FALSE)

centroid <- st_centroid(orig_parks)
centroid <- centroid %>%
  st_transform("+proj=longlat +datum=WGS84")

coords <- centroid %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(long = X, lat = Y)

centroid <- cbind(centroid, coords)

#
# Circle Polygons ---------------------------------------
#

st_crs(centroid) <- 4326
centroid <- st_transform(centroid, crs = 7801)
circles <- st_buffer(centroid, dist = 804)
circles <- st_transform(circles, "+proj=longlat +datum=WGS84")

park_map <- mapview(st_geometry(parks), cex =.5, layer.name = "parks", color = colors[3])
circle_map <- mapview(st_geometry(circles), cex =.5, layer.name = "1/2 mile circles", color = colors[4])
residential <- mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])

map <- park_map + circle_map + residential
map

write_rds(circles, "./data/working/circle_poly.Rds")

#
# TravelTime Polygon Maps --------------------------------
#

readRenviron("~/.Renviron")
traveltime_api <- Sys.getenv("TRAVELAPI")
traveltime_id <- Sys.getenv("TRAVELID")

# walking

for(i in 1:nrow(centroid)){
  Sys.sleep(20)
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
}

# Driving 

for(i in 1:nrow(centroid)){
  Sys.sleep(20)
  park_iso5 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(centroid$lat[i],centroid$long[i]),
                              traveltime=300,
                              type="driving",
                              departure="2021-08-07T08:00:00+01:00")
  
  park_iso10 <- traveltime_map(appId= traveltime_id,
                               apiKey = traveltime_api,
                               location=c(centroid$lat[i],centroid$long[i]),
                               traveltime=600,
                               type="driving",
                               departure="2021-08-07T08:00:00+01:00")
  
  park_iso15 <- traveltime_map(appId= traveltime_id,
                               apiKey = traveltime_api,
                               location=c(centroid$lat[i],centroid$long[i]),
                               traveltime=900,
                               type="driving",
                               departure="2021-08-07T08:00:00+01:00")
  
  saveRDS(park_iso5, file = paste0('./data/working/traveltime_isochrones/park_iso_drv_5_',i,'.RDS'))
  saveRDS(park_iso10, file = paste0('./data/working/traveltime_isochrones/park_iso_drv_10_',i,'.RDS'))
  saveRDS(park_iso15, file = paste0('./data/working/traveltime_isochrones/park_iso_drv_15_',i,'.RDS'))
}

# Public Transport 

for(i in 1:nrow(centroid)){
  Sys.sleep(20)
  park_iso5 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(centroid$lat[i],centroid$long[i]),
                              traveltime=300,
                              type="public_transport",
                              departure="2021-08-07T08:00:00+01:00")
  
  park_iso10 <- traveltime_map(appId= traveltime_id,
                               apiKey = traveltime_api,
                               location=c(centroid$lat[i],centroid$long[i]),
                               traveltime=600,
                               type="public_transport",
                               departure="2021-08-07T08:00:00+01:00")
  
  park_iso15 <- traveltime_map(appId= traveltime_id,
                               apiKey = traveltime_api,
                               location=c(centroid$lat[i],centroid$long[i]),
                               traveltime=900,
                               type="public_transport",
                               departure="2021-08-07T08:00:00+01:00")
  
  saveRDS(park_iso5, file = paste0('./data/working/traveltime_isochrones/park_iso_pt_5_',i,'.RDS'))
  saveRDS(park_iso10, file = paste0('./data/working/traveltime_isochrones/park_iso_pt_10_',i,'.RDS'))
  saveRDS(park_iso15, file = paste0('./data/working/traveltime_isochrones/park_iso_pt_15_',i,'.RDS'))
}


#
# Boundary Polygons ----------------------------------------
#
options(osrm.profile = "walk")
for(i in 1:nrow(parks)){
  polygon <- osrmIsochrone(parks[i,],
                          breaks = c(5,10,15),
                          exclude = NULL,
                          res = 30,
                          returnclass = "sf")
  saveRDS(polygon, file = paste0('./data/working/osrm_isochrones/park_iso_',i,'.RDS'))
}
