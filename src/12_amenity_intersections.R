library(tidyverse)
library(sf)
library(sp)

parks <- st_read("./data/original/arlington_parks/Park_Polygons.shp")
parks = parks %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")
parks = parks %>% filter(Ownership == "Arlington County Park") # 148

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

residential <- read_sf("./data/working/corelogic/residential.csv")
residential_sf <- st_as_sf(residential, coords = c("parcel_level_longitude", "parcel_level_latitude"))
st_crs(residential_sf) <- "+proj=longlat +datum=WGS84"

## park amenities
parks_amenities <- read.csv("./data/working/parks_amenities.csv")

# get indices for parks that have certain amenities
tennis_parks <- which(parks_amenities$tennis == 1)
playground_parks <- which(parks_amenities$playground == 1)
parking_parks <- which(parks_amenities$free_parking == 1)
basketball_parks <- which(parks_amenities$basketball == 1)
grill_parks <- which(parks_amenities$charcoal_grill == 1)

for (i in playground_parks) {
  iso_5_temp <- paste("./data/working/traveltime_isochrones/park_iso_5_", i, ".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
}


