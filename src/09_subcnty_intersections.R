library(tidyverse)
library(tidygeocoder)
library(sf)
library(jsonlite)

acs_tract <- read_rds("./data/working/acs_tract.Rds")
acs_bgrp <- read_rds("./data/working/acs_bgrp.Rds")

#
# Combining block and park data ------------------------------------------------------------------------
#

# read in parks data
parks <- st_read("./data/original/arlington_parks/Park_Polygons.shp")
parks = parks %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")
parks = parks %>% filter(Ownership == "Arlington County Park") # 148

parks$address <- paste0(parks$AddressNum, " ", parks$Street)
parks$county <- "Arlington County"
parks$state <- "Virginia"

# use tinygeocoder to get lats and longs
parks <- parks %>%
  geocode(street = address, county = county, state = state, method = "osm")

# use eric's handy dandy function to get the geoid
geo2fips <- function(latitude, longitude) {
  url <- "https://geo.fcc.gov/api/census/area?lat=%f&lon=%f&format=json"
  res <- jsonlite::fromJSON(sprintf(url, latitude, longitude))[["results"]][["block_fips"]]
  unique(res)
}

# loop to get geoids by park row - these geoids don't merge maybe because block verus block group fips
for(i in 1:nrow(parks)){
  parks$geoid[i] <- geo2fips(parks$lat[i], parks$long[i])
}

# head(acs_bgrp$GEOID)
# head(acs_tract$GEOID)
# head(parks$geoid)
parks$bgrp_geoid <- substr(parks$geoid, start = 1, stop = 12)
parks$tract_geoid <- substr(parks$geoid, start = 1, stop = 11)

# head(parks$bgrp_geoid)
# head(parks$tract_geoid)

# save to use later
write_rds(parks, "./data/working/parks.Rds")

#
# Coverage at the Block and Group Level --------------------
#

# they're still not merging :/
area_bgrp <- left_join(acs_bgrp, parks, by = c("GEOID" = "geoid"))
area_tract <- left_join(acs_tract, parks, by = c("GEOID" = "geoid"))

area_bgrp <- area_bgrp %>% 
  select(GEOID, Acreage) %>%
  group_by(GEOID) %>%
  mutate(acres = mean(Acreage, na.rm = T), count = n()) %>%
  unique()

area_tract <- area_tract %>% 
  select(GEOID, Acreage) %>%
  group_by(GEOID) %>%
  mutate(acres = mean(Acreage, na.rm = T), count = n()) %>%
  unique()

acs_bgrp <- left_join(acs_bgrp, area_bgrp, by = c("GEOID" = "GEOID"))
acs_tract <- left_join(acs_tract, area_tract, by = c("GEOID" = "GEOID"))

read_rds(acs_tract, "./data/working/acs_tract.Rds")
read_rds(acs_bgrp, "./data/working/acs_bgrp.Rds")
