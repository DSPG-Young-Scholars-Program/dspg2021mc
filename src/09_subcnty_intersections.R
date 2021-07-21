library(tidyverse)
library(tidygeocoder)
library(sf)
library(jsonlite)

source("utils.R")

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

parks_geoids <- apply(parks, 1, function(x) geo2fips(x$lat, x$long))

parks$bgrp_geoid <- parks_geoids[1,]
parks$tract_geoid <- parks_geoids[2,]

# loop to get geoids by park row - these geoids don't merge maybe because block verus block group fips
#for(i in 1:nrow(parks)){
#  parks$geoid[i] <- geo2fips(parks$lat[i], parks$long[i])
#}

# head(acs_bgrp$GEOID)
# head(acs_tract$GEOID)
# head(parks$geoid)
#parks$bgrp_geoid <- substr(parks$geoid, start = 1, stop = 12)
#parks$tract_geoid <- substr(parks$geoid, start = 1, stop = 11)

# head(parks$bgrp_geoid)
# head(parks$tract_geoid)

# save to use later
write_rds(parks, "./data/working/parks.Rds")

#
# Coverage at the Block and Group Level --------------------
#

length(unique(parks$tract_geoid))

# they're still not merging :/
area_bgrp <- left_join(acs_bgrp, parks, by = c("GEOID" = "bgrp_geoid"))
area_tract <- left_join(acs_tract, parks, by = c("GEOID" = "tract_geoid"))

area_bgrp <- area_bgrp %>% 
  as.data.frame() %>%
  select(GEOID, Acreage) %>%
  na.omit() %>%
  group_by(GEOID) %>%
  mutate(acres = mean(Acreage, na.rm = T), count = n()) %>%
  unique() %>%
  select(GEOID, acres, count) %>%
  unique()

area_tract <- area_tract %>% 
  as.data.frame() %>%
  select(GEOID, Acreage) %>%
  group_by(GEOID) %>%
  mutate(acres = mean(Acreage, na.rm = T), count = n()) %>%
  unique() %>%
  select(GEOID, acres, count) %>%
  unique()

acs_bgrp <- left_join(acs_bgrp, area_bgrp, by = c("GEOID" = "GEOID"))
acs_tract <- left_join(acs_tract, area_tract, by = c("GEOID" = "GEOID"))

acs_bgrp$acres[is.nan(acs_bgrp$acres)] <- 0
acs_tract$acres[is.nan(acs_tract$acres)] <- 0

write_rds(acs_tract, "./data/working/acs_tract.Rds")
write_rds(acs_bgrp, "./data/working/acs_bgrp.Rds")

#
# Test Differences in Groups -------------------
#

# differences between above median and below median black communities - less parks in black communities
median(acs_tract$black, na.rm = T)
acs_tract$black_group <- ifelse(acs_tract$black >= 6.2277, 1, 0)
acs_tract$black_group[is.na(acs_tract$black_group)] <- 0
var.test(acres ~ black_group, data = acs_tract)

# differences between above median and below median hispanic communities - less parks in hispanic communities
mean(acs_tract$hispanic, na.rm = T)
acs_tract$hispanic_group <- ifelse(acs_tract$hispanic >= 14.52765, 1, 0)
acs_tract$hispanic_group[is.na(acs_tract$hispanic_group)] <- 0
var.test(acres ~ hispanic_group, data = acs_tract)

# differences between above median and below median inpov communities - less parks in poor communities
mean(acs_tract$inpov, na.rm = T)
acs_tract$inpov_group <- ifelse(acs_tract$inpov >= 6.487571, 1, 0)
acs_tract$inpov_group[is.na(acs_tract$inpov_group)] <- 0
var.test(acres ~ inpov_group, data = acs_tract)
