library(SpatialAcc)
library(osrm)
library(dplyr)

sf_use_s2(FALSE)

# read in ACS data
acs_tract <- readRDS("./data/working/acs_tract.Rds")
acs_bgrp <- readRDS("./data/working/acs_bgrp.Rds")

# transform to utm with meter units
acs_tract_utm <- st_transform(acs_tract, crs = "+proj=utm +zone=18S +datum=NAD83 +ellps=GRS80") 
acs_bgrp_utm <- st_transform(acs_bgrp, crs = "+proj=utm +zone=18S +datum=NAD83 +ellps=GRS80")

tract_centroids <- st_centroid(acs_tract_utm) %>%
  st_transform(acs_tract, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84") %>%
  st_coordinates() %>%
  as.data.frame()

tract_centroids$id <- 1:nrow(tract_centroids)
tract_centroids <- tract_centroids %>%
  select(id,
         lon = X,
         lat = Y)

bgrp_centroids <- st_centroid(acs_bgrp_utm) %>%
  st_transform(acs_tract, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84") %>%
  st_coordinates() %>%
  as.data.frame()

bgrp_centroids$id <- 1:nrow(bgrp_centroids)
bgrp_centroids <- bgrp_centroids %>%
  select(id,
         lon = X,
         lat = Y)


## park data
# transform to utm with meter units
parks <- st_read("./data/original/arlington_parks/Park_Polygons.shp") %>%
  filter(Ownership == "Arlington County Park")

parks_centroids <- st_centroid(parks) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84") %>%
  st_coordinates() %>%
  as.data.frame()

parks_centroids$id <- 1:nrow(parks_centroids)
parks_centroids <- parks_centroids %>%
  select(id,
         lon = X,
         lat = Y)


## calculate distances between each
## park and neighborhood centroid
tract_dist <- osrmTable(src = tract_centroids,
                        dst = parks_centroids,
                        measure = "distance")

tract_dist_mat <- tract_dist$distances

bgrp_dist1 <- osrmTable(src = bgrp_centroids[1:60,],
                        dst = parks_centroids,
                        measure = "distance")

bgrp_dist2 <- osrmTable(src = bgrp_centroids[61:120,],
                        dst = parks_centroids,
                        measure = "distance")

bgrp_dist3 <- osrmTable(src = bgrp_centroids[121:181,],
                        dst = parks_centroids,
                        measure = "distance")

bgrp_dist_mat <- rbind(bgrp_dist1$distances,
                       bgrp_dist2$distances,
                       bgrp_dist3$distances)

# run two-step floating catchment area
tract_tsfca <- ac(p = acs_tract$black, 
                  n = acs_tract$acres, 
                  D = tract_dist_mat, 
                  d0 = 1609, 
                  family = "2SFCA")

bgrp_tsfca <- ac(p = acs_bgrp$black, 
                 n = acs_bgrp$acres, 
                 D = bgrp_dist_mat, 
                 d0 = 1609, 
                 family = "2SFCA")






