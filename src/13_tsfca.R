library(sf)
library(SpatialAcc)
library(osrm)
library(dplyr)
library(tidycensus)

# read in ACS data
Sys.getenv("CENSUS_API_KEY")

acs_vars <- c(  
  # total population
  "B01003_001",
  # Hispanic ethnicity
  "B03001_003", "B03001_001",
  # White
  "B02001_002", "B02001_001",
  # Black
  "B02001_003",
  # Asian
  "B02001_005",
  # Other
  "B02001_004", "B02001_006", "B02001_007",
  "B02001_008", "B02001_009", "B02001_010"
  )

data_tract <- get_acs(geography = "tract", 
                      state = 51, 
                      county = 013,
                      variables = acs_vars,
                      year = 2019, 
                      survey = "acs5",
                      cache_table = TRUE, 
                      output = "wide", 
                      geometry = TRUE,
                      keep_geo_vars = TRUE)

acs_tract <- data_tract %>%
  transmute(STATEFP = STATEFP,
            COUNTYFP = COUNTYFP,
            TRACTCE = TRACTCE,
            GEOID = GEOID,
            NAME.x = NAME.x,
            NAME.y = NAME.y,
            ALAND = ALAND,
            AWATER = AWATER,
            total_pop = B01003_001E,
            hispanic = B03001_003E,
            white = B02001_002E,
            black = B02001_003E,
            asian = B02001_005E,
            other_race = B02001_004E + B02001_006E + B02001_007E + B02001_008E + B02001_009E + B02001_010E
            )

acs_tract$total_pop[acs_tract$total_pop == 0] <- 0.0001
acs_tract$hispanic[acs_tract$hispanic == 0] <- 0.0001
acs_tract$white[acs_tract$white == 0] <- 0.0001
acs_tract$black[acs_tract$black == 0] <- 0.0001
acs_tract$asian[acs_tract$asian == 0] <- 0.0001
acs_tract$other[acs_tract$other == 0] <- 0.0001


data_bgrp <- get_acs(geography = "block group", 
                     state = 51, 
                     county = 013,
                     variables = acs_vars,
                     year = 2019, 
                     survey = "acs5",
                     cache_table = TRUE, 
                     output = "wide", 
                     geometry = TRUE,
                     keep_geo_vars = TRUE)

acs_bgrp <- data_bgrp %>%
  transmute(STATEFP = STATEFP,
            COUNTYFP = COUNTYFP,
            TRACTCE = TRACTCE,
            GEOID = GEOID,
            NAME.x = NAME.x,
            NAME.y = NAME.y,
            ALAND = ALAND,
            AWATER = AWATER,
            total_pop = B01003_001E,
            hispanic = B03001_003E,
            white = B02001_002E,
            black = B02001_003E,
            asian = B02001_005E,
            other_race = B02001_004E + B02001_006E + B02001_007E + B02001_008E + B02001_009E + B02001_010E
  )

acs_bgrp$total_pop[acs_bgrp$total_pop == 0] <- 0.0001
acs_bgrp$hispanic[acs_bgrp$hispanic == 0] <- 0.0001
acs_bgrp$white[acs_bgrp$white == 0] <- 0.0001
acs_bgrp$black[acs_bgrp$black == 0] <- 0.0001
acs_bgrp$asian[acs_bgrp$asian == 0] <- 0.0001
acs_bgrp$other[acs_bgrp$other == 0] <- 0.0001

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

# amenities
parks_amenities <- read.csv("./data/working/parks_amenities.csv")


## calculate distances between each
## park and neighborhood centroid
tract_dist <- osrmTable(src = tract_centroids,
                        dst = parks_centroids,
                        measure = "distance")

tract_dist_mat <- tract_dist$distances

write.csv(tract_dist_mat, "./data/working/park_to_tract_dist_mat.csv", row.names = FALSE)

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

write.csv(bgrp_dist_mat, "./data/working/park_to_bgrp_dist_mat.csv", row.names = FALSE)

## run two-step floating catchment area ##

# tract level
all_tract_tsfca <- ac(p = acs_tract$total_pop, 
                      n = parks$Acreage, 
                      D = tract_dist_mat, 
                      d0 = 1609, 
                      family = "2SFCA")
  
white_tract_tsfca <- ac(p = acs_tract$white, 
                        n = parks$Acreage, 
                        D = tract_dist_mat, 
                        d0 = 1609, 
                        family = "2SFCA")

black_tract_tsfca <- ac(p = acs_tract$black, 
                        n = parks$Acreage, 
                        D = tract_dist_mat, 
                        d0 = 1609, 
                        family = "2SFCA")

asian_tract_tsfca <- ac(p = acs_tract$asian, 
                        n = parks$Acreage, 
                        D = tract_dist_mat, 
                        d0 = 1609, 
                        family = "2SFCA")

other_tract_tsfca <- ac(p = acs_tract$other, 
                        n = parks$Acreage, 
                        D = tract_dist_mat, 
                        d0 = 1609, 
                        family = "2SFCA")

# bgrp level
all_bgrp_tsfca <- ac(p = acs_bgrp$total_pop, 
                     n = parks$Acreage, 
                     D = bgrp_dist_mat, 
                     d0 = 1609, 
                     family = "2SFCA")

white_bgrp_tsfca <- ac(p = acs_bgrp$white, 
                       n = parks$Acreage, 
                       D = bgrp_dist_mat, 
                       d0 = 1609, 
                       family = "2SFCA")

black_bgrp_tsfca <- ac(p = acs_bgrp$black, 
                       n = parks$Acreage, 
                       D = bgrp_dist_mat, 
                       d0 = 1609, 
                       family = "2SFCA")

asian_bgrp_tsfca <- ac(p = acs_bgrp$asian, 
                       n = parks$Acreage, 
                       D = bgrp_dist_mat, 
                       d0 = 1609, 
                       family = "2SFCA")

other_bgrp_tsfca <- ac(p = acs_bgrp$other, 
                       n = parks$Acreage, 
                       D = bgrp_dist_mat, 
                       d0 = 1609, 
                       family = "2SFCA")


# accessibility to playground
playground <- which(parks_amenities$playground == 1)

all_tract_tsfca_playground <- ac(p = acs_tract$total_pop, 
                                 n = parks$Acreage[playground], 
                                 D = tract_dist_mat[,playground], 
                                 d0 = 1609, 
                                 family = "2SFCA")

black_tract_tsfca_playground <- ac(p = acs_tract$black, 
                                   n = parks$Acreage[playground], 
                                   D = tract_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

# accessibility to basketball
basketball <- which(parks_amenities$basketball == 1)

all_tract_tsfca_basketball <- ac(p = acs_tract$total_pop, 
                                 n = parks$Acreage[basketball], 
                                 D = tract_dist_mat[,basketball], 
                                 d0 = 1609, 
                                 family = "2SFCA")

black_tract_tsfca_basketball <- ac(p = acs_tract$black, 
                                   n = parks$Acreage[basketball], 
                                   D = tract_dist_mat[,basketball], 
                                   d0 = 1609, 
                                   family = "2SFCA")






