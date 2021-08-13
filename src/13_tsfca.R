library(sf)
library(SpatialAcc)
library(osrm)
library(dplyr)
library(tidycensus)
library(mapview)

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

#write.csv(tract_dist_mat, "./data/working/park_to_tract_dist_mat.csv", row.names = FALSE)
tract_dist_mat <- read.csv("./data/working/park_to_tract_dist_mat.csv")

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

#write.csv(bgrp_dist_mat, "./data/working/park_to_bgrp_dist_mat.csv", row.names = FALSE)
bgrp_dist_mat <- read.csv("./data/working/park_to_bgrp_dist_mat.csv")

## run two-step floating catchment area ##

# tract level - acreage
all_tract_tsfca <- ac(p = acs_tract$total_pop, 
                      n = parks$Acreage, 
                      D = tract_dist_mat, 
                      d0 = 1609, 
                      family = "2SFCA")

acs_tract$all_tract_tsfca <- all_tract_tsfca
  
white_tract_tsfca <- ac(p = acs_tract$white, 
                        n = parks$Acreage, 
                        D = tract_dist_mat, 
                        d0 = 1609, 
                        family = "2SFCA")

acs_tract$white_tract_tsfca <- white_tract_tsfca

black_tract_tsfca <- ac(p = acs_tract$black, 
                        n = parks$Acreage, 
                        D = tract_dist_mat, 
                        d0 = 1609, 
                        family = "2SFCA")

acs_tract$black_tract_tsfca <- black_tract_tsfca

asian_tract_tsfca <- ac(p = acs_tract$asian, 
                        n = parks$Acreage, 
                        D = tract_dist_mat, 
                        d0 = 1609, 
                        family = "2SFCA")

acs_tract$asian_tract_tsfca <- asian_tract_tsfca

other_tract_tsfca <- ac(p = acs_tract$other, 
                        n = parks$Acreage, 
                        D = tract_dist_mat, 
                        d0 = 1609, 
                        family = "2SFCA")

acs_tract$other_tract_tsfca <- other_tract_tsfca

# bgrp level - acreage
all_bgrp_tsfca <- ac(p = acs_bgrp$total_pop, 
                     n = parks$Acreage, 
                     D = bgrp_dist_mat, 
                     d0 = 1609, 
                     family = "2SFCA")

acs_bgrp$all_bgrp_tsfca <- all_bgrp_tsfca

white_bgrp_tsfca <- ac(p = acs_bgrp$white, 
                       n = parks$Acreage, 
                       D = bgrp_dist_mat, 
                       d0 = 1609, 
                       family = "2SFCA")

acs_bgrp$white_bgrp_tsfca <- white_bgrp_tsfca

black_bgrp_tsfca <- ac(p = acs_bgrp$black, 
                       n = parks$Acreage, 
                       D = bgrp_dist_mat, 
                       d0 = 1609, 
                       family = "2SFCA")

acs_bgrp$black_bgrp_tsfca <- black_bgrp_tsfca

asian_bgrp_tsfca <- ac(p = acs_bgrp$asian, 
                       n = parks$Acreage, 
                       D = bgrp_dist_mat, 
                       d0 = 1609, 
                       family = "2SFCA")

acs_bgrp$asian_bgrp_tsfca <- asian_bgrp_tsfca

other_bgrp_tsfca <- ac(p = acs_bgrp$other, 
                       n = parks$Acreage, 
                       D = bgrp_dist_mat, 
                       d0 = 1609, 
                       family = "2SFCA")

acs_bgrp$other_bgrp_tsfca <- other_bgrp_tsfca

# plots #

# tract level

# all
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "all_tract_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# white
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "white_tract_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# black
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "black_tract_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# asian
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "asian_tract_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# other
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "other_tract_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# block group level

# all
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "all_bgrp_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# white
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "white_bgrp_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# black
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "black_bgrp_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# asian
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "asian_bgrp_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# other
mapview(st_geometry(parks), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "other_bgrp_tsfca", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))


# accessibility to playground
playground <- which(parks_amenities$playground == 1)

all_tract_tsfca_playground <- ac(p = acs_tract$total_pop, 
                                 n = parks$Acreage[playground], 
                                 D = tract_dist_mat[,playground], 
                                 d0 = 1609, 
                                 family = "2SFCA")

acs_tract$all_tract_tsfca_playground <- all_tract_tsfca_playground

white_tract_tsfca_playground <- ac(p = acs_tract$white, 
                                   n = parks$Acreage[playground], 
                                   D = tract_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

acs_tract$white_tract_tsfca_playground <- white_tract_tsfca_playground

black_tract_tsfca_playground <- ac(p = acs_tract$black, 
                                   n = parks$Acreage[playground], 
                                   D = tract_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

acs_tract$black_tract_tsfca_playground <- black_tract_tsfca_playground

asian_tract_tsfca_playground <- ac(p = acs_tract$asian, 
                                   n = parks$Acreage[playground], 
                                   D = tract_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

acs_tract$asian_tract_tsfca_playground <- asian_tract_tsfca_playground

other_tract_tsfca_playground <- ac(p = acs_tract$other, 
                                   n = parks$Acreage[playground], 
                                   D = tract_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

acs_tract$other_tract_tsfca_playground <- other_tract_tsfca_playground


# block group level
all_bgrp_tsfca_playground <- ac(p = acs_bgrp$total_pop, 
                                 n = parks$Acreage[playground], 
                                 D = bgrp_dist_mat[,playground], 
                                 d0 = 1609, 
                                 family = "2SFCA")

acs_bgrp$all_bgrp_tsfca_playground <- all_bgrp_tsfca_playground

white_bgrp_tsfca_playground <- ac(p = acs_bgrp$white, 
                                   n = parks$Acreage[playground], 
                                   D = bgrp_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

acs_bgrp$white_bgrp_tsfca_playground <- white_bgrp_tsfca_playground

black_bgrp_tsfca_playground <- ac(p = acs_bgrp$black, 
                                   n = parks$Acreage[playground], 
                                   D = bgrp_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

acs_bgrp$black_bgrp_tsfca_playground <- black_bgrp_tsfca_playground

asian_bgrp_tsfca_playground <- ac(p = acs_bgrp$asian, 
                                   n = parks$Acreage[playground], 
                                   D = bgrp_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

acs_bgrp$asian_bgrp_tsfca_playground <- asian_bgrp_tsfca_playground

other_bgrp_tsfca_playground <- ac(p = acs_bgrp$other, 
                                   n = parks$Acreage[playground], 
                                   D = bgrp_dist_mat[,playground], 
                                   d0 = 1609, 
                                   family = "2SFCA")

acs_bgrp$other_bgrp_tsfca_playground <- other_bgrp_tsfca_playground

# plot #

# tract level

# all
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "all_tract_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# white
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "white_tract_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# black
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "black_tract_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# asian
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "asian_tract_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# other
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "other_tract_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# block group level

mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "all_bgrp_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# white
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "white_bgrp_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# black
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "black_bgrp_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# asian
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "asian_bgrp_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# other
mapview(st_geometry(parks[playground,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "other_bgrp_tsfca_playground", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))



# accessibility to parking

# tract level
parking <- which(parks_amenities$free_parking == 1)

all_tract_tsfca_parking <- ac(p = acs_tract$total_pop, 
                              n = parks$Acreage[parking], 
                              D = tract_dist_mat[,parking], 
                              d0 = 1609, 
                              family = "2SFCA")

acs_tract$all_tract_tsfca_parking <- all_tract_tsfca_parking

white_tract_tsfca_parking <- ac(p = acs_tract$white, 
                                n = parks$Acreage[parking], 
                                D = tract_dist_mat[,parking], 
                                d0 = 1609, 
                                family = "2SFCA")

acs_tract$white_tract_tsfca_parking <- white_tract_tsfca_parking

black_tract_tsfca_parking <- ac(p = acs_tract$black, 
                                n = parks$Acreage[parking], 
                                D = tract_dist_mat[,parking], 
                                d0 = 1609, 
                                family = "2SFCA")

acs_tract$black_tract_tsfca_parking <- black_tract_tsfca_parking

asian_tract_tsfca_parking <- ac(p = acs_tract$asian, 
                                n = parks$Acreage[parking], 
                                D = tract_dist_mat[,parking], 
                                d0 = 1609, 
                                family = "2SFCA")

acs_tract$asian_tract_tsfca_parking <- asian_tract_tsfca_parking

other_tract_tsfca_parking <- ac(p = acs_tract$other, 
                                n = parks$Acreage[parking], 
                                D = tract_dist_mat[,parking], 
                                d0 = 1609, 
                                family = "2SFCA")

acs_tract$other_tract_tsfca_parking <- other_tract_tsfca_parking

# block group level
all_bgrp_tsfca_parking <- ac(p = acs_bgrp$total_pop, 
                                n = parks$Acreage[parking], 
                                D = bgrp_dist_mat[,parking], 
                                d0 = 1609, 
                                family = "2SFCA")

acs_bgrp$all_bgrp_tsfca_parking <- all_bgrp_tsfca_parking

white_bgrp_tsfca_parking <- ac(p = acs_bgrp$white, 
                                  n = parks$Acreage[parking], 
                                  D = bgrp_dist_mat[,parking], 
                                  d0 = 1609, 
                                  family = "2SFCA")

acs_bgrp$white_bgrp_tsfca_parking <- white_bgrp_tsfca_parking

black_bgrp_tsfca_parking <- ac(p = acs_bgrp$black, 
                                  n = parks$Acreage[parking], 
                                  D = bgrp_dist_mat[,parking], 
                                  d0 = 1609, 
                                  family = "2SFCA")

acs_bgrp$black_bgrp_tsfca_parking <- black_bgrp_tsfca_parking

asian_bgrp_tsfca_parking <- ac(p = acs_bgrp$asian, 
                                  n = parks$Acreage[parking], 
                                  D = bgrp_dist_mat[,parking], 
                                  d0 = 1609, 
                                  family = "2SFCA")

acs_bgrp$asian_bgrp_tsfca_parking <- asian_bgrp_tsfca_parking

other_bgrp_tsfca_parking <- ac(p = acs_bgrp$other, 
                                  n = parks$Acreage[parking], 
                                  D = bgrp_dist_mat[,parking], 
                                  d0 = 1609, 
                                  family = "2SFCA")

acs_bgrp$other_bgrp_tsfca_parking <- other_bgrp_tsfca_parking

# plots #

# tract level

# all
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "all_tract_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# white
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "white_tract_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# black
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "black_tract_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# asian
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "asian_tract_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# other
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "other_tract_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# block group level

# all
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "all_bgrp_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# white
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "white_bgrp_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# black
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "black_bgrp_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# asian
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "asian_bgrp_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

# other
mapview(st_geometry(parks[parking,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_bgrp, 
          zcol = "other_bgrp_tsfca_parking", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))


## access to basketball courts

bball <- which(parks_amenities$basketball == 1)

all_tract_tsfca_bball <- ac(p = acs_tract$total_pop, 
                            n = parks$Acreage[bball], 
                            D = tract_dist_mat[,bball], 
                            d0 = 1609, 
                            family = "2SFCA")

acs_tract$all_tract_tsfca_bball <- all_tract_tsfca_bball

mapview(st_geometry(parks[bball,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "all_tract_tsfca_bball", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))

## access to tennis courts

tennis <- which(parks_amenities$tennis == 1)

all_tract_tsfca_tennis <- ac(p = acs_tract$total_pop, 
                            n = parks$Acreage[tennis], 
                            D = tract_dist_mat[,tennis], 
                            d0 = 1609, 
                            family = "2SFCA")

acs_tract$all_tract_tsfca_tennis <- all_tract_tsfca_tennis

mapview(st_geometry(parks[tennis,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "all_tract_tsfca_tennis", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))


## access to charcoal grills

grill <- which(parks_amenities$charcoal_grill == 1)

all_tract_tsfca_grill <- ac(p = acs_tract$total_pop, 
                             n = parks$Acreage[grill], 
                             D = tract_dist_mat[,grill], 
                             d0 = 1609, 
                             family = "2SFCA")

acs_tract$all_tract_tsfca_grill <- all_tract_tsfca_grill

mapview(st_geometry(parks[grill,]), 
        cex =.5, 
        layer.name = "Parks in Arlington County", 
        col.region = "gray",
        color = "gray") + 
  mapview(acs_tract, 
          zcol = "all_tract_tsfca_grill", 
          layer.name = "TSFCA",  
          col.regions = sf.colors(alpha = 0.1))
