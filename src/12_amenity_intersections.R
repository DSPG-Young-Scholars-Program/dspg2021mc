library(tidyverse)
library(sf)
library(sp)
library(mapview)
library(leaflet)

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

# there are maps for all five of the above ammenities for walking
# and public transport maps for tennis, playgrounds, and basketball ammenities

# tennis map ----------------------------------------------------------------------------
for (i in 1:2){
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", tennis_parks[1],".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", tennis_parks[2],".RDS")
  park_iso_5_2 <- readRDS(iso_5_temp)
  park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)
  
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", tennis_parks[1],".RDS")
  park_iso_10_1 <- readRDS(iso_10_temp)
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", tennis_parks[2],".RDS")
  park_iso_10_2 <- readRDS(iso_10_temp)
  park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)
  
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", tennis_parks[1],".RDS")
  park_iso_15_1 <- readRDS(iso_15_temp)
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", tennis_parks[2],".RDS")
  park_iso_15_2 <- readRDS(iso_15_temp)
  park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)
}
  
# loop through isochrones and continiously union with last
for(i in 3:length(tennis_parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}

for(i in 3:length(tennis_parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}

for(i in 3:length(tennis_parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
table
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_tennis.csv", row.names = FALSE)

# make a picture
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
tennis_map = m1 + m2 + m3 + residential
tennis_map

# playground_parks map ----------------------------------------------------------------------------
for (i in 1:2){
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", playground_parks[1],".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", playground_parks[2],".RDS")
  park_iso_5_2 <- readRDS(iso_5_temp)
  park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)
  
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", playground_parks[1],".RDS")
  park_iso_10_1 <- readRDS(iso_10_temp)
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", playground_parks[2],".RDS")
  park_iso_10_2 <- readRDS(iso_10_temp)
  park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)
  
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", playground_parks[1],".RDS")
  park_iso_15_1 <- readRDS(iso_15_temp)
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", playground_parks[2],".RDS")
  park_iso_15_2 <- readRDS(iso_15_temp)
  park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)
}

# loop through isochrones and continiously union with last
for(i in 3:length(playground_parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}

for(i in 3:length(playground_parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}

for(i in 3:length(playground_parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
table
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_playground.csv", row.names = FALSE)

# make a picture
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
playground_map = m1 + m2 + m3 + residential
playground_map

# parking_parks map ----------------------------------------------------------------------------
for (i in 1:2){
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", parking_parks[1],".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", parking_parks[2],".RDS")
  park_iso_5_2 <- readRDS(iso_5_temp)
  park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)
  
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", parking_parks[1],".RDS")
  park_iso_10_1 <- readRDS(iso_10_temp)
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", parking_parks[2],".RDS")
  park_iso_10_2 <- readRDS(iso_10_temp)
  park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)
  
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", parking_parks[1],".RDS")
  park_iso_15_1 <- readRDS(iso_15_temp)
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", parking_parks[2],".RDS")
  park_iso_15_2 <- readRDS(iso_15_temp)
  park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)
}

# loop through isochrones and continiously union with last
for(i in 3:length(parking_parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}

for(i in 3:length(parking_parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}

for(i in 3:length(parking_parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
table
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_parking.csv", row.names = FALSE)

# make a picture
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
parking_map = m1 + m2 + m3 + residential
parking_map

# basketball_parks map ----------------------------------------------------------------------------
for (i in 1:2){
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", basketball_parks[1],".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", basketball_parks[2],".RDS")
  park_iso_5_2 <- readRDS(iso_5_temp)
  park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)
  
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", basketball_parks[1],".RDS")
  park_iso_10_1 <- readRDS(iso_10_temp)
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", basketball_parks[2],".RDS")
  park_iso_10_2 <- readRDS(iso_10_temp)
  park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)
  
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", basketball_parks[1],".RDS")
  park_iso_15_1 <- readRDS(iso_15_temp)
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", basketball_parks[2],".RDS")
  park_iso_15_2 <- readRDS(iso_15_temp)
  park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)
}

# loop through isochrones and continiously union with last
for(i in 3:length(basketball_parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}

for(i in 3:length(basketball_parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}

for(i in 3:length(basketball_parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
table
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_basketball.csv", row.names = FALSE)

# make a picture
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
basket_map = m1 + m2 + m3 + residential
basket_map


# grill_parks map ----------------------------------------------------------------------------
for (i in 1:2){
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", grill_parks[1],".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_5_", grill_parks[2],".RDS")
  park_iso_5_2 <- readRDS(iso_5_temp)
  park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)
  
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", grill_parks[1],".RDS")
  park_iso_10_1 <- readRDS(iso_10_temp)
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_10_", grill_parks[2],".RDS")
  park_iso_10_2 <- readRDS(iso_10_temp)
  park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)
  
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", grill_parks[1],".RDS")
  park_iso_15_1 <- readRDS(iso_15_temp)
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_15_", grill_parks[2],".RDS")
  park_iso_15_2 <- readRDS(iso_15_temp)
  park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)
}

# loop through isochrones and continiously union with last
for(i in 3:length(grill_parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}

for(i in 3:length(grill_parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}

for(i in 3:length(grill_parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
table
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_grill.csv", row.names = FALSE)

# make a picture
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
grill_map = m1 + m2 + m3 + residential
grill_map

# public transport ammenities maps -----------------------------------------
# tennis, playground, basketball
for (i in 1:2){
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_5_", tennis_parks[1],".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_5_", tennis_parks[2],".RDS")
  park_iso_5_2 <- readRDS(iso_5_temp)
  park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)
  
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_10_", tennis_parks[1],".RDS")
  park_iso_10_1 <- readRDS(iso_10_temp)
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_10_", tennis_parks[2],".RDS")
  park_iso_10_2 <- readRDS(iso_10_temp)
  park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)
  
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_15_", tennis_parks[1],".RDS")
  park_iso_15_1 <- readRDS(iso_15_temp)
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_15_", tennis_parks[2],".RDS")
  park_iso_15_2 <- readRDS(iso_15_temp)
  park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)
}

# loop through isochrones and continiously union with last
for(i in 3:length(tennis_parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}

for(i in 3:length(tennis_parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}

for(i in 3:length(tennis_parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
table
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_pt_tennis.csv", row.names = FALSE)

# make a picture
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
tennis_pt_map = m1 + m2 + m3 + residential
tennis_pt_map

for (i in 1:2){
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_5_", playground_parks[1],".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_5_", playground_parks[2],".RDS")
  park_iso_5_2 <- readRDS(iso_5_temp)
  park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)
  
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_10_", playground_parks[1],".RDS")
  park_iso_10_1 <- readRDS(iso_10_temp)
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_10_", playground_parks[2],".RDS")
  park_iso_10_2 <- readRDS(iso_10_temp)
  park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)
  
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_15_", playground_parks[1],".RDS")
  park_iso_15_1 <- readRDS(iso_15_temp)
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_15_", playground_parks[2],".RDS")
  park_iso_15_2 <- readRDS(iso_15_temp)
  park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)
}

# loop through isochrones and continiously union with last
for(i in 3:length(playground_parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}

for(i in 3:length(playground_parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}

for(i in 3:length(playground_parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
table
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_pt_playground.csv", row.names = FALSE)

# make a picture
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
playground_pt_map = m1 + m2 + m3 + residential
playground_pt_map

for (i in 1:2){
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_5_", basketball_parks[1],".RDS")
  park_iso_5_1 <- readRDS(iso_5_temp)
  iso_5_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_5_", basketball_parks[2],".RDS")
  park_iso_5_2 <- readRDS(iso_5_temp)
  park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)
  
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_10_", basketball_parks[1],".RDS")
  park_iso_10_1 <- readRDS(iso_10_temp)
  iso_10_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_10_", basketball_parks[2],".RDS")
  park_iso_10_2 <- readRDS(iso_10_temp)
  park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)
  
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_15_", basketball_parks[1],".RDS")
  park_iso_15_1 <- readRDS(iso_15_temp)
  iso_15_temp <- paste0("./data/working/traveltime_isochrones/park_iso_pt_15_", basketball_parks[2],".RDS")
  park_iso_15_2 <- readRDS(iso_15_temp)
  park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)
}

# loop through isochrones and continiously union with last
for(i in 3:length(basketball_parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}

for(i in 3:length(basketball_parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}

for(i in 3:length(basketball_parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
table
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_pt_basketball.csv", row.names = FALSE)

# make a picture
residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
basket_pt_map = m1 + m2 + m3 + residential
basket_pt_map