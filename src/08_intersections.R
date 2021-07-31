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

#
# Intersections -----------------------------------
#

# Traveltime Point Intersections - Walking ------------------------------------------
# make initial unions of 1 and 2 so that loop is easier
park_iso_5_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_5_1.RDS")
park_iso_5_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_5_2.RDS")
park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)

park_iso_10_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_10_1.RDS")
park_iso_10_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_10_2.RDS")
park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)

park_iso_15_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_15_1.RDS")
park_iso_15_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_15_2.RDS")
park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)

# loop through isochrones and continiously union with last
for(i in 3:nrow(parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}
plot(st_geometry(park_iso5))


for(i in 3:nrow(parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}
plot(st_geometry(park_iso10))

for(i in 3:nrow(parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}
plot(st_geometry(park_iso15))

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

write_rds(park_iso5, "./data/working/traveltime_isochrones/park_intersect_w_5.Rds")
write_rds(park_iso10, "./data/working/traveltime_isochrones/park_intersect_w_10.Rds")
write_rds(park_iso15, "./data/working/traveltime_isochrones/park_intersect_w_15.Rds")

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
write.csv(table, file = "./data/working/park_iso_table.csv", row.names = FALSE)

residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
walk_map = m1 + m2 + m3 + residential
walk_map

# Traveltime Point Intersections - Driving ------------------------------------------
# make initial unions of 1 and 2 so that loop is easier
park_iso_5_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_drv_5_1.RDS")
park_iso_5_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_drv_5_2.RDS")
park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)

park_iso_10_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_drv_10_1.RDS")
park_iso_10_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_drv_10_2.RDS")
park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)

park_iso_15_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_drv_15_1.RDS")
park_iso_15_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_drv_15_2.RDS")
park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)

# loop through isochrones and continiously union with last
for(i in 3:nrow(parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_drv_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}
plot(st_geometry(park_iso5))


for(i in 3:nrow(parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_drv_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}
plot(st_geometry(park_iso10))

for(i in 3:nrow(parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_drv_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}
plot(st_geometry(park_iso15))

# intersect big conglomerated polygons + residential areas
park_intersect_5 <- st_intersection(park_iso5, residential_sf)
park_intersect_10 <- st_intersection(park_iso10, residential_sf)
park_intersect_15 <- st_intersection(park_iso15, residential_sf)

write_rds(park_iso5, "./data/working/traveltime_isochrones/park_intersect_drv_5.Rds")
write_rds(park_iso10, "./data/working/traveltime_isochrones/park_intersect_drv_10.Rds")
write_rds(park_iso15, "./data/working/traveltime_isochrones/park_intersect_drv_15.Rds")

# get coverage based on intersections and the number of total residences from corelogic
park_coverage_5 <- (nrow(park_intersect_5)/nrow(residential_sf)*100)
park_coverage_10 <- (nrow(park_intersect_10)/nrow(residential_sf)*100)
park_coverage_15 <- (nrow(park_intersect_15)/nrow(residential_sf)*100)

# make a nice table
table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
table$Coverage <- c(park_coverage_5, park_coverage_10, park_coverage_15)
colnames(table) <- c("Time", "Coverage")
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_drv_table.csv", row.names = FALSE)
table

residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
drv_map = m1 + m2 + m3 + residential
drv_map

# Traveltime Point Intersections - Public Transport ------------------------------------------
# make initial unions of 1 and 2 so that loop is easier
park_iso_5_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_pt_5_1.RDS")
park_iso_5_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_pt_5_2.RDS")
park_iso5 <- st_union(park_iso_5_1,park_iso_5_2)

park_iso_10_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_pt_10_1.RDS")
park_iso_10_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_pt_10_2.RDS")
park_iso10 <- st_union(park_iso_10_1,park_iso_10_2)

park_iso_15_1 <- readRDS("./data/working/traveltime_isochrones/park_iso_pt_15_1.RDS")
park_iso_15_2 <- readRDS("./data/working/traveltime_isochrones/park_iso_pt_15_2.RDS")
park_iso15 <- st_union(park_iso_15_1,park_iso_15_2)

# loop through isochrones and continiously union with last
for(i in 3:nrow(parks)){
  park_iso5_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_5_",i,".RDS"))
  park_iso5 <- st_union(park_iso5,park_iso5_i)
}
plot(st_geometry(park_iso5))


for(i in 3:nrow(parks)){
  park_iso10_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_10_",i,".RDS"))
  park_iso10 <- st_union(park_iso10,park_iso10_i)
}
plot(st_geometry(park_iso10))

for(i in 3:nrow(parks)){
  park_iso15_i <- readRDS(paste0("./data/working/traveltime_isochrones/park_iso_pt_15_",i,".RDS"))
  park_iso15 <- st_union(park_iso15,park_iso15_i)
}
plot(st_geometry(park_iso15))

write_rds(park_iso5, "./data/working/traveltime_isochrones/park_intersect_pt_5.Rds")
write_rds(park_iso10, "./data/working/traveltime_isochrones/park_intersect_pt_10.Rds")
write_rds(park_iso15, "./data/working/traveltime_isochrones/park_intersect_wpt_15.Rds")

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
write.csv(table, file = "./data/working/traveltime_isochrones/park_iso_pt_table.csv", row.names = FALSE)

residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
m1 = mapview(park_iso5, layer.name = "5 minute isochrone", col.regions = colors[1])
m2 = mapview(park_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
m3 = mapview(park_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
# add the second layer on top
pt_map = m1 + m2 + m3 + residential
pt_map
table

# OSRM Intersections -------------------------------------------------
# # make initial unions of 1 and 2 so that loop is easier
# park_iso_1 <- readRDS("./data/working/osrm_isochrones/park_iso_1.RDS")
# park_iso_2 <- readRDS("./data/working/osrm_isochrones/park_iso_2.RDS")
# osrm_park_iso5 <- st_union(park_iso_1[1,],park_iso_2[1,])
# osrm_park_iso10 <- st_union(park_iso_1[2,],park_iso_2[3,])
# osrm_park_iso15 <- st_union(park_iso_1[3,],park_iso_2[3,])
# 
# # loop through isochrones and continiously union with last
# for(i in 3:nrow(parks)){
#   park_iso_i <- readRDS(paste0("./data/working/osrm_isochrones/park_iso_",i,".RDS"))
#   osrm_park_iso5 <- st_union(osrm_park_iso5,park_iso_i[1,])
#   osrm_park_iso10 <- st_union(osrm_park_iso10,park_iso_i[2,])
#   osrm_park_iso15 <- st_union(osrm_park_iso15,park_iso_i[3,])
# }
# 
# plot(st_geometry(osrm_park_iso5))
# plot(st_geometry(osrm_park_iso10))
# plot(st_geometry(osrm_park_iso15))
# 
# osrm_park_iso10 <- st_union(osrm_park_iso10, osrm_park_iso5)
# osrm_park_iso15 <- st_union(osrm_park_iso15, osrm_park_iso10)
# 
# # intersect big conglomerated polygons + residential areas
# osrm_park_intersect_5 <- st_intersection(osrm_park_iso5, residential_sf)
# osrm_park_intersect_10 <- st_intersection(osrm_park_iso10, residential_sf)
# osrm_park_intersect_15 <- st_intersection(osrm_park_iso15, residential_sf)
# 
# # get coverage based on intersections and the number of total residences from corelogic
# osrm_park_coverage_5 <- (nrow(osrm_park_intersect_5)/nrow(residential_sf)*100)
# osrm_park_coverage_10 <- (nrow(osrm_park_intersect_10)/nrow(residential_sf)*100)
# osrm_park_coverage_15 <- (nrow(osrm_park_intersect_15)/nrow(residential_sf)*100)
# 
# # make a nice table
# osrm_table <- as.data.frame(c("5 Minutes", "10 Minutes", "15 Minutes"))
# osrm_table$Coverage <- c(osrm_park_coverage_5, osrm_park_coverage_10, osrm_park_coverage_15)
# colnames(osrm_table) <- c("Time", "Coverage")
# write.csv(osrm_table, file = "./data/working/osrm_isochrones/osrm_park_iso_table.csv", row.names = FALSE)


#
# Circle Intersections -----------------------------------------
#

circles <- readRDS("./data/working/circle_poly.Rds")
circle_park_iso <- st_union(circles[1,], circles[2,])

for(i in 3:nrow(circles)){
  circle_park_iso <- st_union(circle_park_iso,circles[i,])
}
write_rds(circle_park_iso, "./data/working/circle_park_isochrone.Rds")
plot(st_geometry(circle_park_iso))

# intersect big conglomerated polygons + residential areas
circle_park_intersect <- st_intersection(circle_park_iso, residential_sf)

# get coverage based on intersections and the number of total residences from corelogic
circle_park_coverage <- (nrow(circle_park_intersect)/nrow(residential_sf)*100)

# make a nice table
circle_table <- as.data.frame(c("1/2 Mile Circle"))
circle_table$Coverage <- c(circle_park_coverage)
colnames(circle_table) <- c("Distance", "Coverage")
write.csv(circle_table, file = "./data/working/circle_park_iso_table.csv", row.names = FALSE)
