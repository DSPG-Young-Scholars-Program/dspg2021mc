library(data.table)
library(tidyverse)
library(leaflet)
library(sf)
library(osmdata)
library(mapview)
library(RColorBrewer)
library(readr)

acs_tract <- read_rds("./data/working/acs_tract.Rds")
parks <- st_read("./data/original/arlington_parks/Park_Polygons.shp")

#
# Leaflet Maps ---------------------------------------
#

acs_tract <- acs_tract %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")

parks = parks %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")

bb <- getbb('arlington county, virginia')


#
# Plots --------------
#
pal <- colorNumeric("PuOr", acs_tract$black)
plot_leaf_black <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_black, file = "./output/leaflet/plot_black.png")

#
# Crosstabs ------------- 
#
# education black
pal <- colorNumeric("PuOr", acs_tract$ba_higher_b)
plot_leaf_ba_higher_b <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_ba_higher_b, file = "./output/leaflet/plot_ba_higher_b.png")

# education asian
pal <- colorNumeric("PuOr", acs_tract$ba_higher_a)
plot_leaf_ba_higher_a <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_ba_higher_a, file = "./output/leaflet/plot_ba_higher_a.png")

# education hispanic
pal <- colorNumeric("PuOr", acs_tract$ba_higher_h)
plot_leaf_ba_higher_h <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_ba_higher_h, file = "./output/leaflet/plot_ba_higher_h.png")


# unemployment black
pal <- colorNumeric("PuOr", acs_tract$unemploy_rate_b)
plot_leaf_unemploy_rate_b <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_unemploy_rate_b, file = "./output/leaflet/plot_unemploy_rate_b.png")

# unemployment asian
pal <- colorNumeric("PuOr", acs_tract$unemploy_rate_a)
plot_leaf_unemploy_rate_a <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_unemploy_rate_a, file = "./output/leaflet/plot_unemploy_rate_a.png")

# unemployment hispanic
pal <- colorNumeric("PuOr", acs_tract$unemploy_rate_h)
plot_leaf_unemploy_rate_h <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_unemploy_rate_h, file = "./output/leaflet/plot_unemploy_rate_h.png")

# income black
pal <- colorNumeric("PuOr", acs_tract$med_inc_b)
plot_leaf_med_inc_b <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_med_inc_b, file = "./output/leaflet/plot_med_inc_b.png")

# income asian
pal <- colorNumeric("PuOr", acs_tract$med_inc_a)
plot_leaf_med_inc_a <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_med_inc_a, file = "./output/leaflet/plot_med_inc_a.png")

# income hispanic
pal <- colorNumeric("PuOr", acs_tract$med_inc_h)
plot_leaf_med_inc_h <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_med_inc_h, file = "./output/leaflet/plot_med_inc_h.png")

# poverty black
pal <- colorNumeric("PuOr", acs_tract$pov_b)
plot_leaf_pov_b <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_pov_b, file = "./output/leaflet/plot_pov_b.png")

# poverty asian
pal <- colorNumeric("PuOr", acs_tract$pov_a)
plot_leaf_pov_a <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_pov_a, file = "./output/leaflet/plot_pov_a.png")

# poverty hispanic
pal <- colorNumeric("PuOr", acs_tract$pov_h)
plot_leaf_pov_h <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addPolygons(data = parks, color = "green") %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)
mapshot(plot_leaf_pov_h, file = "./output/leaflet/plot_pov_h.png")
