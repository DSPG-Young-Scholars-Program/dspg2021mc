library(data.table)
library(tidyverse)
library(leaflet)
library(sf)
library(osmdata)
library(RColorBrewer)

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
  st_transform("+proj=longlat +datum=WGS84") %>%
  st_sf() %>%
  st_cast()
for(i in seq(nrow(parks))) {
  names(parks$geometry[i][[1]]) = NULL 
}

bb <- getbb('arlington county, virginia')

pal <- colorNumeric("PuOr", acs_tract$black)

acs_plot <- leaflet() %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addPolygons(data = acs_tract, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("PuOr", black)(black),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))  %>%
  addMarkers(data = parks) %>%
  addLegend(data = acs_tract, pal = pal, values = ~black)

park_plot <- leaflet(data = parks) %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
  addMarkers()

plot(st_geometry(parks))

#call plot
acs_plot
park_plot


