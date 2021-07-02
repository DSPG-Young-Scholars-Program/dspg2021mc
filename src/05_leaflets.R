library(data.table)
library(tidyverse)
library(leaflet)

#
# Leaflet Maps ---------------------------------------
#

# Example with Dialysis Data
# dialysis <- #read in the csv data
#   read_csv("./data/original/dhs-dialysis/kidney-dialysis.csv") %>%
#   #separate comma separted coordinates from one column to two columns
#   separate(CalcLocation, c("lat","long"), sep = ",") %>%
#   filter(State == "VA") %>%
#   #transform data frame into a sf file, did c("y", "x") because c("x","y") = Antartica
#   st_as_sf(coords = c("long","lat")) %>%
#   #labeling the projection to be same as the others
#   st_set_crs("+proj=longlat +datum=WGS84")
# 
# bb <- getbb('patrick county, virginia')
# 
# dialysis_plot <- leaflet(data = dialysis) %>% #create leaflet object
#   addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
#   addMarkers() %>%
#   fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>% #add bounding box
#   addMeasure()
# 
# #call dialysis plot
# dialysis_plot