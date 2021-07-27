library(dplyr)
library(stringr)
library(sf)

# read in .csv with columns filled
# with names of parks with each amenity
arlington_park_amenities <- read.csv("./data/original/arlington_park_amenities.csv")

# function to convert column of parks to 
# numeric vector indicating whether
# each park has that amenity
create_amenity_ind <- function(amenity, all) {
  ind <- which(all %in% amenity)
  amenity_ind <- amenity
  amenity_ind[ind] <- 1
  amenity_ind[-ind] <- 0
  as.numeric(amenity_ind)
}

amenity_ind_df <- apply(arlington_park_amenities[,2:ncol(arlington_park_amenities)], 
                        2, 
                        create_amenity_ind, 
                        all = arlington_park_amenities$all)

arlington_park_amenities <- data.frame(cbind(arlington_park_amenities$all,
                                             amenity_ind_df))
colnames(arlington_park_amenities)[1] <- "ParkName"
arlington_park_amenities$park_name <- arlington_park_amenities$ParkName

# do some manual fixes to match the park polygon park names #
arlington_park_amenities$park_name <- str_replace_all(arlington_park_amenities$park_name, "&", "and")
arlington_park_amenities$park_name <- tolower(arlington_park_amenities$park_name)
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "carlin hall" , 
                                                  "carlin hall community center")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "dawson terrace" , 
                                                  "dawson terrace community center")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "doctor's run park" , 
                                                  "doctors run park")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "fairlington community center and park" , 
                                                  "fairlington community center")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "gulf branch nature center and park" , 
                                                  "gulf branch nature center")
#arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
#                                                  "high view park" , 
#                                                  "halls hill/high view park")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "lee center" , 
                                                  "lee community center")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "thomas jefferson community and fitness center" , 
                                                  "thomas jefferson community center")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "walter reed community center and park" , 
                                                  "walter reed community center")

# read in park polygon data
parks <- st_read("./data/original/arlington_parks/Park_Polygons.shp")

parks <- parks %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")
parks <- parks %>% filter(Ownership == "Arlington County Park") # 148

parks_copy <- parks

parks <- parks[order(parks$ParkName),]

parks$park_name <- parks$ParkName

# do some manual fixes to match the park amenities park names #
parks$park_name <- str_replace_all(parks$park_name, "North", "N")
parks$park_name <- str_replace_all(parks$park_name, "South", "S")
parks$park_name <- str_replace_all(parks$park_name, "Street", "St")
parks$park_name <- tolower(parks$park_name)

parks$park_name <- str_replace(parks$park_name,
                              "isaac crossman park at four mile run",
                              "isaac crossman park")
parks$park_name <- str_replace(parks$park_name,
                              "nauck garden",
                              "nauck park")
parks$park_name <- str_replace(parks$park_name,
                              "oakland st park",
                              "oakland park")
parks$park_name <- str_replace(parks$park_name,
                              "wakefield high school park",
                              "wakefield stadium")
parks$park_name <- str_replace(parks$park_name,
                              "woodmont center",
                              "woodmont park")
parks$park_name <- str_replace(parks$park_name,
                              "zitkala-ša",
                              "zitkala-ša park")

# join park polygons and amenities
parks_amenities <- parks %>%
  left_join(arlington_park_amenities, by = "park_name") %>%
  slice(order(factor(ParkName.x, levels = parks_copy$ParkName))) %>%
  st_drop_geometry()

write.csv(parks_amenities, "./data/working/parks_amenities.csv", row.names = FALSE)


