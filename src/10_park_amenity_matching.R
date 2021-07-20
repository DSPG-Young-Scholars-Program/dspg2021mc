library(stringr)

arlington_park_amenities <- read.csv("~/git/arlington_park_amenities.csv")

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
                                             amenity_ind_df)
                                       )

colnames(arlington_park_amenities)[1] <- "park_name"

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
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "high view park" , 
                                                  "halls hill/high view park")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "lee center" , 
                                                  "lee community center")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "thomas jefferson community and fitness center" , 
                                                  "thomas jefferson community center")
arlington_park_amenities$park_name <- str_replace(arlington_park_amenities$park_name, 
                                                  "walter reed community center and park" , 
                                                  "walter reed community center")




parks <- st_read("./data/original/arlington_parks/Park_Polygons.shp")
parks = parks %>%
  st_as_sf(coords = c("long","lat")) %>%
  st_transform("+proj=longlat +datum=WGS84")
parks = parks %>% filter(Ownership == "Arlington County Park") # 148

parks <- parks[order(parks$ParkName),]

parks$ParkName <- str_replace_all(parks$ParkName, "North", "N")
parks$ParkName <- str_replace_all(parks$ParkName, "South", "S")
parks$ParkName <- str_replace_all(parks$ParkName, "Street", "St")
parks$ParkName <- tolower(parks$ParkName)

parks$ParkName <- str_replace(parks$ParkName,
                              "isaac crossman park at four mile run",
                              "isaac crossman park")
parks$ParkName <- str_replace(parks$ParkName,
                              "nauck garden",
                              "nauck park")
parks$ParkName <- str_replace(parks$ParkName,
                              "oakland st park",
                              "oakland park")
parks$ParkName <- str_replace(parks$ParkName,
                              "wakefield high school park",
                              "wakefield stadium")
parks$ParkName <- str_replace(parks$ParkName,
                              "woodmont center",
                              "woodmont park")
parks$ParkName <- str_replace(parks$ParkName,
                              "zitkala-ša",
                              "zitkala-ša park")


match <- parks$ParkName[(parks$ParkName %in% arlington_park_amenities$park_name)]
no_match <- parks$ParkName[!(parks$ParkName %in% arlington_park_amenities$park_name)]

temp <- parks %>%
  left_join(arlington_park_amenities, by = c("ParkName" = "park_name"))


