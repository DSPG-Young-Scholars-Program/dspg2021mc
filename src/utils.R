# convert lat/long to census block groups and tracts
geo2fips <- function(latitude, longitude) {
  url <- "https://geo.fcc.gov/api/census/area?lat=%f&lon=%f&format=json"
  block <- jsonlite::fromJSON(sprintf(url, latitude, longitude))[["results"]][["block_fips"]]
  
  # get block group
  block_grp <- substr(block, 1, 12)
  block_grp <- ifelse(length(unique(block_grp)) == 1, unique(block_grp), block_grp[1])
  
  # get tract
  tract <- substr(block, 1, 11)
  tract <- ifelse(length(unique(tract)) == 1, unique(tract), tract[1])
  
  c(block_grp, tract)
}