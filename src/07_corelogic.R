library(RPostgreSQL)
library(dplyr)
library(readr)
library(sf)

source("utils.R")

#
# Get Arlington County data from the available tables-----------------------------------------------------------
#

conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv(x = "db_userid"), 
                  password = Sys.getenv(x = "db_pwd"))

arlington <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_2_51 WHERE fips_code = '51013'")
arlington2 <- dbGetQuery(conn, "SELECT * FROM corelogic_usda.current_tax_200627_latest_all_add_vars_add_progs_geom_blk WHERE geoid_cnty = '51013'")

dbDisconnect(conn)
rm(conn)

#
# Check contents ---------------------------------------------------------------------------------------------
#

names(arlington)

# Year
table(arlington$tax_year, useNA = "always")          # 2019
table(arlington$assessed_year, useNA = "always")     # 2019


# Geo identifiers
# empty: block_level_latitude, block_level_longitude, 
# usable: parcel_level_latitude, parcel_level_longitude, census_tract

# Other information not NA:
# land_use_code, county_use_description, property_indicator_code
# owner_1_full_name, owner_1_last_name, owner_1_corporate_indicator, 
# total_value_calculated


#
# Check ---------------------------------------------------------------------------------------------
#

table(arlington$land_use_code, useNA = "always")
#133   163   200   211   500   600   601   602   604   614   650   675 NA
#28 16706   128   289  3310     9   490    75   107    16    28   230   0

table(arlington$owner_1_corporate_indicator, useNA = "always")
# Y     <NA> 
# 2521 18895 

table(arlington$county_use_description, useNA = "always")
# 1 FAMILY DETACHED AFFORDABLE DWELLING UNIT        APARTMENT PARKING              AUTO DEALER                     BANK 
# 27599                       56                        9                       16                       24 
# CO-OP               COMMERCIAL         COMMERCIAL CONDO       COMMERCIAL PARKING        COMMERCIAL RETAIL 
# 1784                      350                      276                      108                      276 
# COMMUNITY BENEFIT UNIT                CONDO HOA             CONDO MASTER                   DUPLEX                FAST FOOD 
# 50                       68                      159                      280                       16 
# GARDEN APARTMENT             GARDEN CONDO              HEALTH CARE        HI-RISE APARTMENT            HI-RISE CONDO 
# 400                     8006                       20                      148                    11049 
# HOA SITE PLAN                    HOTEL   HOTEL RESIDENCE SUITES       MID-RISE APARTMENT           MID-RISE CONDO 
# 292                       62                        9                      114                     3162 
# MIX OFFICE/COMML                MIXED USE         NEIGHBORHOOD CTR      OFFICE BLDG 7+ STRY OFFICE BLDG UNDER 7 STRY 
# 53                       26                       24                      180                       24 
# OFFICE PARKING           PUBLIC SERVICE              RESIDENTIAL               RESTAURANT             SELF-STORAGE 
# 5                       33                     1987                       80                        8 
# SERVICE STATION       SINGLE FAM ZON APT     SINGLE FAM ZON COMML             SMALL OFFICE            STACKED CONDO 
# 64                       65                       12                       50                     2894 
# TOWNHOUSE          TOWNHOUSE CONDO        VACANT COMMERCIAL      VACANT MULTI-FAMILY            VACANT OFFICE 
# 3715                      595                      635                      126                       66 
# VACANT SFR/TWNHS                WAREHOUSE                     <NA> 
#   1309                       51                       12 


#
# Clean residential & plot ---------------------------------------------------------------------------------------------
#

sum(is.na(arlington$parcel_level_longitude)) # 1706
sum(is.na(arlington$parcel_level_latitude)) # 1706

residential <- arlington %>% filter(!is.na(parcel_level_longitude) &
                                    !is.na(parcel_level_latitude))

residential <- residential %>% filter(county_use_description == "1 FAMILY DETACHED" |
                                        county_use_description == "AFFORDABLE DWELLING UNIT" |
                                        county_use_description == "CONDO HOA" |
                                        county_use_description == "COMMEMRCIAL CONDO" |
                                        county_use_description == "CONDO MASTER" |
                                        county_use_description == "DUPLEX" |
                                        county_use_description == "GARDEN APARTMENT" |
                                        county_use_description == "GARDEN CONDO" |
                                        county_use_description == "HI-RISE APARTMENT"|
                                        county_use_description == "HI-RISE CONDO" |
                                        county_use_description == "MID-RISE CONDO" |
                                        county_use_description == "MIS-RISE APARTMENT" |
                                        county_use_description == "RESIDENTIAL" |
                                        county_use_description == "SINGLE FAM ZON APT" |
                                        county_use_description == "SINGLE FAM ZON COMML" |
                                        county_use_description == "STACKED CONDO" |
                                        county_use_description == "TOWNHOUSE" |
                                        county_use_description == "TOWNHOUSE CONDO")

residential$greenspace <- ifelse(residential$county_use_description == "1 FAMILY DETACHED" |
                                   residential$county_use_description == "TOWNHOUSE" |
                                   residential$county_use_description == "TOWNHOUSE CONDO",
                                 1,
                                 0)

residential$parcel_level_latitude <- as.numeric(residential$parcel_level_latitude)
residential$parcel_level_longitude <- as.numeric(residential$parcel_level_longitude)

residential_geoids <- apply(residential, 
                            1, 
                            function(x) geo2fips(as.numeric(x[39]), 
                                                 as.numeric(x[40])))
residential$bgrp_geoid <- residential_geoids[1,]
residential$tract_geoid <- residential_geoids[2,]

# Test plot
residential_sf <- st_as_sf(residential, coords = c("parcel_level_longitude", "parcel_level_latitude"))
plot(st_geometry(residential_sf), pch = 21, cex = 0.3)


#
# Write out ---------------------------------------------------------------------------------------------
#

write.csv(residential, "./data/working/corelogic/residential.csv")

# Basic summary statistics
 
bgrp_greenspace <- residential %>%
  group_by(bgrp_geoid) %>%
  summarise(mean_greenspace = mean(greenspace)) %>%
  as.data.frame()

write.csv(bgrp_greenspace, "./data/working/bgrp_greenspace.csv", row.names = FALSE)

tract_greenspace <- residential %>%
  group_by(tract_geoid) %>%
  summarise(mean_greenspace = mean(greenspace)) %>%
  as.data.frame()

write.csv(tract_greenspace, "./data/working/tract_greenspace.csv", row.names = FALSE)


