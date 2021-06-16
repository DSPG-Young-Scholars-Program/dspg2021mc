# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggthemes")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
library(tidycensus)
library(tidyverse)

######## Pull ACS 2014/18 data for basic Patrick County sociodemographics #################


#
# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#
# Select variables ------------------------------------------------------------------------
#

# Load all variable names
# load_variables(2018, "acs5", cache = TRUE)

# total pop
# B01003_001
# % population age 65+
# B01001_020:25 (male), B01001_044:49 (female) / B01001_001    
# % population age <=18
# B01001_003:006 (male), B01001_027:30 (female) / B01001_001
# % population Hispanic or Latino
# B03001_003 / B03001_001
# % population Black
# B02001_003 / B02001_001
# % population over age 25 without a BA degree
# B15003_002:021 / B15003_001
# % population in labor force that is unemployed
# B23025_005 / B23025_002
# % population living under 100% poverty line
# B17001_002 / B17001_001
# % population without health insurance
# (005 + 008 + 011 + 014 + 017 + 020 + 023 + 026 + 029 + 033 + 036 + 039 + 042 + 045 + 048 + 051 + 054 + 057) /  B27001_001

# Select variables
acsvars <- c(
  # total pop
  "B01003_001",
  # age 65 +
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
  "B01001_001",
  # age <18
  "B01001_003", "B01001_004", "B01001_005", "B01001_006",
  "B01001_027", "B01001_028", "B01001_029", "B01001_030",
  # Hispanic
  "B03001_003", "B03001_001",
  # Black
  "B02001_003", "B02001_001",
  # Without BA
  "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007",
  "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013",
  "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019",
  "B15003_020", "B15003_021", "B15003_001",
  # Unemployed
  "B23025_005", "B23025_002",
  # In poverty
  "B17001_002", "B17001_001",
  # Without health insurance
  "B27001_005", "B27001_008", "B27001_011", "B27001_014", "B27001_017", "B27001_020", "B27001_023",
  "B27001_026", "B27001_029", "B27001_033", "B27001_036", "B27001_039", "B27001_042", "B27001_045",
  "B27001_048", "B27001_051", "B27001_054", "B27001_057", "B27001_001"
 )


#
# Get data ------------------------------------------------------------------------
#

# Get data from 2015/19 5-year estimates for Arlington County (51013) at tract level 
data_tract <- get_acs(geography = "tract", state = 51, county = 013,
                variables = acsvars,
                year = 2019, survey = "acs5",
                cache_table = TRUE, output = "wide", geometry = TRUE,
                keep_geo_vars = TRUE)

# Get data from 2015/19 5-year estimates for Arlington County (51013) at block group level
data_bgrp <- get_acs(geography = "block group", state = 51, county = 013,
                     variables = acsvars,
                     year = 2019, survey = "acs5",
                     cache_table = TRUE, output = "wide", geometry = TRUE,
                     keep_geo_vars = TRUE)

#
# Calculate ------------------------------------------------------------------------
#

# Tract level 
acs_tract <- data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  totalpop_trct = B01003_001E,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
           B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
             B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B03001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
          B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
          B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                 B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                 B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100
  )

# Block group (note: variables with estimate = 0 will have NAs in the final calculation. Disregard these
# for now and use tract-level values for plotting.)
acs_bgrp <- data_bgrp %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  BLKGRPCE = BLKGRPCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  totalpop_bgrp = B01003_001E,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B03001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100
)
