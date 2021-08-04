# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggthemes")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
library(tidycensus)
library(tidyverse)
library(ggthemes)

######## Pull ACS 2015/19 data for basic Arlington County sociodemographics #################


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
# % population with no vehicle available
# B08201_002 / B08201_001
# % population with 1 or more vehicle available
# B08201_003:006 / B08201_001
# % population with own children under 18 years
# B23007_002 / B23007_001 
# % population with no children under 18 years
# B23007_031 / B23007_001 

# Unemployment by race
# S2301_C04_001, S2301_C04_012, S2301_C04_013, S2301_C04_015, S2301_C04_019
# Educational attainment
# S1501_C02_015, S1501_C02_030, S1501_C02_036, S1501_C02_042, S1501_C02_054
# Disconnected youth
# S1401_C02_010

# Select variables
acsvars <- c(
  # total pop
  "B01003_001",
  # Hispanic
  "B03001_003", "B03001_001",
  # White
  "B02001_002",
  # Black
  "B02001_003", "B02001_001",
  # Asian
  "B02001_005",
  # Other
  "B02001_004", "B02001_006", "B02001_007",
  "B02001_008", "B02001_009", "B02001_010",
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
  "B27001_048", "B27001_051", "B27001_054", "B27001_057", "B27001_001",
  # median income at household level - all
  "B19013_001",
  # median income at household level - white
  "B19013A_001",
  # median income at household level - asian
  "B19013D_001",
  # median income at household level - black
  "B19013B_001",
  # median income at household level - hispanic
  "B19013I_001",
  # median income at household level - two or more races
  "B19013G_001",
  # commuting hours
  "B08134_001", "B08134_002", "B08134_003", "B08134_004", "B08134_005", "B08134_006", "B08134_007", "B08134_008", "B08134_009", "B08134_010",
  # gross rent as percentage of income
  "B25070_001", "B25070_007", "B25070_008", "B25070_009", "B25070_010",
  # poverty status - all
  "B17001_001", "B17001_002",
  # poverty status - white
  "B17020A_001", "B17020A_002",
  # poverty status - asian
  "B17020D_001", "B17020D_002",
  # poverty status - black
  "B17020B_001", "B17020B_002",
  # poverty status - hispanic
  "B17020I_001", "B17020I_002",
  # poverty status - two or more races
  "B17020G_001", "B17020G_002",
  # under 18 population
  "B09001_001",
  # no vehicle available
  "B08201_002", "B08201_001",
  # 1 or more vehicles 
  "B08201_003", "B08201_004", "B08201_005", "B08201_006",
  # children under 18 
  "B23007_002", "B23007_001", 
  # no children under 18
  "B23007_031"
)

acs_subject_vars <- c(
  # Unemployment rate - all
  "S2301_C04_001",
  # Unemployment rate - White
  "S2301_C04_012",
  # Unemployment rate - Black
  "S2301_C04_013",
  # Unemployment rate - Asian
  "S2301_C04_015",
  # Unemployment rate - Hispanic
  "S2301_C04_019",
  # 25 and older with BA or higher - all
  "S1501_C02_015",
  # 25 and older with BA or higher - White
  "S1501_C02_030",
  # 25 and older with BA or higher - Black
  "S1501_C02_036",
  # 25 and older with BA or higher - Asian
  "S1501_C02_042",
  # 25 and older with BA or higher - Hispanic
  "S1501_C02_054",
  # % pop enrolled in college or graduate school
  "S1401_C02_010",
  # Ambulatory Disability rate
  "S1810_C02_051",
  # no cars
  "S2504_C02_033"
)


#
# Get data ------------------------------------------------------------------------
#

# Get data from 2015/19 5-year estimates for Arlington County (51013) at tract level 
data_tract <- get_acs(geography = "tract", 
                      state = 51, 
                      county = 013,
                      variables = acsvars,
                      year = 2019, 
                      survey = "acs5",
                      cache_table = TRUE, 
                      output = "wide", 
                      geometry = TRUE,
                      keep_geo_vars = TRUE)

# Get subject table data from 2015/19 5-year estimates for Arlington County (51013) at tract level 
data_tract_subject <- get_acs(geography = "tract", 
                              state = 51, 
                              county = 013,
                              variables = acs_subject_vars,
                              year = 2019, 
                              survey = "acs5",
                              cache_table = TRUE, 
                              output = "wide")

data_tract_subject <- data_tract_subject %>% select(-NAME)

# join with other acs variables
data_tract <- data_tract %>% left_join(data_tract_subject, by = "GEOID")

# Get data from 2015/19 5-year estimates for Arlington County (51013) at block group level
data_bgrp <- get_acs(geography = "block group", 
                     state = 51, 
                     county = 013,
                     variables = acsvars,
                     year = 2019, 
                     survey = "acs5",
                     cache_table = TRUE, 
                     output = "wide", 
                     geometry = TRUE,
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
  hispanic = B03001_003E / B03001_001E * 100,
  white = B02001_002E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  asian = B02001_005E / B02001_001E * 100,
  other_race = (B02001_004E + B02001_006E + B02001_007E + B02001_008E + B02001_009E + B02001_010E) / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100,
  med_inc_all = B19013_001E,
  med_inc_w = B19013A_001E,
  med_inc_b = B19013B_001E,
  med_inc_a = B19013D_001E,
  med_inc_h = B19013I_001E,
  med_inc_o = B19013G_001E,
  pov_all = B17001_002E / B17001_001E * 100,
  pov_w = B17020A_002E / B17020A_001E * 100,
  pov_b = B17020B_002E / B17020B_001E * 100,
  pov_a = B17020D_002E / B17020D_001E * 100,
  pov_h = B17020I_002E / B17020I_001E * 100,
  pov_o = B17020G_002E / B17020G_001E * 100,
  rent_ov_30 = (B25070_007E + B25070_008E + B25070_009E + B25070_010E) / B25070_001E * 100,
  commute_un_10 = B08134_002E / B08134_001E * 100,
  commute_10_14 = B08134_003E / B08134_001E * 100,
  commute_15_19 = B08134_004E / B08134_001E * 100,
  commute_20_24 = B08134_005E / B08134_001E * 100,
  commute_25_29 = B08134_006E / B08134_001E * 100,
  commute_30_34 = B08134_007E / B08134_001E * 100,
  commute_35_44 = B08134_008E / B08134_001E * 100,
  commute_45_59 = B08134_009E / B08134_001E * 100,
  commute_60_pl = B08134_010E / B08134_001E * 100,
  unemploy_rate_all = S2301_C04_001E,
  unemploy_rate_w = S2301_C04_012E,
  unemploy_rate_b = S2301_C04_013E,
  unemploy_rate_a = S2301_C04_015E,
  unemploy_rate_h = S2301_C04_019E,
  ba_higher_all = S1501_C02_015E,
  ba_higher_w = S1501_C02_030E,
  ba_higher_b = S1501_C02_036E,
  ba_higher_a = S1501_C02_042E,
  ba_higher_h = S1501_C02_054E,
  perc_college_higher = S1401_C02_010E,
  perc_under_18 = B09001_001E/B01003_001E*100,
  perc_ambulatory_disability = S1810_C02_051E/B01003_001E*100,
  #no_vehic = S2504_C02_033E,
  no_vehic = B08201_002E/B08201_001E * 100,
  one_or_more_vehic = (B08201_003E + B08201_004E + B08201_005E + B08201_006E)/B08201_001E * 100,
  children_under_18 = B23007_002E/B23007_001E * 100,
  no_children_under_18 = B23007_031E/B23007_001E * 100
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
  hispanic = B03001_003E / B03001_001E * 100,
  white = B02001_002E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  asian = B02001_005E / B02001_001E * 100,
  other_race = (B02001_004E + B02001_006E + B02001_007E + B02001_008E + B02001_009E + B02001_010E) / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100,
  med_inc_w = B19013A_001E,
  med_inc_b = B19013B_001E,
  med_inc_a = B19013D_001E,
  med_inc_h = B19013I_001E,
  med_inc_o = B19013G_001E,
  pov_w = B17020A_002E / B17020A_001E * 100,
  pov_b = B17020B_002E / B17020B_001E * 100,
  pov_a = B17020D_002E / B17020D_001E * 100,
  pov_h = B17020I_002E / B17020I_001E * 100,
  pov_o = B17020G_002E / B17020G_001E * 100,
  rent_ov_30 = (B25070_007E + B25070_008E + B25070_009E + B25070_010E) / B25070_001E * 100,
  commute_un_10 = B08134_002E / B08134_001E * 100,
  commute_10_14 = B08134_003E / B08134_001E * 100,
  commute_15_19 = B08134_004E / B08134_001E * 100,
  commute_20_24 = B08134_005E / B08134_001E * 100,
  commute_25_29 = B08134_006E / B08134_001E * 100,
  commute_30_34 = B08134_007E / B08134_001E * 100,
  commute_35_44 = B08134_008E / B08134_001E * 100,
  commute_45_59 = B08134_009E / B08134_001E * 100,
  commute_60_pl = B08134_010E / B08134_001E * 100,
  perc_under_18 = B09001_001E/B01003_001E*100,
  no_vehic = B08201_002E/B08201_001E * 100,
  one_or_more_vehic = (B08201_003E + B08201_004E + B08201_005E + B08201_006E)/B08201_001E * 100,
  children_under_18 = B23007_002E/B23007_001E * 100,
  no_children_under_18 = B23007_031E/B23007_001E * 100
)

write_rds(acs_tract, "./data/working/acs_tract.Rds")
write_rds(acs_bgrp, "./data/working/acs_bgrp.Rds")


#
# Plots -------------------------------------------------
#

acs_tract <- read_rds("./data/working/acs_tract.Rds")
acs_bgrp <- read_rds("./data/working/acs_bgrp.Rds")

# ba_higher_all
min <- floor(min(acs_tract$ba_higher_all, na.rm = T))
max <- ceiling(max(acs_tract$ba_higher_all, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = ba_higher_all, geometry = geometry)) +
  labs(title = "Share of Population with a Bachelor's \n Age 25-64 by Census Tract, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) Estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min, max), 
                        breaks = seq(min, max, length.out = 5))
ggsave(path = "./output/acs", device = "png", filename = "plot_educ_attain.png", plot = last_plot())

# inpov
min <- floor(min(acs_tract$inpov, na.rm = T))
max <- ceiling(max(acs_tract$inpov, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = inpov, geometry = geometry)) +
  labs(title = "Percent Population Below 100 Percent \n Poverty Line by Census Tract, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) Estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min, max), 
                        breaks = seq(min, max, length.out = 5))
ggsave(path = "./output/acs", device = "png", filename = "plot_inpov.png", plot = last_plot())

# black
min <- floor(min(acs_bgrp$black, na.rm = T))
max <- ceiling(max(acs_bgrp$black, na.rm = T))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = black, geometry = geometry)) +
  labs(title = "Percent Population Black \nby Census Block Group, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) Estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min, max), 
                        breaks = seq(min, max, length.out = 5))
ggsave(path = "./output/acs", device = "png", filename = "plot_black.png", plot = last_plot())
