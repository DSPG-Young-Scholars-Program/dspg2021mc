install.packages("tidycensus")
install.packages("tidyverse")
install.packages("ggthemes")

library(tidycensus)
library(tidyverse)

# API Key
census_api_key("8bd374343abe2525e123fc5d2a97b262fc173fa6", install = TRUE)

# Installed API Key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# selected housing characteristics
# DP04
# gross rent where 30 - 34.9 % of income spent on rent
# DP04_158
# 35 % or more of income spent on rent
# DP04_159

# poverty status in the past 12 mo.
# S1701
# all indiviudals below 200 % of poverty line
# S1701_49

# commuting characteristics by sex   
# S0801
# mean travel time to work
# S0801_50

# income in the past 12 mo. (in 2019 inflation-adjusted dollars)
# S1901
# median income (dollars)
# S1901_12

# Select variables
acsvars <- c("DP04_158", "DP04_159", "DP04_130", "S1701_49", "S0801_50", "S1901_12")

# Get Data
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


