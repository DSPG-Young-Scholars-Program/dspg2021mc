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

# Select variables
acsvars <- c(
  # total pop
  "B01003_001",
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
  "B27001_048", "B27001_051", "B27001_054", "B27001_057", "B27001_001",
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
  # poverty status - white
  "B17020A_001", "B17020A_002",
  # poverty status - asian
  "B17020D_001", "B17020D_002",
  # poverty status - black
  "B17020B_001", "B17020B_002",
  # poverty status - hispanic
  "B17020I_001", "B17020I_002",
  # poverty status - two or more races
  "B17020G_001", "B17020G_002"
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
                     year = 2020, survey = "acs5",
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
  hispanic = B03001_003E / B03001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
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
  commute_60_pl = B08134_010E / B08134_001E * 100
)

# Drop tract level 9801; reassign acs_tract
acs_tract <- acs_tract[-c(53), ]

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
  black = B02001_003E / B02001_001E * 100,
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
  commute_60_pl = B08134_010E / B08134_001E * 100
)


write_rds(acs_tract, "./data/working/acs_tract.Rds")
write_rds(acs_bgrp, "./data/working/acs_bgrp.Rds")

# Plot ----------------------------------------------------------------------

# Tract level
# hispanic
min_hispanic_trct <- floor(min(acs_tract$hispanic))
max_hispanic_trct <- ceiling(max(acs_tract$hispanic))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = hispanic)) +
  labs(title = "Percent population Hispanic \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_hispanic_trct, max_hispanic_trct),
                        breaks = seq(min_hispanic_trct, max_hispanic_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_hispanic_trct.png", plot = last_plot())

# black
min_black_trct <- floor(min(acs_tract$black))
max_black_trct <- ceiling(max(acs_tract$black))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = black)) +
  labs(title = "Percent population Black \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_black_trct, max_black_trct),
                        breaks = seq(min_black_trct, max_black_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_black_trct.png", plot = last_plot())

# without bachelors (noba)
min_noba_trct <- floor(min(acs_tract$noba))
max_noba_trct <- ceiling(max(acs_tract$noba))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = noba)) +
  labs(title = "Percent population without bachelor's degree \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_noba_trct, max_noba_trct),
                        breaks = seq(min_noba_trct, max_noba_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_noba_trct.png", plot = last_plot())

# unemployed
min_unempl_trct <- floor(min(acs_tract$unempl))
max_unempl_trct <- ceiling(max(acs_tract$unempl))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = unempl)) +
  labs(title = "Percent population unemployed \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_unempl_trct, max_unempl_trct),
                        breaks = seq(min_unempl_trct, max_unempl_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_unempl_trct.png", plot = last_plot())

# in poverty
min_inpov_trct <- floor(min(acs_tract$inpov))
max_inpov_trct <- ceiling(max(acs_tract$inpov))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = inpov)) +
  labs(title = "Percent population in poverty \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_inpov_trct, max_inpov_trct),
                        breaks = seq(min_inpov_trct, max_inpov_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_inpov_trct.png", plot = last_plot())

# no health insurance
min_nohealthins_trct <- floor(min(acs_tract$nohealthins))
max_nohealthins_trct <- ceiling(max(acs_tract$nohealthins))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = nohealthins)) +
  labs(title = "Percent population in without health insurance \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.3),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_nohealthins_trct, max_nohealthins_trct),
                        breaks = seq(min_nohealthins_trct, max_nohealthins_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nohealthins_trct.png", plot = last_plot())


# median income white
min_med_inc_w_trct <- floor(min(acs_tract$med_inc_w))
max_med_inc_w_trct <- ceiling(max(acs_tract$med_inc_w))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = med_inc_w)) +
  labs(title = "Median household income white \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.3),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Dollars", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_nohealthins_trct, max_nohealthins_trct),
                        breaks = seq(min_nohealthins_trct, max_nohealthins_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nohealthins_trct.png", plot = last_plot())


# poverty status white
min_pov_w_trct <- floor(min(acs_tract$pov_w))
max_pov_w_trct <- ceiling(max(acs_tract$pov_w))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_w)) +
  labs(title = "Percent population in poverty (white) \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_pov_w_trct, max_pov_w_trct),
                        breaks = seq(min_pov_w_trct, max_pov_w_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_w_trct.png", plot = last_plot())

# poverty status black
# poverty status black
# Drop tract level 9802; reassign acs_tract
acs_tract <- acs_tract[-c(54), ]
min_pov_b_trct <- floor(min(acs_tract$pov_b))
max_pov_b_trct <- ceiling(max(acs_tract$pov_b))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_b)) +
  labs(title = "Percent population in poverty (Black) \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_pov_b_trct, max_pov_b_trct),
                        breaks = seq(min_pov_b_trct, max_pov_b_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_b_trct.png", plot = last_plot())

# poverty status asian
# Drop tract level 1034.01; reassign acs_tract
acs_tract <- acs_tract[-c(47), ]
min_pov_a_trct <- floor(min(acs_tract$pov_a))
max_pov_a_trct <- ceiling(max(acs_tract$pov_a))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_a)) +
  labs(title = "Percent population in poverty (Asian) \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_pov_a_trct, max_pov_a_trct),
                        breaks = seq(min_pov_a_trct, max_pov_a_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_a_trct.png", plot = last_plot())

# poverty status hispanic
min_pov_h_trct <- floor(min(acs_tract$pov_h))
max_pov_h_trct <- ceiling(max(acs_tract$pov_h))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_h)) +
  labs(title = "Percent population in poverty (Hispanic) \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_pov_h_trct, max_pov_h_trct),
                        breaks = seq(min_pov_h_trct, max_pov_h_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_h_trct.png", plot = last_plot())

# poverty status one or more races
min_pov_o_trct <- floor(min(acs_tract$pov_o))
max_pov_o_trct <- ceiling(max(acs_tract$pov_o))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_o)) +
  labs(title = "Percent population in poverty (one or more \nraces) by Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_pov_o_trct, max_pov_o_trct),
                        breaks = seq(min_pov_o_trct, max_pov_o_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_o_trct.png", plot = last_plot())

# gross rent over 30 percent of income
min_rent_ov_30_trct <- floor(min(acs_tract$rent_ov_30))
max_rent_ov_30_trct <- ceiling(max(acs_tract$rent_ov_30))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = rent_ov_30)) +
  labs(title = "Percent population whose rent more than 30 percent \nof income by Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.3),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_rent_ov_30_trct, max_rent_ov_30_trct),
                        breaks = seq(min_rent_ov_30_trct, max_rent_ov_30_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_rent_ov_30_trct.png", plot = last_plot())

# commute under 10 mins
min_commute_un_10_trct <- floor(min(acs_tract$commute_un_10))
max_commute_un_10_trct <- ceiling(max(acs_tract$commute_un_10))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_un_10)) +
  labs(title = "Percent population with commute under 10 minutes \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_commute_un_10_trct, max_commute_un_10_trct),
                        breaks = seq(min_commute_un_10_trct, max_commute_un_10_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_un_10_trct.png", plot = last_plot())


# commute 10 to 14 mins
min_commute_10_14_trct <- floor(min(acs_tract$commute_10_14))
max_commute_10_14_trct <- ceiling(max(acs_tract$commute_10_14))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_10_14)) +
  labs(title = "Percent population with commute between 10 and 14 \nminutes by Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_commute_10_14_trct, max_commute_10_14_trct),
                        breaks = seq(min_commute_10_14_trct, max_commute_10_14_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_10_14_trct.png", plot = last_plot
       
# commute 15 to 19 mins 
min_commute_15_19_trct <- floor(min(acs_tract$commute_15_19))
max_commute_15_19_trct <- ceiling(max(acs_tract$commute_15_19))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_15_19)) +
  labs(title = "Percent population with commute between 15 and 19 minutes \nby Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_commute_15_19_trct, max_commute_15_19_trct),
                        breaks = seq(min_commute_15_19_trct, max_commute_15_19_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_15_19_trct.png", plot = last_plot
       
# commute 20 to 24 mins 
min_commute_20_24_trct <- floor(min(acs_tract$commute_20_24))
max_commute_20_24_trct <- ceiling(max(acs_tract$commute_20_24))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_20_24)) +
  labs(title = "Percent population with commute between 20 and 24 \nminutes by Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_commute_20_24_trct, max_commute_20_24_trct),
                        breaks = seq(min_commute_20_24_trct, max_commute_20_24_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_20_24_trct.png", plot = last_plot

# commute 25 to 29 mins 
min_commute_25_29_trct <- floor(min(acs_tract$commute_25_29))
max_commute_25_29_trct <- ceiling(max(acs_tract$commute_25_29))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_25_29)) +
  labs(title = "Percent population with commute between 25 and 29 \nminutes by Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_commute_25_29_trct, max_commute_25_29_trct),
                        breaks = seq(min_commute_25_29_trct, max_commute_25_29_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_25_29_trct.png", plot = last_plot
       
# commute 30 to 34 mins 
min_commute_30_34_trct <- floor(min(acs_tract$commute_30_34))
max_commute_30_34_trct <- ceiling(max(acs_tract$commute_30_34))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_30_34)) +
  labs(title = "Percent population with commute between 30 and 34 \nminutes by Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_commute_30_34_trct, max_commute_30_34_trct),
                        breaks = seq(min_commute_30_34_trct, max_commute_30_34_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_30_34_trct.png", plot = last_plot
       
       
# commute 35 to 44 mins 
min_commute_35_44_trct <- floor(min(acs_tract$commute_35_44))
max_commute_35_44_trct <- ceiling(max(acs_tract$commute_35_44))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_35_44)) +
  labs(title = "Percent population with commute between 35 and 44 \nminutes by Census tract level, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
    legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_commute_35_44_trct, max_commute_35_44_trct),
                        breaks = seq(min_commute_35_44_trct, max_commute_35_44_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_35_44_trct.png", plot = last_plot
       
       # commute 45 to 59 mins 
       min_commute_45_59_trct <- floor(min(acs_tract$commute_45_59))
       max_commute_45_59_trct <- ceiling(max(acs_tract$commute_45_59))
       ggplot() +
         geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_45_59)) +
         labs(title = "Percent population with commute between 45 and \n59 minutes by Census tract level, 2014/18",
              caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
         theme_map() +
         theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.3),
               legend.title = element_text(size = 11, face = "bold"),
               legend.text = element_text(size = 11),
               legend.position = "right") +
         scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                               limits = c(min_commute_45_59_trct, max_commute_45_59_trct),
                               breaks = seq(min_commute_45_59_trct, max_commute_45_59_trct, length.out = 5))
       ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_45_59_trct.png", plot = last_plot
              
              
 # commute 60+ mins 
      min_commute_60_pl_trct <- floor(min(acs_tract$commute_60_pl))
      max_commute_60_pl_trct <- ceiling(max(acs_tract$commute_60_pl))
      ggplot() +
      geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_60_pl)) +
      labs(title = "Percent population with commute 60+ minutes \nby Census tract level, 2014/18",
                     caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
      theme_map() +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.35),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 11),
      legend.position = "right") +
      scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                                      limits = c(min_commute_60_pl_trct, max_commute_60_pl_trct),
                                      breaks = seq(min_commute_60_pl_trct, max_commute_60_pl_trct, length.out = 5))
      ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_60_pl_trct.png", plot = last_plot
                     