# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggthemes")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
library(tidycensus)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggthemes)
library(ggplot2)

######## Pull ACS 2015/19 data for basic Arlington County sociodemographics #################

acs_tract <- read_rds("./data/working/acs_tract.Rds")
acs_bgrp <- read_rds("./data/working/acs_bgrp.Rds")


# Plot ----------------------------------------------------------------------

# Block Group --------------------------------------------------

# black

min_black_bgrp <- floor(min(acs_bgrp$black, na.rm = T))
max_black_bgrp <- ceiling(max(acs_bgrp$black, na.rm = T))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = black)) +
  labs(title = "Percent population Black \nby Census block level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_black_bgrp, max_black_bgrp),
                        breaks = seq(min_black_bgrp, max_black_bgrp, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_black_bgrp.png", plot = last_plot())


# without bachelors (noba)
min_noba_bgrp <- floor(min(acs_bgrp$noba, na.rm = T))
max_noba_bgrp <- ceiling(max(acs_bgrp$noba, na.rm = T))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = noba)) +
  labs(title = "Percent population without bachelor's degree \nby Block tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_noba_bgrp, max_noba_bgrp),
                        breaks = seq(min_noba_bgrp, max_noba_bgrp, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_noba_bgrp.png", plot = last_plot())

# unemployed
min_unempl_bgrp <- floor(min(acs_bgrp$unempl, na.rm = T))
max_unempl_bgrp <- ceiling(max(acs_bgrp$unempl, na.rm = T))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = unempl)) +
  labs(title = "Percent population unemployed \nby Census Block level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_unempl_bgrp, max_unempl_bgrp),
                        breaks = seq(min_unempl_bgrp, max_unempl_bgrp, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_unempl_bgrp.png", plot = last_plot())


# Tract level --------------------------------------------
# hispanic
min_hispanic_trct <- floor(min(acs_tract$hispanic, na.rm = T))
max_hispanic_trct <- ceiling(max(acs_tract$hispanic, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = hispanic)) +
  labs(title = "Percent population Hispanic \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_hispanic_trct, max_hispanic_trct),
                        breaks = seq(min_hispanic_trct, max_hispanic_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_hispanic_trct.png", plot = last_plot())

# black
min_black_trct <- floor(min(acs_tract$black, na.rm = T))
max_black_trct <- ceiling(max(acs_tract$black, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = black)) +
  labs(title = "Percent population Black \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_black_trct, max_black_trct),
                        breaks = seq(min_black_trct, max_black_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_black_trct.png", plot = last_plot())

# without bachelors (noba)
min_noba_trct <- floor(min(acs_tract$noba, na.rm = T))
max_noba_trct <- ceiling(max(acs_tract$noba, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = noba)) +
  labs(title = "Percent population without bachelor's degree \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_noba_trct, max_noba_trct),
                        breaks = seq(min_noba_trct, max_noba_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_noba_trct.png", plot = last_plot())

# unemployed
min_unempl_trct <- floor(min(acs_tract$unempl, na.rm = T))
max_unempl_trct <- ceiling(max(acs_tract$unempl, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = unempl)) +
  labs(title = "Percent population unemployed \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_unempl_trct, max_unempl_trct),
                        breaks = seq(min_unempl_trct, max_unempl_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_unempl_trct.png", plot = last_plot())

# in poverty
min_inpov_trct <- floor(min(acs_tract$inpov, na.rm = T))
max_inpov_trct <- ceiling(max(acs_tract$inpov, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = inpov)) +
  labs(title = "Percent population in poverty \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_inpov_trct, max_inpov_trct),
                        breaks = seq(min_inpov_trct, max_inpov_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_inpov_trct.png", plot = last_plot())

# no health insurance
min_nohealthins_trct <- floor(min(acs_tract$nohealthins, na.rm = T))
max_nohealthins_trct <- ceiling(max(acs_tract$nohealthins, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = nohealthins)) +
  labs(title = "Percent population in without health insurance \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.3),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_nohealthins_trct, max_nohealthins_trct),
                        breaks = seq(min_nohealthins_trct, max_nohealthins_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nohealthins_trct.png", plot = last_plot())


# median income white
min_med_inc_w_trct <- floor(min(acs_tract$med_inc_w, na.rm = T))
max_med_inc_w_trct <- ceiling(max(acs_tract$med_inc_w, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = med_inc_w)) +
  labs(title = "Median household income white \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.3),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Dollars", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_nohealthins_trct, max_nohealthins_trct),
                        breaks = seq(min_nohealthins_trct, max_nohealthins_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nohealthins_trct.png", plot = last_plot())


# poverty status white
min_pov_w_trct <- floor(min(acs_tract$pov_w, na.rm = T))
max_pov_w_trct <- ceiling(max(acs_tract$pov_w, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_w)) +
  labs(title = "Percent population in poverty (white) \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_pov_w_trct, max_pov_w_trct),
                        breaks = seq(min_pov_w_trct, max_pov_w_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_w_trct.png", plot = last_plot())

# poverty status black
# poverty status black
# Drop tract level 9802; reassign acs_tract
acs_tract <- acs_tract[-c(54), ]
min_pov_b_trct <- floor(min(acs_tract$pov_b, na.rm = T))
max_pov_b_trct <- ceiling(max(acs_tract$pov_b, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_b)) +
  labs(title = "Percent population in poverty (Black) \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_pov_b_trct, max_pov_b_trct),
                        breaks = seq(min_pov_b_trct, max_pov_b_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_b_trct.png", plot = last_plot())

# poverty status asian
# Drop tract level 1034.01; reassign acs_tract
acs_tract <- acs_tract[-c(47), ]
min_pov_a_trct <- floor(min(acs_tract$pov_a, na.rm = T))
max_pov_a_trct <- ceiling(max(acs_tract$pov_a, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_a)) +
  labs(title = "Percent population in poverty (Asian) \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_pov_a_trct, max_pov_a_trct),
                        breaks = seq(min_pov_a_trct, max_pov_a_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_a_trct.png", plot = last_plot())

# poverty status hispanic
min_pov_h_trct <- floor(min(acs_tract$pov_h, na.rm = T))
max_pov_h_trct <- ceiling(max(acs_tract$pov_h, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_h)) +
  labs(title = "Percent population in poverty (Hispanic) \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_pov_h_trct, max_pov_h_trct),
                        breaks = seq(min_pov_h_trct, max_pov_h_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_h_trct.png", plot = last_plot())

# poverty status one or more races
min_pov_o_trct <- floor(min(acs_tract$pov_o, na.rm = T))
max_pov_o_trct <- ceiling(max(acs_tract$pov_o, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = pov_o)) +
  labs(title = "Percent population in poverty (one or more \nraces) by Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_pov_o_trct, max_pov_o_trct),
                        breaks = seq(min_pov_o_trct, max_pov_o_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_pov_o_trct.png", plot = last_plot())

# gross rent over 30 percent of income
min_rent_ov_30_trct <- floor(min(acs_tract$rent_ov_30, na.rm = T))
max_rent_ov_30_trct <- ceiling(max(acs_tract$rent_ov_30, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = rent_ov_30)) +
  labs(title = "Percent population whose rent more than 30 percent \nof income by Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.3),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_rent_ov_30_trct, max_rent_ov_30_trct),
                        breaks = seq(min_rent_ov_30_trct, max_rent_ov_30_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_rent_ov_30_trct.png", plot = last_plot())

# commute under 10 mins
min_commute_un_10_trct <- floor(min(acs_tract$commute_un_10, na.rm = T))
max_commute_un_10_trct <- ceiling(max(acs_tract$commute_un_10, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_un_10)) +
  labs(title = "Percent population with commute under 10 minutes \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_un_10_trct, max_commute_un_10_trct),
                        breaks = seq(min_commute_un_10_trct, max_commute_un_10_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_un_10_trct.png", plot = last_plot())


# commute 10 to 14 mins
min_commute_10_14_trct <- floor(min(acs_tract$commute_10_14, na.rm = T))
max_commute_10_14_trct <- ceiling(max(acs_tract$commute_10_14, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_10_14)) +
  labs(title = "Percent population with commute between 10 and 14 \nminutes by Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_10_14_trct, max_commute_10_14_trct),
                        breaks = seq(min_commute_10_14_trct, max_commute_10_14_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_10_14_trct.png", plot = last_plot())

# commute 15 to 19 mins 
min_commute_15_19_trct <- floor(min(acs_tract$commute_15_19, na.rm = T))
max_commute_15_19_trct <- ceiling(max(acs_tract$commute_15_19, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_15_19)) +
  labs(title = "Percent population with commute between 15 and 19 minutes \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_15_19_trct, max_commute_15_19_trct),
                        breaks = seq(min_commute_15_19_trct, max_commute_15_19_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_15_19_trct.png", plot = last_plot())

# commute 20 to 24 mins 
min_commute_20_24_trct <- floor(min(acs_tract$commute_20_24, na.rm = T))
max_commute_20_24_trct <- ceiling(max(acs_tract$commute_20_24, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_20_24)) +
  labs(title = "Percent population with commute between 20 and 24 \nminutes by Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_20_24_trct, max_commute_20_24_trct),
                        breaks = seq(min_commute_20_24_trct, max_commute_20_24_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_20_24_trct.png", plot = last_plot())

# commute 25 to 29 mins 
min_commute_25_29_trct <- floor(min(acs_tract$commute_25_29, na.rm = T))
max_commute_25_29_trct <- ceiling(max(acs_tract$commute_25_29, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_25_29)) +
  labs(title = "Percent population with commute between 25 and 29 \nminutes by Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_25_29_trct, max_commute_25_29_trct),
                        breaks = seq(min_commute_25_29_trct, max_commute_25_29_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_25_29_trct.png", plot = last_plot())

# commute 30 to 34 mins 
min_commute_30_34_trct <- floor(min(acs_tract$commute_30_34, na.rm = T))
max_commute_30_34_trct <- ceiling(max(acs_tract$commute_30_34, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_30_34)) +
  labs(title = "Percent population with commute between 30 and 34 \nminutes by Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_30_34_trct, max_commute_30_34_trct),
                        breaks = seq(min_commute_30_34_trct, max_commute_30_34_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_30_34_trct.png", plot = last_plot())


# commute 35 to 44 mins 
min_commute_35_44_trct <- floor(min(acs_tract$commute_35_44, na.rm = T))
max_commute_35_44_trct <- ceiling(max(acs_tract$commute_35_44, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_35_44)) +
  labs(title = "Percent population with commute between 35 and 44 \nminutes by Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_35_44_trct, max_commute_35_44_trct),
                        breaks = seq(min_commute_35_44_trct, max_commute_35_44_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_35_44_trct.png", plot = last_plot())

# commute 45 to 59 mins 
min_commute_45_59_trct <- floor(min(acs_tract$commute_45_59, na.rm = T))
max_commute_45_59_trct <- ceiling(max(acs_tract$commute_45_59, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_45_59)) +
  labs(title = "Percent population with commute between 45 and \n59 minutes by Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.3),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_45_59_trct, max_commute_45_59_trct),
                        breaks = seq(min_commute_45_59_trct, max_commute_45_59_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_45_59_trct.png", plot = last_plot())


# commute 60+ mins 
min_commute_60_pl_trct <- floor(min(acs_tract$commute_60_pl, na.rm = T))
max_commute_60_pl_trct <- ceiling(max(acs_tract$commute_60_pl, na.rm = T))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = commute_60_pl)) +
  labs(title = "Percent population with commute 60+ minutes \nby Census tract level, 2015/19",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.35),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min_commute_60_pl_trct, max_commute_60_pl_trct),
                        breaks = seq(min_commute_60_pl_trct, max_commute_60_pl_trct, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_commute_60_pl_trct.png", plot = last_plot())
                     