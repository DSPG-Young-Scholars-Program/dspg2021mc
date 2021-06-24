library(data.table)
library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)


################## EJ Screen Environmetal Data ##################

ej <- fread("data/original/01_ejscreen.csv")

environment <- ej %>% transmute(fips = ID,
                                ACSTOTPOP = ACSTOTPOP,
                                MINORPOP = MINORPOP,
                                MINORPCT = MINORPCT * 100,
                                ACSIPOVBAS = ACSIPOVBAS,
                                LOWINCOME = LOWINCOME,
                                LOWINCPCT = LOWINCPCT * 100,
                                LINGISOPCT = LINGISOPCT * 100,
                                VULEOPCT = VULEOPCT * 100,
                                DSLPM,
                                CANCER,
                                RESP,
                                PTRAF,
                                OZONE,
                                REGION,
                                PM25 = PM25,
                                P_PM25 = P_PM25)

environment$fips <- as.character(environment$fips)

#
# Write-Out Data -----------------------------------------
#

write_rds(environment, "./data/working/ej.Rds")

#
# Maps ---------------------------------------------------
#

# to get geometry attached to environmental data
acs_bgrp <- read_rds("./data/working/acs_bgrp.Rds")
environment <- left_join(environment, acs_bgrp, by = c("fips" = "GEOID"))

# particles (PM25)
min_pm25 <- floor(min(environment$PM25))
max_pm25 <- ceiling(max(environment$PM25))
ggplot() +
  geom_sf(data = environment, size = 0.2, aes(fill = PM25, geometry = geometry)) +
  labs(title = "PM25 Levels by Census Block Group, 2019",
       caption = "Source: EJ Screen 2019 Estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_pm25, max_pm25), 
                        breaks = seq(min_pm25, max_pm25, length.out = 5))
ggsave(path = "./output/ej/", device = "png", filename = "plot_pm25.png", plot = last_plot())





