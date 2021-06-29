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
                                P_CANCR,
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

# cancer percentile
min <- floor(min(environment$P_CANCR))
max <- ceiling(max(environment$P_CANCR))
ggplot() +
  geom_sf(data = environment, size = 0.2, aes(fill = P_CANCR, geometry = geometry)) +
  labs(title = "Air Toxin Cancer Risk\n by Census Block Group, 2019",
       caption = "Source: EJ Screen 2019 Estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percentile", low = "#ffc20a", high = "#0c7bdc",
                        limits = c(min, max), 
                        breaks = seq(min, max, length.out = 5))
ggsave(path = "./output/ej", device = "png", filename = "plot_p_cancer.png", plot = last_plot())





