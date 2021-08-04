
# create aggregate tables at 
# block group and tract level
# for factors of interest

library(dplyr)
library(sf)

# read in ACS data
acs_tract <- readRDS("./data/working/acs_tract.Rds")
acs_bgrp <- readRDS("./data/working/acs_bgrp.Rds")

# read in corelogic aggregate data
bgrp_greenspace <- read.csv("./data/working/bgrp_greenspace.csv")
bgrp_greenspace$bgrp_geoid <- as.character(bgrp_greenspace$bgrp_geoid)
  
tract_greenspace <- read.csv("./data/working/tract_greenspace.csv")
tract_greenspace$tract_geoid <- as.character(tract_greenspace$tract_geoid)

# merge
tract_data <- acs_tract %>%
  merge(tract_greenspace, 
        by.x = "GEOID", 
        by.y = "tract_geoid",
        all.x = TRUE) %>%
  as.data.frame()

bgrp_data <- acs_bgrp %>%
  merge(bgrp_greenspace, 
        by.x = "GEOID", 
        by.y = "bgrp_geoid",
        all.x = TRUE) %>%
  as.data.frame()

write_rds(tract_data, "./data/working/acs_tract.Rds")
write_rds(bgrp_data, "./data/working/acs_bgrp.Rds")

