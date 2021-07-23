
# create aggregate tables at 
# block group and tract level
# for factors of interest

# read in ACS data
acs_tract <- readRDS("../data/working/acs_tract.Rds")
acs_bgrp <- readRDS("../data/working/acs_bgrp.Rds")

# read in corelogic aggregate data
bgrp_greenspace <- read.csv("~/git/bgrp_greenspace.csv")
bgrp_greenspace$bgrp_geoid <- as.character(bgrp_greenspace$bgrp_geoid)
  
tract_greenspace <- read.csv("~/git/tract_greenspace.csv")
tract_greenspace$tract_geoid <- as.character(tract_greenspace$tract_geoid)

# merge
tract_data <- acs_tract %>%
  left_join(tract_greenspace, by = c("GEOID" = "tract_geoid")) %>%
  as.data.frame()

bgrp_data <- acs_bgrp %>%
  left_join(bgrp_greenspace, by = c("GEOID" = "bgrp_geoid")) %>%
  as.data.frame()





