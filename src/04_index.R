library(data.table)
library(tidyverse)

#
# Index Creation -----------------------------
#

acs_tract <- read_rds("./data/working/acs_tract.Rds")

# we have information split by race for med_inc, pov, unemploy_rate, and ba_higher
acs_tract <- acs_tract %>%
                mutate(pov_index = (((abs(pov_a - pov_all) + abs(pov_h - pov_all) + abs(pov_w - pov_all) +
                         abs(pov_b - pov_all) + abs(pov_o - pov_all))/5)*100)/pov_all,
                med_inc_index = (((abs(med_inc_a - med_inc_all) + abs(med_inc_h - med_inc_all) + abs(med_inc_w - med_inc_all) +
                           abs(med_inc_b - med_inc_all) + abs(med_inc_o - med_inc_all))/5)*100)/med_inc_all,
                unemploy_rate_index = (((abs(unemploy_rate_a - unemploy_rate_all) + abs(unemploy_rate_h - unemploy_rate_all) + abs(unemploy_rate_w - unemploy_rate_all) +
                          abs(unemploy_rate_b - unemploy_rate_all))/4)*100)/unemploy_rate_all,
                ba_higher_rate_index = (((abs(ba_higher_a - ba_higher_all) + abs(ba_higher_h - ba_higher_all) + abs(ba_higher_w - ba_higher_all) +
                                           abs(ba_higher_b - ba_higher_all))/4)*100)/ba_higher_all)

ggplot(acs_tract, aes(x=pov_index)) +
  geom_histogram()
