library(tidyverse)
library(rstatix)
library(ggpubr)
library(readr)
library(dplyr)
library(tidyr)

# Read in data
acs_tract <- read_rds("./data/working/acs_tract.Rds")
acs_bgrp <- read_rds("./data/working/acs_bgrp.Rds")

# Prepare data and inspect random sample
tract_variables <- acs_tract[, c("black", "hispanic", "noba", "unempl",
                                 "inpov", "nohealthins", "med_inc_w", "med_inc_a",
                                 "med_inc_b", "med_inc_h", "pov_w", "pov_b", "pov_a",
                                 "pov_h", "rent_ov_30", "commute_un_10", "commute_10_14",
                                 "commute_15_19", "commute_20_24", "commute_25_29",
                                 "commute_30_34", "commute_35_44", "commute_45_59",
                                 "commute_60_pl", "unemploy_rate_w", "unemploy_rate_a",
                                 "unemploy_rate_w", "unemploy_rate_b", "unemploy_rate_h",
                                 "ba_higher_w", "ba_higher_a", "ba_higher_h", "ba_higher_b")]

bgrp_variables <- acs_bgrp[, c("black", "noba", "unempl", "rent_ov_30", "commute_un_10", "commute_10_14",
                               "commute_15_19", "commute_20_24", "commute_25_29",
                               "commute_30_34", "commute_35_44", "commute_45_59",
                               "commute_60_pl")]

# T-test tables ----------
t_test_tract <- lapply(tract_variables, function(x) 
  t.test(x, na.action = na.pass))
# create empty vectors
t_test_vector <- c()
p_value_vector <- c()
conf_int_vector <- c()
# create loop
for(x in t_test_tract){
  t_test = x$statistic
  t_test_vector = append(t_test_vector, t_test)
  p_value = x$p.value
  p_value_vector = append(p_value_vector, p_value)
  conf_int = x$conf.int
  conf_int_vector = append(conf_int_vector, conf_int)
}
# add names/store/save table
t_test_table_tract <- data.frame(names(t_test_tract), t_test_vector, p_value_vector, conf_int_vector)
write_csv(t_test_table_tract, "./output") # need to fix conf int

# Multi-panel plots ------------
# reduce df (8-9 variables at a time)
tract_pt1 <- acs_tract[, c("black", "hispanic", "noba", "unempl",
                           "inpov", "nohealthins", "med_inc_w", "med_inc_a")]
tract_pt2 <- acs_tract[, c("med_inc_b", "med_inc_h", "pov_w", "pov_b", "pov_a",
                           "pov_h", "rent_ov_30", "commute_un_10")]                  
tract_pt3 <- acs_tract[, c("commute_10_14","commute_15_19", "commute_20_24", "commute_25_29",
                            "commute_30_34", "commute_35_44", "commute_45_59", "commute_60_pl")]
tract_pt4 <- acs_tract[, c("unemploy_rate_w", "unemploy_rate_a",
                           "unemploy_rate_w", "unemploy_rate_b", "unemploy_rate_h",
                           "ba_higher_w", "ba_higher_a", "ba_higher_h", "ba_higher_b")] 

# Transform data into long format (do 4 times)
tract_long_pt1 <- pivot_longer(
  tract_pt1,
  "black":"med_inc_a",
  names_to = "variables",
  values_to = "value",
)
tract_long_pt2 <- pivot_longer(
  tract_pt2,
  "med_inc_b":"commute_un_10",
  names_to = "variables",
  values_to = "value",
)
tract_long_pt3 <- pivot_longer(
  tract_pt3,
  "commute_10_14":"commute_60_pl",
  names_to = "variables",
  values_to = "value",
)
tract_long_pt4 <- pivot_longer(
  tract_pt4,
  "unemploy_rate_w":"ba_higher_b",
  names_to = "variables",
  values_to = "value",
)

# tract (issues with scales)
tract_plots <- ggboxplot(
  tract_long_pt1, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables)
# Add statistical test p-values
tract.stat.test <- tract.stat.test %>% add_xy_position(x = "variables")
myplot + stat_pvalue_manual(tract.stat.test, label = "p.adj.signif")

# block group level
ggboxplot(
  bgrp_variables_long, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables)

# Summary stats tables ----------
# select variables
tract_vars <- acs_tract[, c("black", "hispanic", "inpov", "med_inc_w", "med_inc_a",
                               "med_inc_b", "med_inc_h", "no_vehic", "perc_under_18")]
## summary statistics ##
# standard deviation
tract_sd <- tract_vars %>% 
  summarise_if(is.numeric, sd, na.rm = TRUE)
# mean
tract_mean <- tract_vars %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
# max
tract_max <- tract_vars %>% 
  summarise_if(is.numeric, max, na.rm = TRUE)
# min
tract_min <- tract_vars %>% 
  summarise_if(is.numeric, min, na.rm = TRUE)

# combine
tract_tab <- rbind(tract_sd, tract_mean, tract_min, tract_max,
              deparse.level = 1, make.row.names = TRUE,
              stringsAsFactors = default.stringsAsFactors(), factor.exclude = NA)
tract_tab <- t(tract_tab)
colnames(tract_tab) <- c("sd", "mean", "min", "max")
print(tract_tab)

write.table(tract_tab, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

### variables not available at block group level ###
