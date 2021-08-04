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
# create loop
for(x in t_test_tract){
  t_test = x$statistic
  t_test_vector = append(t_test_vector, t_test)
  p_value = x$p.value
  p_value_vector = append(p_value_vector, p_value)
}
# add names/store/save table
t_test_table_tract <- data.frame(names(t_test_tract), t_test_vector, p_value_vector)
write.csv(t_test_table_tract, file = "./output/t_test_table_tract.csv")

# repeat for bgrp
t_test_bgrp <- lapply(bgrp_variables, function(x) 
  t.test(x, na.action = na.pass))
# create empty vectors
t_test_vector <- c()
p_value_vector <- c()
# create loop
for(x in t_test_bgrp){
  t_test = x$statistic
  t_test_vector = append(t_test_vector, t_test)
  p_value = x$p.value
  p_value_vector = append(p_value_vector, p_value)
}
# add names/store/save table
t_test_table_bgrp <- data.frame(names(t_test_bgrp), t_test_vector, p_value_vector)
write.csv(t_test_table_bgrp, file = "./output/t_test_table_bgrp.csv")

# Multi-panel plots ------------ 
# reduce df (9 variables at a time)
tract_pt1 <- acs_tract[, c("black", "hispanic", "noba", "unempl",
                           "inpov", "nohealthins", "rent_ov_30")]
tract_pt2 <- acs_tract[, c("pov_w", "pov_b", "pov_a", "pov_h")]
tract_pt2 <- tract_pt2 %>% 
  rename(
    White = pov_w, Black = pov_b, Asian = pov_a, Hispanic = pov_h
  )
tract_pt3 <- acs_tract[, c("commute_un_10", "commute_10_14","commute_15_19", 
                           "commute_20_24", "commute_25_29", "commute_30_34", 
                           "commute_35_44", "commute_45_59", "commute_60_pl")]  
tract_pt3 <- tract_pt3 %>% 
  rename(
    "Under 10 Minutes" = commute_un_10, "10 to 14 Minutes" = commute_10_14,
    "15 to 19 Minutes" = commute_15_19, "20 to 24 Minutes" = commute_20_24,
    "25 to 29 Minutes" = commute_30_34, "35 to 44 Minutes" = commute_35_44,
    "45 to 59 Minutes" = commute_45_59, "Over 60 Minutes" = commute_60_pl
  )
tract_pt4 <- acs_tract[, c("unemploy_rate_w", "unemploy_rate_b", "unemploy_rate_a", "unemploy_rate_h")]
tract_pt4 <- tract_pt4 %>% 
  rename(
    White = unemploy_rate_w, Black = unemploy_rate_b, Asian = unemploy_rate_a, Hispanic = unemploy_rate_h
  )
tract_pt5 <- acs_tract[, c("ba_higher_w", "ba_higher_a", "ba_higher_b", "ba_higher_h")]
tract_pt5 <- tract_pt5 %>%
  rename(
  White = ba_higher_w, Black = ba_higher_b, Asian = ba_higher_a, Hispanic = ba_higher_h
)
tract_pt6 <- acs_tract[, c("med_inc_w", "med_inc_b", "med_inc_a", "med_inc_h")]
tract_pt6 <- tract_pt6 %>%
  rename(
    White = med_inc_w, Black = med_inc_b, Asian = med_inc_a, Hispanic = med_inc_h
  )
######## Transform data into long format 
tract_long_pt1 <- pivot_longer(
  tract_pt1,
  "black":"rent_ov_30",
  names_to = "variables",
  values_to = "value",
)
tract_long_pt2 <- pivot_longer(
  tract_pt2,
  "White":"Hispanic",
  names_to = "variables",
  values_to = "value",
)
tract_long_pt3 <- pivot_longer(
  tract_pt3,
  "Under 10 Minutes":"Over 60 Minutes",
  names_to = "variables",
  values_to = "value",
)
tract_long_pt4 <- pivot_longer(
  tract_pt4,
  "White":"Hispanic",
  names_to = "variables",
  values_to = "value",
)
tract_long_pt5 <- pivot_longer(
  tract_pt5,
  "White":"Hispanic",
  names_to = "variables",
  values_to = "value",
)
tract_long_pt6 <- pivot_longer(
  tract_pt6,
  "White":"Hispanic",
  names_to = "variables",
  values_to = "value",
)

##### plotting tract
tract_plot_pov <- ggboxplot(
  tract_long_pt2, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", ylab = "Percent", legend = "none",
  title = "Distribution of Racial Groups Living in \n Poverty by tract, 2015/19",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables, scales = "free")
ggsave(path = "./output/", device = "png", filename = "tract_plot_pov.png", plot = last_plot())

tract_plot_commute <- ggboxplot(
  tract_long_pt3, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", ylab = "Percent", legend = "none",
  title = "Distribution of commute times \n by tract, 2015/19",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables, scales = "free")
ggsave(path = "./output/", device = "png", filename = "tract_plot_commute.png", plot = last_plot())

tract_plot_unemploy <- ggboxplot(
  tract_long_pt4, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", ylab = "Percent", legend = "none",
  title = "Distribution of unemployment by racial group \n by tract, 2015/19",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables)
ggsave(path = "./output/", device = "png", filename = "tract_plot_unemploy.png", plot = last_plot())

tract_plot_edu <- ggboxplot(
  tract_long_pt5, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", ylab = "Percent", legend = "none", 
  title = "Distribution of educational attainment \n by racial group by tract, 2015/19",
  ggtheme = theme_pubr(border = TRUE), outlier.shape = 
) +
  facet_wrap(~variables, scales = "free")
ggsave(path = "./output/", device = "png", filename = "tract_plot_edu.png", plot = last_plot())

tract_plot_inc <- ggboxplot(
  tract_long_pt6, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", ylab = "Percent", legend = "none", 
  title = "Distribution of median income \n by racial group by tract, 2015/19",
  ggtheme = theme_pubr(border = TRUE), outlier.shape = 
) +
  facet_wrap(~variables, scales = "free")
ggsave(path = "./output/", device = "png", filename = "tract_plot_inc.png", plot = last_plot())


# block group level
# Transform block group data in to long variables 
bgrp_pt1 <- acs_bgrp[, c("black", "hispanic", "noba", "unempl",
                         "inpov", "nohealthins", "med_inc_w", "med_inc_a")]
bgrp_pt2 <- acs_tract[, c("med_inc_b", "med_inc_h", "pov_w", "pov_b", "pov_a",
                          "pov_h", "rent_ov_30", "commute_un_10")]                  
bgrp_pt3 <- acs_tract[, c("commute_10_14","commute_15_19", "commute_20_24", "commute_25_29",
                          "commute_30_34", "commute_35_44", "commute_45_59", "commute_60_pl")]
bgrp_pt4 <- acs_tract[, c("unemploy_rate_w", "unemploy_rate_a",
                          "unemploy_rate_w", "unemploy_rate_b", "unemploy_rate_h",
                          "ba_higher_w", "ba_higher_a", "ba_higher_h", "ba_higher_b")] 


bgrp_long_pt1 <- pivot_longer(
  bgrp_pt1,
  "black":"med_inc_a",
  names_to = "variables",
  values_to = "value",
)

bgrp_long_pt2 <- pivot_longer(
  bgrp_pt2,
  "med_inc_b":"commute_un_10",
  names_to = "variables",
  values_to = "value",
)

bgrp_long_pt3 <- pivot_longer(
  bgrp_pt3,
  "commute_10_14":"commute_60_pl",
  names_to = "variables",
  values_to = "value",
)

bgrp_long_pt4 <- pivot_longer(
  bgrp_pt4,
  "unemploy_rate_w":"ba_higher_b",
  names_to = "variables",
  values_to = "value",
)



# block group level
bgrp_plot_black <- ggboxplot(
  bgrp_long_pt1, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics",ylab = "Percent",
  title = "Distribution of percent black \n by median income by block, 2015/19", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables, scales = "free")

bgrp_plot_inc <- ggboxplot(
  bgrp_long_pt2, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", ylab = "Percent",  
  title = "Distribution of median income \n by commute under 10mins by block, 2015/19", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables, scales = "free")

bgrp_plot_commute <- ggboxplot(
  bgrp_long_pt3, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", ylab = "Percent",  
  title = "Distribution of unemployment \n by population with ba by block, 2015/19",legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables, scales = "free")


bgrp_plot_unemploy <- ggboxplot(
  bgrp_long_pt4, x = "variables", y = "value", palette = "npg",
  xlab = "Demographics", ylab = "Percent",  
  title = "Distribution of median income \n by racial group by tract, 2015/19",legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables, scales = "free")



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
tract_tab <- rbind(tract_sd, tract_mean, tract_min, tract_max)
tract_tab <- t(tract_tab)
colnames(tract_tab) <- c("sd", "mean", "min", "max")
write.csv(tract_tab, file = "./output/tract_tab.csv")
### variables not available at block group level ###
