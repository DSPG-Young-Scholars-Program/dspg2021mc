library(tidyverse)
library(data.table)


################## EJ Screen Environmetal Data ##################

ej <- fread("data/original/01_ejscreen.csv")

environment <- ej %>% transmute(fips = ID,
                       PM25 = PM25,
                       P_PM25 = P_PM25)
