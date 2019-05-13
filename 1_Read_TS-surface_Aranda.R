# Packages ----------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(here)


# Read TS surface data ----------------------------------------------------

SST <- read_table2(here::here("Data/TS_Surface", "SST_Aranda_INTEGRAL_WINTER.txt"), skip = 4,
                   col_names = c("date_time","SST","source","SST_well","SST_hull",
                                 "SST_CTD","SST_origWell", "SST_origHull")) %>% 
  select(date_time, SST) %>% 
  filter(date_time > ymd_h("2019-02-28 11"))

SSS <- read_table2(here::here("Data/TS_Surface", "SSS_Aranda_INTEGRAL_WINTER.txt"), skip = 4,
                   col_names = c("date_time","SSS","SSS_ctd","Temp", "SS_orgFlow")) %>% 
  select(date_time, SSS) %>% 
  filter(date_time > ymd_h("2019-02-28 11"))



# Merge SST and SSS data --------------------------------------------------

TS <- full_join(SSS, SST)



# Control plots TS timeseries ---------------------------------------------

TS %>% 
  ggplot(aes(date_time, SST))+
  geom_path()+
  theme_bw()

TS %>% 
  ggplot(aes(date_time, SSS))+
  geom_path()+
  theme_bw()



# Write TS data file ------------------------------------------------------

write_csv(TS, here("Data/_summarized_data", "TS_surface.csv"))


