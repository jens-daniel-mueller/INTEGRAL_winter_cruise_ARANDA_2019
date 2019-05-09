library(lubridate)
library(tidyverse)
library(here)


#### Read TS surface data ####

TS <- read_table2(here("Data/TS_Surface", "temp_sal_20190310b.txt"), skip = 1,
                 col_names = c("date.time","VAT1C","VAT3C","VAT4C","VACNS","VASAL","VAT5C"),
                 col_types = list("c","n","n","n","n","n","n")) %>% 
  mutate(date.time = ymd_hms(date.time)) %>% 
  select(date.time,
         SST = VAT5C,
         Sal = VASAL,
         Air.temp = VAT1C) %>% 
  filter(date.time > ymd_h("2019-02-28 11"))


#### Control plots TS timeseries ####

TS %>% 
  ggplot(aes(date.time, SST))+
  geom_path()+
  theme_bw()


TS %>% 
  ggplot(aes(date.time, Sal))+
  geom_path()+
  theme_bw()

TS %>% 
  ggplot(aes(date.time, Air.temp))+
  geom_path()+
  theme_bw()


#### remove errorneous low salinity data ####

TS <- 
  TS %>% 
  mutate(SST = if_else(SST<2.5, SST, NaN))

# TS <- 
#   TS %>% 
#   mutate(Sal = if_else(Sal>2, Sal, NaN))


write_csv(TS, here("Data/_summarized_data", "TS_surface.csv"))


