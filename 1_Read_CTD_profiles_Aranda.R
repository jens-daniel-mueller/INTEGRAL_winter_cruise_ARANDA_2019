# Packages ----------------------------------------------------------------

library(lubridate)
library(tidyverse)

# Read TS surface data ----------------------------------------------------

df <- read_csv(here::here("Data/CTD_Profiles", "CTD_Aranda_INTEGRAL_WINTER_2019_without_O2.csv")) %>% 
  select(4:10,14,18,22) %>% 
  rename(Station = Station, 
             Lat = Latitude,
             Lon = Longitude, 
             date = `Date visited`,
             time = `Time visited`,
             bottom = `Bottom depth`, 
             dep = Depth,
             fluor = `ctd-fluorescence`,
             sal = `ctd-salinity`, 
             tem = `ctd-temperature`) %>% 
  mutate(date_time = ymd_hms(paste(date, time))) %>% 
  select(-c(date,time))

# Control plots CTD casts -------------------------------------------------


df %>% 
  ggplot(aes(sal, dep, col=Lat, group=Station))+
  geom_path()+
  scale_y_reverse()+
  scale_color_viridis_c()+
  theme_bw()

df %>% 
  ggplot(aes(tem, dep, col=Lat, group=Station))+
  geom_path()+
  scale_y_reverse()+
  scale_color_viridis_c()+
  theme_bw()

df %>% 
  ggplot(aes(fluor, dep, col=Lat, group=Station))+
  geom_path()+
  scale_y_reverse()+
  scale_color_viridis_c()+
  theme_bw()

df %>% 
  filter(dep <5, dep>1) %>% 
  ggplot(aes(date_time, tem, col=sal))+
  geom_point()+
  geom_path()+
  scale_color_viridis_c()+
  theme_bw()



# Write CTD data file -----------------------------------------------------

write_csv(df, here::here("Data/_summarized_data", "CTD_profiles.csv"))


