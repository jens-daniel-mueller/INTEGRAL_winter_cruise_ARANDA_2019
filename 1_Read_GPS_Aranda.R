# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)

# Read final track data ---------------------------------------------------

track <- read_tsv(here::here("Data/GPS_Aranda", "ship_qc.txt"), skip = 14,
                  col_types = list("T","n","n","n","n","n","n","n"))

# Calculate coordinated in decimal degrees --------------------------------

track <-
  track %>% 
  mutate(Lat = GPRMC_latdeg + GPRMC_latmin/60,
         Lon = GPRMC_londeg + GPRMC_lonmin/60) %>% 
  select(date_time=GPRMC_Timestamp, Lat, Lon) %>% 
  filter(date_time > ymd_h("2019-02-28T11"),
         Lat > 40,
         Lon > 0)


track %>% 
  ggplot(aes(Lon, Lat))+
  geom_path()

# write summarized data files ---------------------------------------------


write_csv(track, here("Data/_summarized_data", "GPS_Aranda.csv"))
