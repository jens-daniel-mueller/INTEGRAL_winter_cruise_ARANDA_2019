library(tidyverse)
library(lubridate)
library(here)



#### Read Track data ####

track <- read_tsv(here("Data/Track", "positions_20190310b.txt"),
               col_types = list("T","n","n","n","n"))

# track <-
#   track %>% 
#   mutate(Lat = Lat_deg + Lat_min/60,
#          Lon = Lon_deg + Lon_min/60) %>% 
#   select(date.time=UTC, Lat, Lon) %>% 
#   filter(date.time > ymd_h("2019-02-28T11"),
#          Lat > 40,
#          Lon > 0)
track <-
  track %>% 
  mutate(Lat = GPRMC_latdeg + GPRMC_latmin/60,
         Lon = GPRMC_londeg + GPRMC_lonmin/60) %>% 
  select(date.time=GPRMC_Timestamp, Lat, Lon) %>% 
  filter(date.time > ymd_h("2019-02-28T11"),
         Lat > 40,
         Lon > 0)


track %>% 
  ggplot(aes(Lon, Lat))+
  geom_path()


write_csv(track, here("Data/_summarized_data", "Track_INTEGRAL_winter.csv"))
