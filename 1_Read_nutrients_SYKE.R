# Packages ----------------------------------------------------------------

library(lubridate)
library(tidyverse)

# Read TS surface data ----------------------------------------------------

df <- read_tsv(here::here("Data/Nutrients_Syke", "INTEGRAL_NUTRIENTS_NOT_QUALITY_CONTROLLED_JM.odv")) %>% 
  select(2,4:9,11:22) %>% 
  rename(Station = 1,
         date_time = 2,
         Lon = 3,
         Lat = 4,
         bottom = 5,
         sechi = 6,
         dep = 7,
         tem = 8,
         sal = 9,
         ox_conc = 10,
         dens = 11,
         ox_sat = 12,
         fluores = 13,
         NO32 = 14, 
         NO2 = 15,
         PO4 = 16, 
         Si = 17, 
         TN = 18, 
         TP = 19)

# Control plots CTD casts -------------------------------------------------

df <- df %>% 
  arrange(Station, dep)

df %>% 
  filter(!is.na(sal)) %>% 
  ggplot(aes(sal, dep, col=Lat, group=Station))+
  geom_path()+
  geom_point(size=0.5)+
  scale_y_reverse()+
  scale_color_viridis_c()+
  theme_bw()


df %>% 
  filter(!is.na(TP)) %>% 
  ggplot(aes(TP, dep, col=Lat, group=Station))+
  geom_path()+
  geom_point(size=0.5)+
  scale_y_reverse()+
  scale_color_viridis_c()+
  theme_bw()

df %>% 
  filter(!is.na(PO4)) %>% 
  ggplot(aes(PO4, dep, col=Lat, group=Station))+
  geom_path()+
  geom_point(size=0.5)+
  scale_y_reverse()+
  scale_color_viridis_c()+
  theme_bw()


# Write CTD data file -----------------------------------------------------

write_csv(df, here::here("Data/_summarized_data", "Nutrient_Profiles_Syke.csv"))


