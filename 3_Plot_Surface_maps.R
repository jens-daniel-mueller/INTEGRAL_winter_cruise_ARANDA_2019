# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Load merged surface data files ------------------------------------------

df_sum <- read_csv(here::here("Data/_merged_data", "Surface_data_5min_resolution.csv"))
df <- read_csv(here::here("Data/_merged_data", "Surface_data_full_resolution.csv"))


# Plot surface maps -------------------------------------------------------

basemap <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))
land.colour   <- "grey75"
border.colour <- "grey10"
xmin= 17
xmax= 25.5
ymin=59
ymax=66


df_sum %>% 
  ggplot(aes(Lon, Lat, col=SST))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here::here("Plots/Surface_maps", "SST_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)

df_sum %>% 
  ggplot(aes(Lon, Lat, col=SSS))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here::here("Plots/Surface_maps", "SSS_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)


df_sum %>% 
  filter(pCO2_Equi < 500) %>%
  ggplot(aes(Lon, Lat, col=pCO2_Equi))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  labs(title = "restricted to pCO2 < 500 uatm")+
  theme_bw()

ggsave(here::here("Plots/Surface_maps", "pCO2_Equi_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)


df_sum %>% 
  filter(pCH4_Equi < 10) %>%
  ggplot(aes(Lon, Lat, col=pCH4_Equi))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  labs(title = "restricted to pCH4 < 10 uatm")+
  theme_bw()

ggsave(here::here("Plots/Surface_maps", "pCH4_Equi_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)

df_sum %>% 
  filter(pHT_0218 > 7.5) %>% 
  ggplot(aes(Lon, Lat, col=pHT_0218))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  labs(title = "restricted to pH >7.5")+
  theme_bw()

ggsave(here::here("Plots/Surface_maps", "pH_0218_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)
