#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)

#### Load data of IOW Equilibrator system ####

df <- read_csv( here("Data/_merged_data", "Track_TS_Equi_pH.csv"))


#### Plot surface maps ####


basemap <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))
land.colour   <- "grey75"
border.colour <- "grey10"
xmin= 17
xmax= 25.5
ymin=59
ymax=66


df %>% 
  filter(!is.na(SST)) %>% 
  ggplot(aes(Lon, Lat, col=SST))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps", "SST_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)

df %>% 
  filter(!is.na(Sal)) %>% 
  ggplot(aes(Lon, Lat, col=Sal))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps", "Sal_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)


df %>% 
  filter(!is.na(CH4_CO2),
         CH4_CO2 > 420,
         CH4_CO2 < 550) %>% 
  ggplot(aes(Lon, Lat, col=CH4_CO2))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps", "pCO2_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)


df %>% 
  filter(!is.na(CH4_CH4),
         CH4_CH4 >2,
         CH4_CH4 <12) %>% 
  ggplot(aes(Lon, Lat, col=CH4_CH4))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps", "pCH4_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)

df %>% 
  filter(!is.na(pH),
         pH>7.5) %>% 
  ggplot(aes(Lon, Lat, col=pH))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps", "pH_Map_INTEGRAL_winter_RV-Aranda.jpg"), width = 7, height = 8)




#### Plot surface maps ####

df.Finn.Arch <- df %>% 
  filter(Lon > 20, Lon < 22, Lat > 60.2, Lat < 61.5)

basemap <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))
land.colour   <- "grey75"
border.colour <- "grey10"
xmin= 20
xmax= 22
ymin=60.5
ymax=61.5


df.Finn.Arch %>% 
  filter(!is.na(SST)) %>% 
  ggplot(aes(Lon, Lat, col=SST))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps/Finn_Arch", "SST_Map_INTEGRAL_winter_RV-Aranda.jpg"), 
       width = 4, height = 3)

df.Finn.Arch %>% 
  filter(!is.na(Sal),
         Sal >5) %>% 
  ggplot(aes(Lon, Lat, col=Sal))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps/Finn_Arch", "Sal_Map_INTEGRAL_winter_RV-Aranda.jpg"), 
       width = 4, height = 3)

df.Finn.Arch %>% 
  filter(!is.na(CH4_CO2)) %>% 
  ggplot(aes(Lon, Lat, col=CH4_CO2))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps/Finn_Arch", "pCO2_Map_INTEGRAL_winter_RV-Aranda.jpg"), 
       width = 4, height = 3)

df.Finn.Arch %>% 
  filter(!is.na(CH4_CH4)) %>% 
  ggplot(aes(Lon, Lat, col=CH4_CH4))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps/Finn_Arch", "pCH4_Map_INTEGRAL_winter_RV-Aranda.jpg"), 
       width = 4, height = 3)

df.Finn.Arch %>% 
  filter(!is.na(pH)) %>% 
  ggplot(aes(Lon, Lat, col=pH))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_c()+
  theme_bw()

ggsave(here("Plots/Surface_maps/Finn_Arch", "pH_Map_INTEGRAL_winter_RV-Aranda.jpg"), 
       width = 4, height = 3)



