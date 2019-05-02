# Packages ----------------------------------------------------------------

library(seacarb)
library(lubridate)
library(tidyverse)
library(here)


# Read Tavastaland data provided by Anna Willstrand Wranne (SMHI) ---------

df <- read_tsv(here("Data/2018_Tavastaland", "Tavastland_38055_20171203000046_20180430000049_JM.txt"))

df <-
  df %>% 
  mutate(date.time=ymd_hms(date.time)) %>% 
  filter(pCO2_from_co2>0,
         Tem_eq-Tem < 3)

df <- df[,-c(3,5,7,9:13,15:17,19)]


# Produce some overview plots ---------------------------------------------

df %>% 
  ggplot(aes(Lon, Lat)) +
  geom_point()

df %>% 
  ggplot(aes(Tem, Tem_eq-Tem)) +
  geom_point()

df %>% 
  ggplot()+
  geom_point(aes(Lat, Sal_8181))

df %>% 
  filter(Lat <58, Lat>57) %>% 
  ggplot(aes(date.time, pCO2_from_co2)) +
  geom_point()

df %>% 
  filter(Lat>60) %>% 
  ggplot(aes(date.time, pCO2_from_co2, col=Lat)) +
  geom_point()+
  ylim(0,500)



# Filter and calculate area mean values for Gulf of Bothnia ---------------

df_north <-
  df %>%
  filter(Lat>60, Lat <65.5,
         year(date.time) == 2018) %>% 
  mutate(Lat.int = cut(Lat, seq(60,80,1)),
         week = week(date.time))

df_north_mean <-
  df_north %>%
  group_by(Lat.int, week) %>% 
  summarise_all(list("mean","min","max","sd"), na.rm=TRUE) %>% 
  ungroup()



# Plots for the Gulf of Bothnia -------------------------------------------

df_north_mean %>% 
  ggplot(aes(date.time_mean, pCO2_from_co2_mean, 
             ymin=pCO2_from_co2_min, ymax=pCO2_from_co2_max, 
             col=Lat.int, fill=Lat.int))+
  geom_hline(yintercept = 400)+
  geom_ribbon(alpha=0.5)+
  geom_path()+
  scale_color_viridis_d(name="Lat intervals")+
  scale_fill_viridis_d(name="Lat intervals")+
  theme_bw()+
  labs(x="Date", y="Weekly mean pCO2 (?atm)")+
  facet_wrap(~Lat.int)

ggsave(here("Plots/Tavastaland", "pCO2_Tavastaland_GuB_spring_2018.jpg"))

df_north_mean %>% 
  ggplot(aes(date.time_mean,Tem_mean, 
             ymin=Tem_min, ymax=Tem_max, 
             col=Lat.int, fill=Lat.int))+
  geom_ribbon(alpha=0.5)+
  geom_path()+
  scale_color_viridis_d(name="Lat intervals")+
  scale_fill_viridis_d(name="Lat intervals")+
  theme_bw()+
  labs(x="Date", y="Weekly mean Temp (?C)")+
  facet_wrap(~Lat.int)

ggsave(here("Plots/Tavastaland", "Temperature_Tavastaland_GuB_spring_2018.jpg"))



basemap <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))
land.colour   <- "grey75"
border.colour <- "grey10"
xmin= 17
xmax= 25
ymin=60
ymax=66


df_north %>% 
  ggplot(aes(Lon, Lat, col=Lat.int))+
  geom_point()+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  scale_color_viridis_d(name="Lat intervals")+
  theme_bw()

ggsave(here("Plots/Tavastaland", "Map_Tavastaland_GuB_spring_2018.jpg"))

