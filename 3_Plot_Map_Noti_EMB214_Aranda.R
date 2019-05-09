# Packages ----------------------------------------------------------------

library(tidyverse)
library(mapdata)
library(plotKML)
library(data.table)
library(geosphere)
library(ggrepel)
library(rgdal)



# Load coastline data -----------------------------------------------------

baltic.coastlines <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))

land.colour   <- "grey75"
border.colour <- "grey10"


# Load bathymetric data Baltic Sea ----------------------------------------

baltic.bathy   <- read_csv(here::here("Data/Bathymetry", "marmap_coord_4;53;30;67_res_1.csv"))

baltic.bathy   <- baltic.bathy %>% 
  rename(lon = V1, lat = V2, dep = V3) %>% 
  mutate(bathy.breaks = cut(-dep, c(seq(0, 50, length.out = 6), seq(100, 300, length.out = 3), +Inf))) %>% 
  filter(!is.na(bathy.breaks))


# define function for bathymetric map -------------------------------------

plot.map <- function(bathy = baltic.bathy, basemap = baltic.coastlines, xmin = 10, xmax = 30.5, ymin = 53.5, ymax = 65.5){
  
  ggplot() + 
    coord_quickmap(#projection= "azequalarea", 
    xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
    geom_tile(data = na.omit(bathy), aes(x=lon, y=lat, fill=bathy.breaks)) +
    scale_fill_brewer(palette = "Blues", name = "Water depth (m)") +
    geom_polygon(data=basemap, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)
}


# Old EMB214 track as in proposal -----------------------------------------

df_prop <- readGPX(here::here("Data","INTEGRAL_spring_cruise.gpx"))
track_prop <- do.call("rbind", unlist(df_prop$routes, recursive=F))

route_prop <- data.table(data.frame(lon=track_prop[1,], lat=track_prop[2,], Station=track_prop[3,]))
route_prop$lon <- as.numeric(as.character(route_prop$lon))
route_prop$lat <- as.numeric(as.character(route_prop$lat))


# Latest EMB214 as in notification -----------------------------------------------

df <- readGPX(here::here("Data","INTEGRAL_spring_cruise_V2_avoid_3nm.gpx"))
track <- do.call("rbind", unlist(df$routes, recursive=F))

route <- data.table(data.frame(lon=track[1,], lat=track[2,], Station=track[3,]))
route$lon <- as.numeric(as.character(route$lon))
route$lat <- as.numeric(as.character(route$lat))

route[ , lon.prev := shift(lon)]
route[ , lat.prev := shift(lat)]

route$dist.sm<-distHaversine(route[,1:2], route[,4:5])/1852
route$travel.time.hr <- route$dist/10
route[1,]$travel.time.hr <- 0

route$station.time.hr <- 0
route[Station %in% c("TF115",  "TF113",  "TF104",  "TF0284", "TF0283", "MS4", "C3", "B03",  "F3",
                     "RR1", "I1", "RR5",  "RR7", "I2", "I3", "I4", "I5", "I6", "S150", "S142",
                     "S141",  "S112", "S113",  "S120",  "S121", "S122",   "S131", "S132", "S133",
                     "TF200")]$station.time.hr <- 1

route[Station %in% c("BY15", "TF213")]$station.time.hr <- 9

route$total.time.hr <- route$travel.time.hr + route$station.time.hr

route[, Cumul.time.hr := cumsum(total.time.hr)]
route$Cumul.time.days <- route$Cumul.time.hr/24

route$lon.prev <- NULL
route$lat.prev <- NULL

route <- route %>% 
  mutate(lat_deg = floor(lat),
         lat_min = (lat - floor(lat))*60,
         lon_deg = floor(lon),
         lon_min = (lon - floor(lon))*60)


route %>% 
write_csv(here::here("Data/_summarized_data",
                     "EMB214_stations_all.csv"))

Profiles <- route[route$station.time.hr %in% c(1,9),]
#write.csv(Profiles, "INTEGRAL_spring_cruise_stations_casts.csv", row.names = FALSE)



# Load GPS track from INTEGRAL winter cruise on RV Aranda -----------------

track_ARANDA <- read_csv(here::here("Data/_summarized_data", "GPS_Aranda.csv"))


# Exclusive economic zone borders -----------------------------------------

wd <- str_glue(getwd(),"/Data/Bathymetry/World_EEZ_v10_20180221")

setwd(wd)

muni     <- readOGR(dsn=".", layer="eez_boundaries_v10")
# muni    <- subset(muni, esp$EEZ1 %in% c("German Exclusive Economic Zone",
#                                        "Latvian Exclusive Economic Zone",
#                                        "Polish Exclusive Economic Zone",
#                                        "Swedish Exclusive Economic Zone",
#                                        "Finnish Exclusive Economic Zone",
#                                        "Russian Exclusive Economic Zone",
#                                        "Danish Exclusive Economic Zone",
#                                        "Estonian Exclusive Economic Zone",
#                                        "Lithuanian Exclusive Economic Zone"))

muni@data$id <- rownames(muni@data)
muni.df <- fortify(muni)
muni.df <- plyr::join(muni.df, muni@data, by="id")


# Plot tracks on map ------------------------------------------------------

#small

plot.map()+
  labs(x="Longitude (deg E)", y="Latitude (deg N)")+
  geom_path(data=muni.df, aes(x=long, y=lat, group=group, col="EEZ borders"), size=0.5)+
  geom_path(data=track_ARANDA, aes(Lon,Lat, col="INTEGRAL winter"), size=1)+
  geom_path(data=route_prop, aes(lon,lat, col="EMB214 proposal"), size=1)+
  geom_path(data=route, aes(lon,lat, col="EMB214 notification"), size=1)+
  geom_point(data=route[route$station.time.hr == 1,], aes(lon,lat, shape="CTD cast"), size=1.5, col="black", fill="white")+
  geom_point(data=route[route$station.time.hr == 9,], aes(lon,lat, shape="CTD cast"), size=2, col="black", fill="white")+
  geom_point(data=route[route$station.time.hr == 0,], aes(lon,lat, shape="Waypoint"), size=0.5, col="black", fill="white")+
  scale_shape_manual(values = c(21,24,22), name="Stations/Waypoints")+
  scale_color_manual(values = c("black","red", "grey", "orange"), name="Track")


ggsave(here::here("Plots/Tracks", "cruise_tracks_INTEGRAL_winter_and_summer.jpg"),
       width = 8, height = 8, dpi = 300)

#large

plot.map()+
  labs(x="Longitude (deg E)", y="Latitude (deg N)")+
  geom_path(data=muni.df, aes(x=long, y=lat, group=group, col="EEZ borders"), size=1)+
  geom_path(data=track_ARANDA, aes(Lon,Lat, col="INTEGRAL winter"), size=1)+
  geom_path(data=route_prop, aes(lon,lat, col="EMB214 proposal"), size=1)+
  geom_path(data=route, aes(lon,lat, col="EMB214 notification"), size=1)+
  geom_point(data=route[route$station.time.hr == 1,], aes(lon,lat, shape="CTD cast"), size=1.5, col="black", fill="white")+
  geom_point(data=route[route$station.time.hr == 9,], aes(lon,lat, shape="CTD cast"), size=2, col="black", fill="white")+
  geom_point(data=route[route$station.time.hr == 0,], aes(lon,lat, shape="Waypoint"), size=0.5, col="black", fill="white")+
  geom_text_repel(data=route, 
                  aes(lon,lat, label=Station))+
  scale_shape_manual(values = c(21,24,22), name="Stations/Waypoints")+
  scale_color_manual(values = c("black","red", "grey", "orange"), name="Track")

ggsave(here::here("Plots/Tracks", "cruise_tracks_INTEGRAL_winter_and_summer_large.pdf"),
       width = 45, height = 45, dpi = 300)
