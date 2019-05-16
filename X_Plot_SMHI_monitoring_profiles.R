#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)

#library(seacarb)

#### Read CT Airica ####

C3 <- read_csv(here("Data/2012_nutrients_monitoring_SMHI", "SMHI_C3_2000-2012_JM.csv")) %>% 
  mutate(station="C3 (Bothnian Sea, south)")
F3 <- read_csv(here("Data/2012_nutrients_monitoring_SMHI", "SMHI_F3_A5_1968-2012_JM.csv")) %>% 
  mutate(station="F3 (Bothnian Bay, north)")

df <- bind_rows(C3, F3)
rm(C3, F3)

# df <- df %>% 
#   mutate(year.classes = if_else(Year > 2008, as.character(Year), "<2008"))

df.climatology <- df %>% 
  mutate(dep.int = as.numeric(as.character(
    cut(dep, seq(-4,300,10), as.numeric(seq(0,290,10)))))) %>% 
  group_by(station, Month, dep.int) %>% 
  summarise_all(list("mean"), na.rm=TRUE) %>% 
  ungroup()


#### Plot results ####

# Temperature

df.climatology %>% 
  ggplot()+
  geom_path(aes(tem, dep.int, col=station))+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~Month)+
  labs(y="Depth (m)", x="Temperature (deg C)")+
  theme_bw()+
  scale_color_brewer(palette = "Set1", name="Station")+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "Temp_Profiles_Climatology_SMHI_GoB_F3-C3.jpg"), width = 6, height = 6)


ggplot()+
  geom_path(data = df, aes(tem, dep, col="Individual Profiles",
                           group=interaction(Year,Month, Day)))+
  geom_path(data = df.climatology, aes(tem, dep.int, col="Climatological mean"))+
  scale_y_reverse(breaks=seq(0,300,20))+
  scale_color_manual(values = c("red", "grey"), name="")+
  facet_grid(station~Month)+
  labs(y="Depth (m)", x="Temperature (deg C)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "Temp_Profiles_all_SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)



# Phosphate

df.climatology %>% 
  filter(!is.na(PO4)) %>% 
  ggplot()+
  geom_path(aes(PO4, dep.int, col=station))+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~Month)+
  labs(y="Depth (m)", x="Phosphate (uM)")+
  theme_bw()+
  scale_color_brewer(palette = "Set1", name="Station")+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "PO4_Profiles_Climatology_SMHI_GoB_F3-C3.jpg"), width = 6, height = 6)


ggplot()+
  geom_path(data = df[!is.na(df$PO4),], aes(PO4, dep, col="Individual Profiles",
                           group=interaction(Year,Month, Day)))+
  geom_path(data = df.climatology[!is.na(df.climatology$PO4),], aes(PO4, dep.int, col="Climatological mean"))+
  scale_y_reverse(breaks=seq(0,300,20))+
  scale_color_manual(values = c("red", "grey"), name="")+
  facet_grid(Month~station)+
  labs(y="Depth (m)", x="Phosphate (uM)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "PO4_Profiles_all_SMHI_GoB_F3-C3.jpg"), width = 3, height = 15)



ggplot()+
  geom_path(data = df[!is.na(df$PO4) & df$station=="F3 (Bothnian Bay, north)",],
            aes(PO4, dep, col="Individual Profiles",group=interaction(Year,Month, Day)))+
  geom_path(data = df.climatology[!is.na(df.climatology$PO4) & df.climatology$station=="F3 (Bothnian Bay, north)",], 
            aes(PO4, dep.int, col="Climatological mean"))+
  scale_y_reverse(breaks=seq(0,300,20))+
  scale_color_manual(values = c("red", "grey"), name="")+
  facet_wrap(~Month)+
  labs(y="Depth (m)", x="Phosphate (uM)")+
  theme_bw()+
  xlim(0,0.3)+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "PO4_Profiles_all_SMHI_GoB_F3.jpg"), width = 5, height = 6)




# Total Phosphorus

df.climatology %>% 
  filter(!is.na(TP)) %>% 
  ggplot()+
  geom_path(aes(TP, dep.int, col=station))+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~Month)+
  labs(y="Depth (m)", x="Total Phosphorus (uM)")+
  theme_bw()+
  scale_color_brewer(palette = "Set1", name="Station")+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "TP_Profiles_Climatology_SMHI_GoB_F3-C3.jpg"), width = 6, height = 6)


df.climatology %>% 
  filter(!is.na(TP)) %>% 
  ggplot()+
  geom_path(aes(TP, dep.int, col=as.factor(Month)))+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~station, scales = "free")+
  labs(y="Depth (m)", x="Total Phosphorus (uM)")+
  theme_bw()+
  scale_color_brewer(palette = "Spectral", name="Month")

ggsave(here("Plots/SMHI", "TP_Profiles_Climatology_seasonal_SMHI_GoB_F3-C3.jpg"), width = 6, height = 6)


ggplot()+
  geom_path(data = df[!is.na(df$TP),], aes(TP, dep, col="Individual Profiles",
                           group=interaction(Year,Month, Day)))+
  geom_path(data = df.climatology[!is.na(df.climatology$TP),], aes(TP, dep.int, col="Climatological mean"))+
  scale_y_reverse(breaks=seq(0,300,20))+
  scale_color_manual(values = c("red", "grey"), name="")+
  facet_grid(station~Month)+
  labs(y="Depth (m)", x="Total Phosphorus (uM)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "TP_Profiles_all_SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)






# Nitrate

df.climatology %>% 
  filter(!is.na(NO3)) %>% 
  ggplot()+
  geom_path(aes(NO3, dep.int, col=station))+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~Month)+
  labs(y="Depth (m)", x="Nitrate (uM)")+
  theme_bw()+
  scale_color_brewer(palette = "Set1", name="Station")+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "NO3_Profiles_Climatology_SMHI_GoB_F3-C3.jpg"), width = 6, height = 6)


ggplot()+
  geom_path(data = df[!is.na(df$NO3),], aes(NO3, dep, col="Individual Profiles",
                           group=interaction(Year,Month, Day)))+
  geom_path(data = df.climatology[!is.na(df.climatology$NO3),], aes(NO3, dep, col="Climatological mean"))+
  scale_y_reverse(breaks=seq(0,300,20))+
  scale_color_manual(values = c("red", "grey"), name="")+
  facet_grid(station~Month)+
  labs(y="Depth (m)", x="Nitrate (uM)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "NO3_Profiles_all_SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)



# TN

df.climatology %>% 
  filter(!is.na(TN)) %>% 
  ggplot()+
  geom_path(aes(TN, dep.int, col=station))+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~Month)+
  labs(y="Depth (m)", x="Total nitrogen (uM)")+
  theme_bw()+
  scale_color_brewer(palette = "Set1", name="Station")+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "TN_Profiles_Climatology_SMHI_GoB_F3-C3.jpg"), width = 6, height = 6)


ggplot()+
  geom_path(data = df[!is.na(df$TN),], aes(TN, dep, col="Individual Profiles",
                           group=interaction(Year,Month, Day)))+
  geom_path(data = df.climatology[!is.na(df.climatology$TN),], aes(TN, dep, col="Climatological mean"))+
  scale_y_reverse(breaks=seq(0,300,20))+
  scale_color_manual(values = c("red", "grey"), name="")+
  facet_grid(station~Month)+
  labs(y="Depth (m)", x="Total Nitrogen (uM)")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "TN_Profiles_all_SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)




# pH

df.climatology %>% 
  filter(!is.na(pH)) %>% 
  ggplot()+
  geom_path(aes(pH, dep.int, col=station))+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~Month)+
  labs(y="Depth (m)", x="pH")+
  theme_bw()+
  scale_color_brewer(palette = "Set1", name="Station")+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "pH_Profiles_Climatology_SMHI_GoB_F3-C3.jpg"), width = 6, height = 6)


ggplot()+
  geom_path(data = df[!is.na(df$pH),], aes(pH, dep, col="Individual Profiles",
                           group=interaction(Year,Month, Day)))+
  geom_path(data = df.climatology[!is.na(df.climatology$pH),], aes(pH, dep, col="Climatological mean"))+
  scale_y_reverse(breaks=seq(0,300,20))+
  scale_color_manual(values = c("red", "grey"), name="")+
  facet_grid(station~Month)+
  labs(y="Depth (m)", x="pH")+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(here("Plots/SMHI", "pH_Profiles_all_SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)




#### Plot timeseries of surface water parameter ####

df.climatology.timeseries <- df %>% 
  filter(dep <= 100) %>% 
  mutate(dep.int = cut(dep, seq(0,300,20))) %>% 
  filter(!is.na(dep.int)) %>% 
  group_by(station, Month, dep.int) %>% 
  summarise_all(list("mean"), na.rm=TRUE) %>% 
  ungroup()




df.climatology.timeseries %>% 
  filter(!is.na(NO3)) %>% 
  ggplot(aes(Month, NO3))+
  geom_path(aes(col=as.factor(dep.int)))+
  geom_point(aes(fill=as.factor(dep.int)), shape=21)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_color_viridis_d(name="Depth (m)", direction = -1)+
  scale_fill_viridis_d(name="Depth (m)", direction = -1)+
  facet_wrap(~station)+
  labs(y="Nitrate (uM)")+
  theme_bw()
 
ggsave(here("Plots/SMHI", "NO3_climatological_seasonality_SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)

  
df.climatology.timeseries %>% 
  filter(!is.na(tem)) %>% 
  ggplot(aes(Month, tem))+
  geom_path(aes(col=as.factor(dep.int)))+
  geom_point(aes(fill=as.factor(dep.int)), shape=21)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_color_viridis_d(name="Depth (m)", direction = -1)+
  scale_fill_viridis_d(name="Depth (m)", direction = -1)+
  facet_wrap(~station)+
  labs(y="Temperature (deg C)")+
  theme_bw()
 
ggsave(here("Plots/SMHI", "Temp_climatological_seasonality_SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)


df.climatology.timeseries %>% 
  filter(!is.na(PO4)) %>% 
  ggplot(aes(Month, PO4))+
  geom_path(aes(col=as.factor(dep.int)))+
  geom_point(aes(fill=as.factor(dep.int)), shape=21)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_color_viridis_d(name="Depth (m)", direction = -1)+
  scale_fill_viridis_d(name="Depth (m)", direction = -1)+
  facet_wrap(~station, scales = "free")+
  labs(y="Phosphate (uM)")+
  theme_bw()
 
ggsave(here("Plots/SMHI", "PO4_climatological_seasonality__SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)

  

df.climatology.timeseries %>% 
  filter(!is.na(TN)) %>% 
  ggplot(aes(Month, TN))+
  geom_path(aes(col=as.factor(dep.int)))+
  geom_point(aes(fill=as.factor(dep.int)), shape=21)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_color_viridis_d(name="Depth (m)", direction = -1)+
  scale_fill_viridis_d(name="Depth (m)", direction = -1)+
  facet_wrap(~station)+
  labs(y="Total Nitrogen (uM)")+
  theme_bw()
 
ggsave(here("Plots/SMHI", "TN_climatological_seasonality__SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)

  

df.climatology.timeseries %>% 
  filter(!is.na(TP)) %>% 
  #       station=="F3 (Bothnian Bay, north)") %>% 
  ggplot(aes(Month, TP))+
  geom_path(aes(col=as.factor(dep.int)))+
  geom_point(aes(fill=as.factor(dep.int)), shape=21)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_color_viridis_d(name="Depth (m)", direction = -1)+
  scale_fill_viridis_d(name="Depth (m)", direction = -1)+
  facet_wrap(~station, scales = "free_y")+
  labs(y="Total Phosphorus (uM)")+
  theme_bw()
 
ggsave(here("Plots/SMHI", "TP_climatological_seasonality__SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)

  
  

df.climatology.timeseries %>% 
  filter(!is.na(Ox)) %>% 
  ggplot(aes(Month, Ox))+
  geom_path(aes(col=as.factor(dep.int)))+
  geom_point(aes(fill=as.factor(dep.int)), shape=21)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_color_viridis_d(name="Depth (m)", direction = -1)+
  scale_fill_viridis_d(name="Depth (m)", direction = -1)+
  facet_wrap(~station, scales = "free_y")+
  labs(y="O2)")+
  theme_bw()
 
#ggsave(here("Plots/SMHI", "O2_climatological_seasonality__SMHI_GoB_F3-C3.jpg"), width = 10, height = 4)

  
  
  
  
  
  
  
  
  
  