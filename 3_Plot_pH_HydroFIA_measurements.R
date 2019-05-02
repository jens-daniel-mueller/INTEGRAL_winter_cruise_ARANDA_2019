#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)

#### Read HydroFIA pH surface data ####

HF <- read_csv(here("Data/_summarized_data", "HydroFIA_pH_all_INTEGRAL_winter.csv"))
HF.SW <- read_csv(here("Data/_summarized_data", "HydroFIA_pH_SW_INTEGRAL_winter.csv"))
HF.discrete <- read_csv(here("Data/_summarized_data", "HydroFIA_pH_discrete_INTEGRAL_winter.csv"))


#### Plot results ####


HF.SW %>% 
  filter(date.time > ymd_h("2019-02-28T12")) %>% 
  ggplot()+
  #geom_rect(aes(xmax=ymd_h("2020-01-01T12"),xmin=ymd_h("2000-01-01T12"),ymax=7.55, ymin=-Inf), alpha=0.05)+
  geom_hline(yintercept = 7.55)+
  geom_path(aes(date.time, pH, col=instrument))+
  scale_x_datetime(date_labels = "%d.%b, %H:%M")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  labs(x="Date", y="pHT @ 25oC")+
  theme_bw()

ggsave(here("Plots/pH", "HydroFIA_pH_surface_seawater_INTEGRAL_winter.jpg"))

HF.SW %>% 
  filter(date.time > ymd_h("2019-02-28T12"),
         pH>7.55) %>% 
  ggplot()+
  geom_path(aes(date.time, pH, col=instrument))+
  scale_x_datetime(date_labels = "%d.%b, %H:%M")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  labs(x="Date", y="pHT @ 25oC")+
  theme_bw()

ggsave(here("Plots/pH", "HydroFIA_pH_surface_seawater_INTEGRAL_winter_755.jpg"), width = 12, height = 5)


HF %>% 
  filter(date.time > ymd_h("2019-02-28T12")) %>% 
  ggplot()+
  geom_path(aes(date.time, pHFitPoints))+
  scale_x_datetime(date_labels = "%d.%b, %H:%M")+
  #scale_color_brewer(palette = "Set1", name="Instrument")+
  labs(x="Date")+
  facet_wrap(~instrument)+
  theme_bw()

ggsave(here("Plots/pH", "HydroFIA_pH_pHFitPoints_seawater_INTEGRAL_winter.jpg"))

HF %>% 
  filter(pHError < 0.01,
         date.time > ymd_h("2019-02-28T12")) %>% 
  ggplot()+
  geom_path(aes(date.time, pHError))+
  scale_x_datetime(date_labels = "%d.%b, %H:%M")+
  #scale_color_brewer(palette = "Set1", name="Instrument")+
  labs(x="Date")+
  facet_wrap(~instrument)+
  theme_bw()

ggsave(here("Plots/pH", "HydroFIA_pH_pHError_seawater_INTEGRAL_winter.jpg"))

HF.discrete %>% 
  ggplot()+
  geom_point(aes(pH, dep, col=instrument))+
  geom_path(aes(pH, dep))+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  scale_y_reverse()+
  facet_wrap(~station)+
  labs(y="Depth (m)", x="pHT")+
  theme_bw()

ggsave(here("Plots/pH", "HydroFIA_pH_profiles_INTEGRAL_winter.jpg"),
       width = 8, height = 8)




#### Interactive plot with plotly ####

library(plotly)

ggplotly(
HF.SW %>% 
    filter(date.time > ymd_h("2019-02-28T12")) %>% 
    ggplot()+
    geom_path(aes(date.time, pH, col=instrument))+
    geom_point(data=HF.discrete[HF.discrete$dep==0,], aes(date.time, pH, fill=instrument), shape=21)+
    scale_x_datetime(date_labels = "%d.%b, %H:%M")+
    scale_color_brewer(palette = "Set1", name="Instrument")+
    scale_fill_brewer(palette = "Set1", name="Instrument")+
    labs(x="Date", y="pHT @ 25oC")+
    theme_bw()
)


ggplotly(
HF.SW %>% 
  filter(date.time > ymd_h("2019-02-28T12")) %>% 
  ggplot()+
  geom_path(aes(date.time, pH, linetype=instrument))+
  geom_point(aes(date.time, pH, col=temperatureSample))+
  scale_x_datetime(date_labels = "%d.%b, %H:%M")+
  scale_color_viridis_c()+
  labs(x="Date", y="pHT @ 25oC")+
  theme_bw()
)


ggplotly(
  
  HF.discrete %>% 
    ggplot()+
    geom_point(aes(pH, dep, col=instrument))+
    scale_color_brewer(palette = "Set1", name="Instrument")+
    scale_y_reverse()+
    facet_wrap(~station)+
    theme_bw()
)


HF.discrete <-
  HF.discrete %>% 
  arrange(date.time)
