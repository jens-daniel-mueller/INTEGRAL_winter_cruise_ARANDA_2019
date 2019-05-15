# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Read summarized HydroFIA pH surface data --------------------------------

HF.SW <- read_csv(here::here("Data/_summarized_data", "pH_HydroFIA_SW_both.csv"))


# Plot timeseries ---------------------------------------------------------

HF.SW %>% 
  filter(pHT > 7.4) %>% 
  ggplot()+
  geom_path(aes(date_time, pHT, col=instrument))+
  scale_x_datetime(date_labels = "%d.%b, %H:%M")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  labs(x="Date", y="pHT @ 25oC")+
  theme_bw()

ggsave(here::here("Plots/pH", "HydroFIA_pH_surface_seawater_INTEGRAL_winter.jpg"),
       width = 15, height = 5)


# HF %>% 
#   filter(date.time > ymd_h("2019-02-28T12")) %>% 
#   ggplot()+
#   geom_path(aes(date.time, pHFitPoints))+
#   scale_x_datetime(date_labels = "%d.%b, %H:%M")+
#   #scale_color_brewer(palette = "Set1", name="Instrument")+
#   labs(x="Date")+
#   facet_wrap(~instrument)+
#   theme_bw()
# 
# ggsave(here("Plots/pH", "HydroFIA_pH_pHFitPoints_seawater_INTEGRAL_winter.jpg"))
# 
# HF %>% 
#   filter(pHError < 0.01,
#          date.time > ymd_h("2019-02-28T12")) %>% 
#   ggplot()+
#   geom_path(aes(date.time, pHError))+
#   scale_x_datetime(date_labels = "%d.%b, %H:%M")+
#   #scale_color_brewer(palette = "Set1", name="Instrument")+
#   labs(x="Date")+
#   facet_wrap(~instrument)+
#   theme_bw()
# 
# ggsave(here("Plots/pH", "HydroFIA_pH_pHError_seawater_INTEGRAL_winter.jpg"))
# 
# HF.discrete %>% 
#   ggplot()+
#   geom_point(aes(pH, dep, col=instrument))+
#   geom_path(aes(pH, dep))+
#   scale_color_brewer(palette = "Set1", name="Instrument")+
#   scale_y_reverse()+
#   facet_wrap(~station)+
#   labs(y="Depth (m)", x="pHT")+
#   theme_bw()
# 
# ggsave(here("Plots/pH", "HydroFIA_pH_profiles_INTEGRAL_winter.jpg"),
#        width = 8, height = 8)
