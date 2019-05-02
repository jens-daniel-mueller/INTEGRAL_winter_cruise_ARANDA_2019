#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)

#library(seacarb)

#### Read CT Airica ####

df.all <- read_csv(here("Data/_merged_data", "CT_profiles_measured_and_calculated.csv"))
df <- read_csv(here("Data/_summarized_data", "CT_AIRICA_all_INTEGRAL_winter.csv"))



#### Plot results ####

df %>% 
  ggplot()+
  geom_path(aes(CT, dep))+
  geom_point(aes(CT, dep, fill=CV.abs), shape=21)+
  scale_y_reverse(breaks=seq(0,300,20))+
  scale_fill_viridis_c(name="Uncertainty")+
  facet_wrap(~station, labeller = label_both)+
  labs(x="CT (umol/kg)", y="Depth (m)")+
  theme_bw()

ggsave(here("Plots/CT", "CT_Profiles_Stations_AIRICA.jpg"), width = 9, height = 8)

df %>% 
  ggplot(aes(date, CV.abs, fill=CT))+
  geom_point(shape=21)+
  scale_fill_viridis_c(name="CT (umol/kg)")+
  labs(x="Date", y="Uncertainty (CV, umol/kg)")+
  theme_bw()

ggsave(here("Plots/CT", "CT_AIRICA_uncertainty_timeseries.jpg"), width = 6, height = 3)

df.all %>% 
  ggplot(aes(CT, dep, col=instrument))+
  geom_path()+
  geom_point()+
  scale_y_reverse(breaks=seq(0,300,100))+
  facet_wrap(~station)+
  scale_color_brewer(palette = "Set1", name="CT estimate")+
  labs(x="CT (umol/kg)", y="Depth (m)")+
  theme_bw()

ggsave(here("Plots/CT", "CT_Profiles_Stations_AIRICA_and_calculated.jpg"), width = 9.5, height = 8)

# df %>% 
#   ggplot()+
#   geom_path(aes(CT/sal, dep))+
#   geom_point(aes(CT/sal, dep, fill=CV.abs), shape=21)+
#   scale_y_reverse()+
#   scale_fill_viridis_c(name="Uncertainty")+
#   facet_wrap(~station, labeller = label_both)+
#   labs(x="CT (umol/kg)", y="Depth (m)")+
#   theme_bw()
# 
# ggsave(here("Plots/CT", "CT-per-Sal_Profiles_Stations.jpg"), width = 6, height = 8)

df %>% 
  ggplot()+
  geom_path(aes(sal, dep))+
  geom_point(aes(sal, dep))+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~station, labeller = label_both)+
  labs(x="Salinity", y="Depth (m)")+
  theme_bw()

ggsave(here("Plots/CT", "Sal_Profiles_Stations.jpg"), width = 6, height = 8)


