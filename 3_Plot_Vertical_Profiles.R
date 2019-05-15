# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Read merged profiles data sets ------------------------------------------

df_long <- read_csv(here::here("Data/_merged_data", "Vertical_profiles_long.csv"))
df <- read_csv(here::here("Data/_merged_data", "Vertical_profiles.csv"))



# Plot profiles -----------------------------------------------------------

df_long %>% 
  filter(parameter %in% c("CT_ship", "CT_lab"),
         !is.na(value)) %>% 
  ggplot(aes(value, dep, col=parameter))+
  geom_point()+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~station)+
  labs(x="CT (umol/kg)", y="Depth (m)")+
  theme_bw()

ggsave(here::here("Plots/Profiles", "CT_Profiles_Stations_AIRICA.jpg"), width = 9, height = 8)


df_long %>% 
  filter(parameter %in% c("PO4", "TP"),
         !is.na(value)) %>% 
  ggplot(aes(value, dep, col=parameter))+
  geom_point()+
  scale_y_reverse(breaks=seq(0,300,20))+
  facet_wrap(~station)+
  labs(x="concentration (umol/kg)", y="Depth (m)")+
  theme_bw()

ggsave(here::here("Plots/Profiles", "P_Profiles_Stations_SYKE.jpg"), width = 9, height = 8)


