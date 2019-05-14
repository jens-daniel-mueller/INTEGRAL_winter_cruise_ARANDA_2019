
# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(seacarb)


# Read HydroFIA pH profile data -------------------------------------------

HF.discrete <- read_csv(here::here("Data/_summarized_data", "pH_HydroFIA_discrete.csv"))



# CT calculation from measured pH and AT ----------------------------------
# AT calculated from Salinity according to Mueller et al. 2016

CT <-
  HF.discrete %>% 
  select(date_time,instrument,station,dep,sal,pHT) %>% 
  mutate(
    AT = 1610 +220.9*(sal-7) +5.1*(2019-1995) -0.6*(sal-7)*(2019-1995),
    CT = carb(flag=8, var1=pHT, var2=AT*1e-6, S=sal, T=25, k1k2="m10")$DIC*1e6)



CT %>% 
  ggplot()+
  geom_point(aes(CT, dep, col=instrument))+
  geom_path(aes(CT, dep))+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  scale_y_reverse()+
  facet_wrap(~station)+
  labs(x="CT (umol/kg)", y="Depth (m)")+
  theme_bw()


CT %>% 
  ggplot()+
  geom_point(aes(sal, dep, col=instrument))+
  geom_path(aes(sal, dep))+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  scale_y_reverse()+
  facet_wrap(~station)+
  labs(x="Salinity", y="Depth (m)")+
  theme_bw()



# Write summarized file ---------------------------------------------------

write_csv(CT, here("Data/_summarized_data", "CT_profiles_calculated_from_pH.csv"))


