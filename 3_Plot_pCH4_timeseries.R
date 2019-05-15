# Packages ----------------------------------------------------------------

library(tidyverse)

#### Read summarized pCH4 data sets ####

HC.IOW <- read_csv(here::here("Data/_summarized_data", "pCH4_HydroC_IOW.csv"))
Equi   <- read_csv(here::here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW.csv"))


#### Plot pCH4 timeseries from two sensors ####

ggplot()+
  geom_path(data=HC.IOW, aes(date_time, pCH4, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date_time, CH4_ppm, col="Equi_IOW"))+
  labs(x="Date", y="pCH4 (uatm)", title = "pCH4 range limited to 1-12 uatm")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw()+
  ylim(1,12)


ggsave(here::here("Plots/pCH4", "pCH4_surface_seawater_INTEGRAL_winter.jpg"), width = 13, height = 5)

