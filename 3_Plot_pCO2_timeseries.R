# Packages ----------------------------------------------------------------

library(tidyverse)

# Read summarized data sets -----------------------------------------------

HC.IOW <- read_csv(here::here("Data/_summarized_data", "pCO2_HydroC_IOW.csv"))
HC.FMI <- read_csv(here::here("Data/_summarized_data", "pCO2_HydroC_FMI.csv"))
Equi   <- read_csv(here::here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW.csv"))



#  Plot timeseries from three sensor types --------------------------------

ggplot()+
  geom_path(data=HC.FMI, aes(date_time, pCO2_corr, col="HydroC_FMI"))+
  geom_path(data=HC.IOW, aes(date_time, pCO2_corr, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date_time, CO2_ppm, col="Equi_IOW"))+
  labs(x="Date", y="pCO2 (uatm)", title = "pCO2 range limited to 420-550 uatm")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw()+
  ylim(420,550)


ggsave(here::here("Plots/pCO2", "pCO2_surface_seawater_INTEGRAL_winter.jpg"), width = 23, height = 5)
