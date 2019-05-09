#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)


#### Read summarized data sets

HC.IOW <- read_csv(here("Data/_summarized_data", "pCO2_HydroC_IOW_INTEGRAL_winter.csv"))
HC.FMI <- read_csv(here("Data/_summarized_data", "pCO2_HydroC_FMI_INTEGRAL_winter.csv"))
Equi   <- read_csv(here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW_INTEGRAL_winter.csv"))


#### Plot timeseries from three sensor types ###

ggplot()+
  geom_path(data=HC.FMI, aes(date.time, pCO2_corr, col="HydroC_FMI"))+
  geom_path(data=HC.IOW, aes(date.time, pCO2_corr, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date.time, CH4_CO2, col="Equi_IOW"))+
  labs(x="Date", y="pCO2 (µatm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw()+
  ylim(420,550)


ggsave(here("Plots/pCO2", "pCO2_surface_seawater_INTEGRAL_winter.jpg"), width = 13, height = 5)
