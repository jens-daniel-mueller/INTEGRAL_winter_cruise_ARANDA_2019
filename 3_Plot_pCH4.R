#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)


#### Read summarized pCH4 data sets ####

HC.IOW <- read_csv(here("Data/_summarized_data", "pCH4_HydroC_IOW_INTEGRAL_winter.csv"))
Equi   <- read_csv(here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW_INTEGRAL_winter.csv"))


#### Plot pCH4 timeseries from two sensors ####

ggplot()+
  geom_path(data=HC.IOW, aes(date.time, pCH4, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date.time, CH4_CH4, col="Equi_IOW"))+
  labs(x="Date", y="pCH4 (µatm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw()+
  ylim(1,12)


ggsave(here("Plots/pCH4", "pCH4_surface_seawater_INTEGRAL_winter.jpg"), width = 13, height = 5)

#### Interactive plot with plotly ####

library(plotly)

ggplotly(
ggplot()+
  geom_path(data=HC.IOW, aes(date.time, pCH4, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date.time, CH4_CH4, col="Equi_IOW"))+
  labs(x="Date", y="pCH4 (µatm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw()
)

