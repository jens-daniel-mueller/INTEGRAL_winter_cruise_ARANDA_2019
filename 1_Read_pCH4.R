# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)


#### Read pCH4 data from HydroC IOW ####

HC.IOW <- read_csv2(here("Data/HydroC_IOW", "SD_datafile_20190310_150805CH4T-1118-001.txt"), 
                     skip = 5, 
                     col_names = c("Date","Time","Weekday","P_pump","p_TDLAS",
"p_in","I_total","U_supply","external_pump","internal_1",
"internal_2","Runtime","internal_3","internal_4","internal_5",
"internal_6","Conc_estimate","pCH4","xCH4","T_control","T_gas","%rH_gas","X23")) %>% 
  mutate(instrument = "HydroC_IOW") %>% 
  select(-X23) %>% 
  mutate(date.time = ymd_hms(paste(as.character(Date), as.character(Time)))) %>% 
  filter(date.time > ymd_h("2019-02-28T11"))



#### Read pCH4 data from IOW equilibrator system ####

setwd("C:/Mueller_Jens_Data/Projects/INTEGRAL/Aranda/INTEGRAL_winter_cruise_RV_ARANDA_2019_data_evaluation/Data/Equi_IOW")
files <- list.files(pattern = "*.WLD")

Equi <- files %>%
  map(read_csv, skip=60,
      col_names = c("Date","Time","CH4_CH4","CH4_CO2","CH4_H2O",
                              "N2O_CO","N2O_N2O","N2O_H2O","TEqui","TInlet",
                              "FlowSwEQ","FlowAirEQ","CH4_Press","CH4_Temp",
                              "N2O_Press","N2O_Temp", "Days","Secs")) %>%
  reduce(rbind) %>% 
  mutate(date.time = dmy_hms( paste(as.character(Date), as.character(Time)))) %>% 
  filter(date.time > ymd_h("2019-02-28T11"))


#### subset relevant columns and write summarized data files ####

HC.IOW <- 
  HC.IOW %>% 
  select("date.time","p_TDLAS","p_in",
         "T_control","Conc_estimate","pCH4","xCH4",
         "T_gas","%rH_gas")

Equi <-
  Equi %>%
  select("date.time","CH4_CH4","CH4_CO2","CH4_H2O",
         "N2O_CO","N2O_N2O","N2O_H2O","TEqui","TInlet",
         "FlowSwEQ","FlowAirEQ","CH4_Press","CH4_Temp",
         "N2O_Press","N2O_Temp") %>% 
  arrange(date.time)

#### Control plot ####

ggplot()+
  geom_path(data=HC.IOW, aes(date.time, pCH4, col="HydroC_IOW"))+
  geom_point(data=Equi, aes(date.time, CH4_CH4, col="Equi_IOW"))+
  labs(x="Date", y="pCH4 (?atm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw() +
  ylim(2,12)


write_csv(HC.IOW, here("Data/_summarized_data", "pCH4_HydroC_IOW_INTEGRAL_winter.csv"))
#write_csv(Equi, here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW_INTEGRAL_winter.csv"))







