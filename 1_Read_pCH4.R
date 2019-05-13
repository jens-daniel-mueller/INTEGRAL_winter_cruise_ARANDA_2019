# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)


# Read HydroC data (operated by IOW, Daniel Poenisch) ---------------------

HC.IOW <- read_csv2(here::here("Data/Tracegases_Surface/HydroC_IOW", "SD_datafile_20190311_103707CH4T-1118-001.txt"), 
                     skip = 5, 
                     col_names = c("Date","Time","Weekday","P_pump","p_TDLAS",
"p_in","I_total","U_supply","external_pump","internal_1",
"internal_2","Runtime","internal_3","internal_4","internal_5",
"internal_6","Conc_estimate","pCH4","xCH4","T_control","T_gas","%rH_gas","X23")) %>% 
  mutate(instrument = "HydroC_IOW") %>% 
  select(-X23) %>% 
  mutate(date_time = ymd_hms(paste(as.character(Date), as.character(Time)))) %>% 
  filter(date_time > ymd_h("2019-02-28T11"))



# Read data from headspace equilibrator system ----------------------------
# operated by IOW, Michael Glockzin
# operated by GEOMAR, Annette Kock

setwd("C:/Mueller_Jens_Data/Cruises/190228_Aranda_INTEGRAL_winter/INTEGRAL_winter_cruise_ARANDA_2019/Data/Tracegases_Surface/Equi_IOW/Raw _data_received_during_cruise")
files <- list.files(pattern = "*.WLD")

Equi <- files %>%
  map(read_csv, skip=60,
      col_names = c("Date","Time","CH4_CH4","CH4_CO2","CH4_H2O",
                    "N2O_CO","N2O_N2O","N2O_H2O","TEqui","TInlet",
                    "FlowSwEQ","FlowAirEQ","CH4_Press","CH4_Temp",
                    "N2O_Press","N2O_Temp", "Days","Secs")) %>%
  reduce(rbind) %>%
  mutate(date_time = dmy_hms( paste(as.character(Date), as.character(Time)))) %>%
  filter(date_time > ymd_h("2019-02-28T11")) %>%
  arrange(date_time)

rm(files)

Equi %>% 
  filter(CH4_CH4 < 10) %>% 
  ggplot(aes(date_time, CH4_CH4))+
  geom_path()


# Subset relevant columns and write summarized data files -----------------


HC.IOW <- 
  HC.IOW %>% 
  select("date_time","p_TDLAS","p_in",
         "T_control","Conc_estimate","pCH4","xCH4",
         "T_gas","%rH_gas")

Equi <-
  Equi %>%
  select("date_time","CH4_CH4","CH4_CO2","CH4_H2O",
         "N2O_CO","N2O_N2O","N2O_H2O","TEqui","TInlet",
         "FlowSwEQ","FlowAirEQ","CH4_Press","CH4_Temp",
         "N2O_Press","N2O_Temp") %>% 
  arrange(date_time)




# Control plot ------------------------------------------------------------

ggplot()+
  geom_path(data=HC.IOW, aes(date_time, pCH4, col="HydroC_IOW"))+
  geom_point(data=Equi, aes(date_time, CH4_CH4, col="Equi_IOW"))+
  labs(x="Date", y="pCH4 (?atm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw() +
  ylim(2,12)




# Write summarized data files ---------------------------------------------


write_csv(HC.IOW, here("Data/_summarized_data", "pCH4_HydroC_IOW.csv"))
#write_csv(Equi, here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW_INTEGRAL_winter.csv"))







