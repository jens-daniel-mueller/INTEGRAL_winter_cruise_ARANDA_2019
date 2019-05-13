# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)



# Read HydroC data (operated by FMI, Heidi Pettersson) --------------------


HC.FMI <- read_csv2(here("Data/Tracegases_Surface/HydroC_FMI", "SD_datafile_20190311_090007CO2-0412-009.txt"), 
                    skip = 5, 
                    col_names = c("Date","Time","Weekday","P_pump","p_NDIR","p_in","
                                   I_total","U_supply","Zero","Flush","external_pump","
                                   Runtime","Signal_raw","Signal_ref","T_sensor","
                                   Signal_proc","Conc_estimate","pCO2_corr","xCO2_corr","
                                   T_control","T_gas","%rH_gas","X23")) %>% 
  mutate(instrument = "HydroC_FMI") %>% 
  select(-X23) %>% 
  mutate(date_time = ymd_hms(paste(as.character(Date), as.character(Time)))) %>% 
  filter(Zero==0, Flush==0, date_time > ymd_h("2019-02-28T11"))

HC.FMI %>% 
  filter(pCO2_corr < 600) %>% 
  ggplot(aes(date_time, pCO2_corr))+
  geom_path()


# Read HydroC data (operated by IOW, Daniel Poenisch) ---------------------

HC.IOW <- read_csv2(here("Data/Tracegases_Surface/HydroC_IOW", "SD_datafile_20190311_094716CO2T-1118-004.txt"), 
                     skip = 5, 
                     col_names = c("Date","Time","Weekday","P_pump","p_NDIR","p_in","
                                   I_total","U_supply","Zero","Flush","external_pump","
                                   Runtime","Signal_raw","Signal_ref","T_sensor","
                                   Signal_proc","Conc_estimate","pCO2_corr","xCO2_corr","
                                   T_control","T_gas","%rH_gas","X23")) %>% 
  mutate(instrument = "HydroC_IOW") %>% 
  select(-X23) %>% 
  mutate(date_time = ymd_hms(paste(as.character(Date), as.character(Time)))) %>% 
  filter(Zero==0, Flush==0, date_time > ymd_h("2019-02-28T11"))

HC.IOW %>% 
  filter(pCO2_corr < 600) %>% 
  ggplot(aes(date_time, pCO2_corr))+
  geom_path()




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
  filter(CH4_CO2 < 600) %>% 
  ggplot(aes(date_time, CH4_CO2))+
  geom_path()



# subset relevant columns -------------------------------------------------

HC.FMI <- 
  HC.FMI %>% 
  select("date_time","p_NDIR","p_in","Zero","Flush",
         "T_sensor","Conc_estimate","pCO2_corr","xCO2_corr",
         "T_gas","%rH_gas")

HC.IOW <- 
  HC.IOW %>% 
  select("date_time","p_NDIR","p_in","Zero","Flush",
         "T_sensor","Conc_estimate","pCO2_corr","xCO2_corr",
         "T_gas","%rH_gas")

Equi <-
  Equi %>%
  select("date_time","CH4_CH4","CH4_CO2","CH4_H2O",
         "N2O_CO","N2O_N2O","N2O_H2O","TEqui","TInlet",
         "FlowSwEQ","FlowAirEQ","CH4_Press","CH4_Temp",
         "N2O_Press","N2O_Temp")



# Plots for quality control -----------------------------------------------



library(plotly)

ggplotly(
ggplot()+
  geom_path(data=HC.FMI, aes(date_time, pCO2_corr, col="HydroC_FMI"))+
  geom_path(data=HC.IOW, aes(date_time, pCO2_corr, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date_time, CH4_CO2, col="Equi_IOW"))+
  labs(x="Date", y="pCO2 (?atm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw()
)


ggplot()+
  geom_path(data=HC.FMI, aes(date_time, pCO2_corr, col="HydroC_FMI"))+
  geom_path(data=HC.IOW, aes(date_time, pCO2_corr, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date_time, CH4_CO2, col="Equi_IOW"))+
  labs(x="Date", y="pCO2 (?atm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  #ylim(400, 600)+
  theme_bw()


Equi %>% 
ggplot()+
  geom_point(aes(date_time, FlowSwEQ))+
  theme_bw()

Equi <- 
  Equi %>% 
  filter(FlowSwEQ >4)

Equi %>% 
ggplot()+
  geom_point(aes(date_time, TInlet-TEqui))+
  theme_bw()

Equi <- 
  Equi %>% 
  filter(TInlet-TEqui > -1,
         TInlet-TEqui < 0)



# Write summarized data files ---------------------------------------------

write_csv(HC.IOW, here("Data/_summarized_data", "pCO2_HydroC_IOW.csv"))
write_csv(HC.FMI, here("Data/_summarized_data", "pCO2_HydroC_FMI.csv"))
write_csv(Equi, here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW.csv"))

