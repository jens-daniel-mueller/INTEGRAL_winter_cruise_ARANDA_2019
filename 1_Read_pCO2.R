#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)


#### Read HydroC data (operated by FMI, Heidi Pettersson) ####

setwd("C:/Mueller_Jens_Data/Projects/INTEGRAL/Aranda/INTEGRAL_winter_cruise_RV_ARANDA_2019_data_evaluation/Data/HydroC_FMI")
files <- list.files(pattern = "*.txt")

for (file in files){
  
HC.FMI <-
  read_csv2(here("Data/HydroC_FMI", file), 
                     skip = 4, 
                     col_names = c("Date","Time","Weekday","P_pump","p_NDIR","p_in","
                                   I_total","U_supply","Zero","Flush","external_pump","
                                   Runtime","Signal_raw","Signal_ref","T_sensor","
                                   Signal_proc","Conc_estimate","pCO2_corr","xCO2_corr","
                                   T_control","T_gas","%rH_gas","X23")) %>% 
  mutate(instrument = "HydroC_Syke") %>% 
  select(-X23) %>% 
  mutate(date.time = dmy_hms(paste(Date, as.character(Time)))) %>% 
  filter(Zero==0, Flush==0) %>% 
  filter(date.time > ymd_h("2019-02-28T11"))
 
if(exists("temp"))  {temp <- bind_rows(temp, HC.FMI)}
if(!exists("temp")) {temp <- HC.FMI}
}

rm(HC.FMI, file, files)
HC.FMI <-temp
rm(temp)


#### Read HydroC data (operated by IOW, Daniel Pönisch) ####

HC.IOW <- read_csv2(here("Data/HydroC_IOW", "SD_datafile_20190310_142404CO2T-1118-004.txt"), 
                     skip = 5, 
                     col_names = c("Date","Time","Weekday","P_pump","p_NDIR","p_in","
                                   I_total","U_supply","Zero","Flush","external_pump","
                                   Runtime","Signal_raw","Signal_ref","T_sensor","
                                   Signal_proc","Conc_estimate","pCO2_corr","xCO2_corr","
                                   T_control","T_gas","%rH_gas","X23")) %>% 
  mutate(instrument = "HydroC_IOW") %>% 
  select(-X23) %>% 
  mutate(date.time = ymd_hms(paste(as.character(Date), as.character(Time)))) %>% 
  filter(Zero==0, Flush==0, date.time > ymd_h("2019-02-28T11"))



#### Read tracegases from headspace equilibrator system (operated by IOW, Michael Glockzin) ####

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
  filter(date.time > ymd_h("2019-02-28T11")) %>% 
  arrange(date.time)

rm(files)

#### subset relevant columns and write summarized data files ####

HC.FMI <- 
  HC.FMI %>% 
  select("date.time","p_NDIR","p_in","Zero","Flush",
         "T_sensor","Conc_estimate","pCO2_corr","xCO2_corr",
         "T_gas","%rH_gas")

HC.IOW <- 
  HC.IOW %>% 
  select("date.time","p_NDIR","p_in","Zero","Flush",
         "T_sensor","Conc_estimate","pCO2_corr","xCO2_corr",
         "T_gas","%rH_gas")

Equi <-
  Equi %>%
  select("date.time","CH4_CH4","CH4_CO2","CH4_H2O",
         "N2O_CO","N2O_N2O","N2O_H2O","TEqui","TInlet",
         "FlowSwEQ","FlowAirEQ","CH4_Press","CH4_Temp",
         "N2O_Press","N2O_Temp")


library(plotly)

ggplotly(
ggplot()+
  geom_path(data=HC.FMI, aes(date.time, pCO2_corr, col="HydroC_FMI"))+
  geom_path(data=HC.IOW, aes(date.time, pCO2_corr, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date.time, CH4_CO2, col="Equi_IOW"))+
  labs(x="Date", y="pCO2 (µatm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  theme_bw()
)


ggplot()+
  geom_path(data=HC.FMI, aes(date.time, pCO2_corr, col="HydroC_FMI"))+
  geom_path(data=HC.IOW, aes(date.time, pCO2_corr, col="HydroC_IOW"))+
  geom_path(data=Equi, aes(date.time, CH4_CO2, col="Equi_IOW"))+
  labs(x="Date", y="pCO2 (µatm)")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  #ylim(400, 600)+
  theme_bw()


Equi %>% 
ggplot()+
  geom_point(aes(date.time, FlowSwEQ))+
  theme_bw()

Equi <- 
  Equi %>% 
  filter(FlowSwEQ >4)

Equi %>% 
ggplot()+
  geom_point(aes(date.time, TInlet-TEqui))+
  theme_bw()

Equi <- 
  Equi %>% 
  filter(TInlet-TEqui > -1,
         TInlet-TEqui < 0)




write_csv(HC.IOW, here("Data/_summarized_data", "pCO2_HydroC_IOW_INTEGRAL_winter.csv"))
write_csv(HC.FMI, here("Data/_summarized_data", "pCO2_HydroC_FMI_INTEGRAL_winter.csv"))
write_csv(Equi, here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW_INTEGRAL_winter.csv"))

