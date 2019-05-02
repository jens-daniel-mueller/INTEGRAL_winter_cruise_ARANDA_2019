#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)

#library(seacarb)

#### original HydroFIA data files with pH calculated according to outdated Mosley et al (2004) ####

HF1 <- read_csv(here::here("Data/HydroFIApH/PH-0218-001", "data.txt"), skip = 2) %>% 
  mutate(instrument = "1_PH-0218-001")

HF3 <- read_csv(here::here("Data/HydroFIApH/PH-1017-001", "data.txt"), skip = 2) %>% 
  mutate(instrument = "3_PH-1017-001")

HF <- bind_rows(HF1, HF3)
rm(HF1, HF3)


names(HF) <- c("timestamp","measurementCounter","sampleName","action",
"inlet","salinity","pH","pHFitPoints","pHError",
"pHReference","invalidSalinity","invalidTemperature",
"invalidpH","spectrumQuality","emptyAcid","emptyIndicator",
"pHAdjust","spectrumIntegrationTime","temperatureSample",
"temperatureBox","temperatureSampleC","temperatureBoxC","serialAcid",
"serialIndicator","absorbance434","absorbance487","absorbance578","systemUptime",
"samplePump","circuitValve","injectionValve","inletValve","acidPump","indicatorPump",
"acidCartridgeLevel","indicatorCartridgeLevel", "instrument")

HF <-
  HF %>% 
  select(date.time = "timestamp","sampleName","action","salinity","pH","pHFitPoints","pHError",
         "temperatureSample","serialAcid","serialIndicator",
         "absorbance434","absorbance487","absorbance578", "instrument") %>% 
  filter(date.time > ymd_h("2019-02-28T12"),
         pH < 8)


#### subset data from continous surface water measurements ####

HF.SW <-
HF %>% 
  mutate(type = substr(sampleName,1,2)) %>% 
  filter(type == "SW")

#### subset data from discrete profile samples ####

HF.discrete <-
HF %>% 
  mutate(type = substr(sampleName,1,2)) %>% 
  filter(type != "SW") %>% 
  mutate(station = substr(sampleName,1,4),
         dep = as.numeric(substr(sampleName,5,7))) 
  



# HF <-
#   HF %>% 
#   mutate(AT = (salinity/35)*2300e-6,
#          pH.Mueller.25 = pHinsi(pH = pH, ALK = AT, Tinsi = 25, Tlab = temperatureSample))


#### Plot results and cleaning of errorneous measurements ####

library(plotly)

#ggplotly(
HF.SW %>% 
  #filter(pH>7.55) %>% 
  ggplot()+
  geom_path(aes(date.time, pH, col=instrument))+
  scale_x_datetime(date_labels = "%d.%b, %H:%M")+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  labs(x="Date", y="pHT @ 25oC")+
  theme_bw()
#)

HF.SW <-
  HF.SW %>% 
  filter(pH<7.8)


HF.discrete %>% 
  ggplot()+
  geom_point(aes(pH, dep, col=instrument))+
  scale_color_brewer(palette = "Set1", name="Instrument")+
  scale_y_reverse()+
  facet_wrap(~station)+
  theme_bw()

HF.discrete <-
  HF.discrete %>% 
  group_by(station, dep) %>% 
  slice(tail(row_number(), 3)) %>%  
  ungroup() %>% 
  filter(!(pH == 7.295 & station == "LL12")) %>% 
  filter(!(station %in% c("OUL3","OUL4","JUNK","TRIS","BOX-")))



write_csv(HF, here("Data/_summarized_data", "HydroFIA_pH_all_INTEGRAL_winter.csv"))
write_csv(HF.SW, here("Data/_summarized_data", "HydroFIA_pH_SW_INTEGRAL_winter.csv"))
write_csv(HF.discrete, here("Data/_summarized_data", "HydroFIA_pH_discrete_INTEGRAL_winter.csv"))
