# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(zoo)


# Read summarized data files ----------------------------------------------

# Read HydroFIA pH surface data for two instruments
HF_SW_1017 <- read_csv(here::here("Data/_summarized_data", "pH_HydroFIA_SW_1017.csv")) %>% 
  select(1,4) %>% 
  rename(pHT_0117 = pHT)

HF_SW_0218 <- read_csv(here::here("Data/_summarized_data", "pH_HydroFIA_SW_0218.csv")) %>% 
  select(1,4) %>% 
  rename(pHT_0218 = pHT)


# Read summarized equilibrator data
Equi   <- read_csv(here::here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW.csv")) %>% 
  select(1:5) %>% 
  rename(pCH4_Equi = CH4_ppm,
         pCO2_Equi = CO2_ppm,
         pCO_Equi = CO_ppm,
         pN20_Equi = N2O_ppm)


# HydroC pCO2 and pCH4 data
HC_pCH4_IOW   <- read_csv(here::here("Data/_summarized_data", "pCH4_HydroC_IOW.csv")) %>% 
  select(1, 6) %>% 
  rename(pCH4_HC = pCH4)

HC_pCO2_IOW   <- read_csv(here::here("Data/_summarized_data", "pCO2_HydroC_IOW.csv")) %>% 
  select(24, 18) %>% 
  rename(pCO2_HC_IOW = pCO2_corr)

HC_pCO2_FMI   <- read_csv(here::here("Data/_summarized_data", "pCO2_HydroC_FMI.csv")) %>% 
  select(24, 18) %>% 
  rename(pCO2_HC_FMI = pCO2_corr)


# Read summarized track and surface TS datasets

track <- read_csv( here("Data/_summarized_data", "GPS_Aranda.csv"))
TS    <- read_csv( here("Data/_summarized_data", "TS_surface.csv"))



# Merge data sets ---------------------------------------------------------

df <- full_join(track, TS)
df <- full_join(df, Equi)
df <- full_join(df, HC_pCH4_IOW)
df <- full_join(df, HC_pCO2_FMI)
df <- full_join(df, HC_pCO2_IOW)
df <- full_join(df, HF_SW_0218)
df <- full_join(df, HF_SW_1017)
df <- full_join(df, HF_SW_1017)



# Interpolate data to common time stamps ----------------------------------

# GPS interpolation

df <- df %>% 
  mutate(Lat = na.approx(Lat, na.rm = FALSE),
         Lon = na.approx(Lon, na.rm = FALSE),
         SSS = na.approx(SSS, na.rm = FALSE),
         SST = na.approx(SST, na.rm = FALSE))



# Summarize data by grouping into time intervals --------------------------

df_sum <- df %>% 
  mutate(date_time_int = cut(date_time, "5 min")) %>% 
  select(-date_time) %>% 
  group_by(date_time_int) %>% 
  summarise_all("mean", na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(date_time_int = ymd_hms(date_time_int))

df_sum %>% 
  ggplot(aes(date_time_int, pCO2_Equi))+
  geom_path()



# Write merged data files -------------------------------------------------

write_csv(df, here("Data/_merged_data", "Surface_data_full_resolution.csv"))
write_csv(df_sum, here("Data/_merged_data", "Surface_data_5min_resolution.csv"))
