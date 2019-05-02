#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)
library(zoo)

# Part 1: Merge surface data from continous water supply
# Part 1: Merge profiling data from CTD casts

### Surface Data ####

#### Read HydroFIA pH surface data ####

HF.SW <- read_csv(here("Data/_summarized_data", "HydroFIA_pH_SW_INTEGRAL_winter.csv"))

#### Read summarized equilibrator data ####

Equi   <- read_csv(here("Data/_summarized_data", "pCO2_pCH4_Equilibrator_IOW_INTEGRAL_winter.csv"))


#### Read summarized track and surface TS datasets ####

track <- read_csv( here("Data/_summarized_data", "Track_INTEGRAL_winter.csv"))
TS    <- read_csv( here("Data/_summarized_data", "TS_surface.csv"))


#### Merge Track, TS and Equilibrator data ####

df <- merge(track, TS, all = TRUE)
df <- merge(df, Equi, all = TRUE)
df <- merge(df, HF.SW, all = TRUE)

#### Interpolate track data to times of other observations ####

df <-
df %>% 
  mutate(Lat = na.approx(Lat, na.rm = FALSE),
         Lon = na.approx(Lon, na.rm = FALSE))



write_csv(df, here("Data/_merged_data", "Track_TS_Equi_pH.csv"))



### Profile Data ####

#### Read CT data calculated from discrete HydroFIA pH measurements ####

CT_calc <- read_csv(here("Data/_summarized_data", "CT_profiles_calculated_from_pH_all_INTEGRAL_winter.csv"))

CT_calc <-
  CT_calc %>% 
  select(station, dep, CT, instrument) %>% 
  mutate(instrument = "HydroFIA pH + AT-S")


#### Read CT Airica ####

CT <- read_csv(here("Data/_summarized_data", "CT_AIRICA_all_INTEGRAL_winter.csv"))

CT <- CT %>% 
  select(station, dep, CT) %>% 
  mutate(instrument="AIRICA")

CT_all <- bind_rows(CT, CT_calc)


write_csv(CT_all, here("Data/_merged_data", "CT_profiles_measured_and_calculated.csv"))

