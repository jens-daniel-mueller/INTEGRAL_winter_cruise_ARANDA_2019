# Packages ----------------------------------------------------------------

library(tidyverse)
library(zoo)


# Read summarized profile data sets ---------------------------------------


# CT calculated from HydroFIA pH
CT_calc <- read_csv(here::here("Data/_summarized_data", "CT_profiles_calculated_from_pH.csv"))
CT_calc <- CT_calc %>% 
  select(3:8) %>% 
  rename(sal_pH = sal,
         AT_calc = AT,
         CT_calc = CT)


# CT Airica
CT <- read_csv(here::here("Data/_summarized_data", "CT_AIRICA_IOW.csv"))
CT <- CT %>% 
  select(9, 2:6) %>% 
  rename(sal_CT = sal)


# CTD Profiles
CTD <- read_csv(here::here("Data/_summarized_data", "CTD_profiles.csv"))
CTD <- CTD %>% 
  select(-c(4,9)) %>% 
  rename(station = Station)

# Nutrients
nuts <- read_csv(here::here("Data/_summarized_data", "Nutrient_Profiles_Syke.csv"))
nuts <- nuts %>% 
  select(-c(2:5)) %>% 
  rename(station = Station,
         tem_nuts = tem,
         sal_nuts = sal)
  
# Merge data sets ---------------------------------------------------------

df <- full_join(CT, CT_calc)
df <- full_join(df, CTD)
df <- full_join(df, nuts)

rm(CT, CT_calc, CTD, nuts)


df_clean <- df %>% 
  filter(!(station %in% c("BOX", "ICE", "OUL1", "OUL2", "OUL3", "OUL4"))) %>% 
  mutate(station = str_remove_all(station, "-"),
         station = if_else(station == "BO1", "B01", station),
         station = if_else(station == "BO3", "B03", station),
         station = if_else(station == "Keri", "KERI", station),
         station = if_else(station == "UT0", "UTO", station),
         station = if_else(station == "UTÖ_CTD", "UTO", station),
         station = if_else(station == "UTO_S", "UTO", station),)

# Plots for quality control -----------------------------------------------

df_long <- df_clean %>% 
  gather(key = "parameter", value = "value", c(3:10,13:28))


df_long %>% 
  filter(parameter %in% c("sal_CT", "sal_pH", "sal", "sal_nuts")) %>% 
  ggplot(aes(value, dep, col=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(~station)

df_long %>% 
  filter(parameter %in% c("CT_lab", "CT_ship", "CT_calc", "AT_calc")) %>% 
  ggplot(aes(value, dep, col=parameter))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(~station)

df_long %>% 
  filter(parameter %in% c("PO4")) %>% 
  ggplot(aes(value, dep, col=station))+
  geom_point()+
  scale_y_reverse()

df_clean %>% 
  ggplot(aes(Lon, Lat, col= station))+
  geom_point()




# Write merged data files -------------------------------------------------

write_csv(df_clean, here::here("Data/_merged_data", "Vertical_profiles.csv"))
write_csv(df_long, here::here("Data/_merged_data", "Vertical_profiles_long.csv"))

