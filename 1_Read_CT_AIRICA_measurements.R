#### load required packages ####
library(tidyverse)
library(lubridate)
library(here)
#library(seacarb)

#### Read CT Airica ####

# 20190301

df_01 <- read_csv(here::here("Data/CT_AIRICA_IOW", "CT_20190301.csv"))

df_01 <- df_01 %>% 
  filter(type == "bottle") %>% 
  mutate(date = ymd(paste(year, month, day))) %>% 
  filter(!(station == "Keri" & replicate == 1 & dep == 30)) %>% 
  group_by(station, dep) %>%
  filter(replicate == max(replicate)) %>%
  ungroup() %>%
  select(date, station, dep, sal, CT, CV.abs, CV.rel)


# 20190303 and later

df_03 <- read_tsv(here::here("Data/CT_AIRICA_IOW", "20190303_corr.dbs"))
df_05 <- read_tsv(here::here("Data/CT_AIRICA_IOW", "20190305.dbs"))
df_08 <- read_tsv(here::here("Data/CT_AIRICA_IOW", "20190308.dbs"))
df_09 <- read_tsv(here::here("Data/CT_AIRICA_IOW", "20190309.dbs"))

df_all <- bind_rows(df_03, df_05, df_08, df_09)
rm(df_03, df_05, df_08, df_09)

df_all <- df_all %>% 
  select(type=1,bottle=2,sal=8,
         CT=14,CV.rel=17,CV.abs=18) %>% 
  filter(type == "bottle") %>% 
  mutate(date = ymd(substr(bottle,1,10)),
         station = substr(bottle,12,15),
         dep = as.numeric(substr(bottle,17,19)),
         replicate = as.numeric(substr(bottle,21,21)),
         replicate = if_else(is.na(replicate), 1, replicate)) %>% 
  group_by(station, dep) %>%
  filter(replicate == max(replicate)) %>%
  ungroup() %>%
  select(date, station, dep, sal, CT, CV.rel, CV.abs) %>% 
  mutate(CV.rel = as.numeric(CV.rel),
         CV.abs = as.numeric(CV.abs))

df <-bind_rows(df_all, df_01) %>% 
  arrange(station, dep)

rm(df_all, df_01)


#### Plot results ####

df %>% 
  ggplot(aes(CT, dep, col=sal))+
  #geom_path()+
  geom_point()+
  scale_y_reverse()+
  scale_color_viridis_c()+
  facet_wrap(~station, scales = "free_x")+
  theme_bw()

df %>%
  ggplot(aes(sal,CT, col=dep))+
  geom_smooth(method = "lm", se=FALSE, fullrange=TRUE)+
  geom_point()+
  xlim(0,10)+
  ylim(0,2300)+
  scale_color_viridis_c()
  

write_csv(df, here("Data/_summarized_data", "CT_AIRICA_all_INTEGRAL_winter.csv"))
