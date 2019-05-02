library(data.table)
library(seacarb)
library(lubridate)
library(tidyverse)
library(here)


df.area.2019 <- read_csv(here("Data/_summarized_data", "Finnmaid_mean_area_CT_2019.csv"))
df.area <- read_csv(here("Data/Finnmaid", "df_mean_area_CT_2003-2018.csv"))

df.area <- rbind(df.area, df.area.2019)
df.area$day.date <- as.POSIXct(strptime(paste(2000,df.area$day), format = "%Y %j",tz="GMT"))

rm(df.area.2019)

df.area <-
  df.area %>% 
  filter(day<120,
       Area %in% c("4.EGS", "5.NGS", "6.WGF", "7.HGF"))


start <- as.POSIXct(strptime("2000/02/28", format = "%Y/%m/%d",tz="GMT"))
end   <- as.POSIXct(strptime("2000/03/11", format = "%Y/%m/%d",tz="GMT"))

shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(
  length(levels(as.factor(df.area$year)))-1
)), "red")


df.area %>%
  ggplot()+
  geom_vline(xintercept =  c(start, end))+
  geom_hline(yintercept=400)+
  geom_path(aes(day.date, mean.pCO2, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area)+
  labs(y=expression(pCO[2]~(µatm)), x="Date")+
  scale_x_datetime(date_labels = "%b", date_breaks = "month")+
  theme_bw()

ggsave(here("Plots/Finnmaid", "pCO2_vs_time_all_years_northern_subareas.jpg"))

df.area %>%
  ggplot()+
  geom_vline(xintercept =  c(start, end))+
  geom_path(aes(day.date, mean.Tem, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area)+
  labs(y="Tem (°C)", x="Date")+
  scale_x_datetime(date_labels = "%b", date_breaks = "month")+
  theme_bw()

ggsave(here("Plots/Finnmaid", "Tem_vs_time_all_years_northern_subareas.jpg"))



#### 2019 data - Regional distribution in northern parts of Central Baltic and GuF



df.2019 <- read_csv(here("Data/_summarized_data", "Finnmaid_2019.csv"))

df.2019 <-
  df.2019 %>% 
  filter(Lon>20)


shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(
  length(levels(as.factor(df.2019$ID)))-1
)), "red")


df.2019 %>% 
  ggplot(aes(Lon, pCO2, col=as.factor(ID)))+
  geom_hline(yintercept = 400)+
  geom_path()+
  labs(y=expression(pCO[2]~(µatm)))+
  scale_color_manual(values = shadesOfGreyRed, name="Date")+
  theme_bw()

ggsave(here("Plots/Finnmaid", "pCO2_vs_Lon_2019.jpg"))


df.2019 %>% 
  ggplot(aes(Lon, Tem, col=as.factor(ID)))+
  geom_path()+
  labs(y="Tem (degC)")+
  scale_color_manual(values = shadesOfGreyRed, name="Date")+
  theme_bw()
  
ggsave(here("Plots/Finnmaid", "Tem_vs_Lon_2019.jpg"))


