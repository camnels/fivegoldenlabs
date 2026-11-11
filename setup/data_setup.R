Data prep for Lab 5

library(tidyverse)

vdem<-read_csv("setup/V-Dem-CY-Core-v16/V-Dem-CY-Core-v16.csv")
glimpse(vdem)

vdem_sel<-vdem %>%
  select(country_name:year,v2x_polyarchy) %>%
  filter(year %in% c(2000,2020))
table(vdem_sel$year)


oecd<-read_csv("setup/OECD.ECO.MAD,DSD_EPS@DF_EPS,1.0,filtered,2026-10-20 19-50-56.csv")
glimpse(oecd)

oecd_sel<-oecd %>%
  filter(TIME_PERIOD %in% c(2000,2020)) %>%
  select(REF_AREA,`Reference area`,TIME_PERIOD,OBS_VALUE) %>%
  rename(country_text_id=REF_AREA,
         country_name=`Reference area`,
         year=TIME_PERIOD,
         oecd_climpol_stringent=OBS_VALUE) 

wdi<-read_csv("setup/P_Data_Extract_From_World_Development_Indicators/cb7473fa-ad6d-4c66-9e37-4d506052d954_Data.csv")
glimpse(wdi)

wdi_sel<-wdi %>%
  pivot_longer(c(`2000 [YR2000]`,`2020 [YR2020]`),names_to="year",values_to="measure") %>%
  mutate(year=as.numeric(substr(year,1,4))) %>%
  mutate(wdi_var=case_when(str_detect(`Series Name`,"Carbon")~"co2_emissions",
                           str_detect(`Series Name`,"GDP")~"gdp",
                           str_detect(`Series Name`,"Population")~"tot_population")) %>%
  select(`Country Name`,`Country Code`,year,wdi_var,measure) %>%
  rename(country_name=`Country Name`,
         country_text_id=`Country Code`) %>%
  group_by(country_name,country_text_id,year,wdi_var) %>%
  filter(measure!="..") %>%
  summarise(measure=as.numeric(first(measure))) %>%
  pivot_wider(names_from=wdi_var,values_from=measure)

all_vars<-oecd_sel %>%
  inner_join(wdi_sel) %>%
  inner_join(vdem_sel) %>%
  select(-country_id)
glimpse(all_vars)
