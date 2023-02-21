devtools::load_all("../wqtrends", helpers = F)
# library(wqtrends)
library(tidyverse)
library(lubridate)
library(tbeptools)

tomod <- epcdata %>% 
  select(
    segment = bay_segment, 
    station = epchc_station, 
    date = SampleTime, 
    yr, 
    tn, 
    chla
  ) %>% 
  mutate(
    date = as.Date(date), 
    doy = yday(date), 
    cont_year = decimal_date(date), 
    mo = month(date, label = T)
  ) %>% 
  pivot_longer(cols = c(tn, chla), names_to = 'param', values_to = 'value') %>% 
  na.omit() %>% 
  filter(yr > 2001) %>% 
  arrange(station, date)

tbtnmod <- tomod %>% 
  filter(param == 'tn') %>% 
  group_by(segment, station) %>% 
  nest() %>% 
  mutate(
    mod = purrr::pmap(list(station, data), function(station, data){
      
      cat(station, '\n')
      
      anlz_gam(data, trans = 'log10')
      
    })
  )
  