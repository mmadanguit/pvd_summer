library(tidyverse)
library(sf)
library(mapview)

# import data
locData <- locations_and_tract_18_09_01_to_19_11_01 %>% # load
  select(-c(provider, vehicle_status, vehicle_status_reason, 
            device_type, areas, lat, lng)) %>% # remove unwanted columns
  arrange(TRACT) %>% group_by(TRACT) %>% arrange(start_time, .by_group = TRUE) %>%
  filter(TRACT <= 37) # restrict to our TRACTS
days <- as.character(seq(as.Date("2019/1/1 00:00:00"), by = "day", length.out = 365)) # data for a whole year