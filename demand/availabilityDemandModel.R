library(tidyverse)
library(sf)
library(mapview)
source("intervals.R")
# setwd("~/Documents/github/pvd_summer/demand")

# import data
locData <- locations_and_tract_18_09_01_to_19_11_01 %>% # load
  select(-c(provider, vehicle_status, vehicle_status_reason, 
            device_type, areas, lat, lng)) %>% # remove unwanted columns
  arrange(TRACT) %>% group_by(TRACT) %>% arrange(start_time, .by_group = TRUE) %>%
  filter(TRACT <= 37)# restrict to our TRACTS

# split date and time
start <- str_split_fixed(locData$start_time, " ", 2)
end <- str_split_fixed(locData$end_time, " ", 2)
locData <- locData %>% select(-c(start_time, end_time)) %>% 
  add_column(startDate = start[,1], endDate = end[,1], 
             startTime = start[,2], endTime = end[,2])
period <- as.character(
  seq(as.Date("2019-10-22"), as.Date("2019-11-8"), by = "day"))
intervalData <- getIntervalData(locData, period)