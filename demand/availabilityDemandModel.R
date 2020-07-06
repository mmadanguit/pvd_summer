library(tidyverse)
library(sf)
library(mapview)
source("intervals.R")
# setwd("~/Documents/github/pvd_summer/demand")

clean <- function(df){
  "Selects only the location data needed"
  df <- df %>% select(-c(provider, vehicle_status, vehicle_status_reason, 
              device_type, areas, lat, lng)) %>% # remove unwanted columns
    arrange(TRACT) %>% group_by(TRACT) %>% 
    arrange(start_time, .by_group = TRUE) %>%
    filter(TRACT <= 37)
  return(df)
}

splitTimeCol <- function(df){
  "Splits the data columns into seperate day and time"
  start <- str_split_fixed(df$start_time, " ", 2)
  end <- str_split_fixed(df$end_time, " ", 2)
  df <- df %>% select(-c(start_time, end_time)) %>% 
    add_column(startDate = start[,1], endDate = end[,1], 
               startTime = start[,2], endTime = end[,2])
  return(df)
}

period <- as.character(
  seq(as.Date("2019-10-22"), as.Date("2019-11-8"), by = "day"))
locData <- locations_and_tract_and_rounded_latlong_from_oct18_oct19
locData <- clean(locData) %>% splitTimeCol %>%
  filter((startDate %in% period) & (endDate %in% period)) # restrict to period

# intervalData <- getIntervalData(locData, period)