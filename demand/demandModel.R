library(tidyverse)
library(sf)
library(tidycensus)
library(leaflet)
library(mapview)
# setwd("~/Documents/github/pvd_summer/")
# tripsPerTract is imported csv with all trips per tract data
trips <- tripsPerTract %>% select(c('TRACT','event_time','n')) 

# CONSTRUCT DATASET
dt <- as.data.frame(str_split_fixed(trips$event_time, " ", 2)) # separate date and time
trips <- trips %>% select(c('TRACT','n')) %>% cbind(DATE = dt$V1, TIME = dt$V2) # construct
trips$day <- weekdays(as.Date(trips$DATE))

# time <- subset(trips, DATE > "2020-01-03" & DATE < "2020-01-10") # filter by time period
time <- trips %>% subset(DATE == "2020-01-07")
geoData <- readRDS("censusData/riDataGeo.Rds")
mapview(geoData) # map data needs GEOID, name, and geometry