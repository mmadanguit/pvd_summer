library(tidyverse)
library(sf)
library(tidycensus)
library(leaflet)
library(mapview)
# setwd("~/Documents/github/pvd_summer/")
# tripsPerTract is imported csv with all trips per tract data
trips <- tripsPerTract %>% select(c('TRACT','event_time','n')) 

dt <- as.data.frame(str_split_fixed(trips$event_time, " ", 2)) # separate date and time
# would be cool to get the day of the week and specify it in here
# some sort of calendar look up
trips <- trips %>% select(c('TRACT','n')) %>% cbind(DATE = dt$V1, TIME = dt$V2) # construct

time <- subset(trips, DATE > "2020-01-03" & DATE < "2020-01-10") # filter to one week
geoData <- readRDS("censusData/riDataGeo.Rds")
mapview(geoData) # map data needs GEOID, name, and geometry

# OLD
b <- as.timeDate(trips$event_time)