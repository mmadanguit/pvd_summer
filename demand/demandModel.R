library(tidyverse)
library(sf)
library(tidycensus)
library(leaflet)
library(mapview)
# setwd("~/Documents/github/pvd_summer/")
# tripsPerTract is imported csv with all trips per tract data
trips <- tripsPerTract %>% select(c('TRACT','event_time','n')) 

# CONSTRUCT TRIPS DATASET
dt <- as.data.frame(str_split_fixed(trips$event_time, " ", 2)) # separate date and time
trips <- trips %>% select(c('TRACT','n')) %>% cbind(DATE = dt$V1, TIME = dt$V2) 
trips$day <- weekdays(as.Date(trips$DATE))

# LOAD GEODATA AND SIMPLIFY TRACT #s
geoData <- readRDS("censusData/riDataGeo.Rds")
geoData$TRACT = as.double(geoData$NAME %>% str_remove_all("\\p{Letter}") %>% 
                            str_remove_all("\\N{Comma}") %>%
                            str_remove_all("\\h")) # white space
# geoData <- geoData[order(geoData$TRACT),] #reorder
geoData <- geoData %>% arrange(geoData$TRACT)

# DAILY DEMAND
# time <- subset(trips, DATE > "2020-01-03" & DATE < "2020-01-10") # filter by time period
demand <- trips %>% group_by(TRACT) %>% summarize(average = mean(n), standard_deviation=sd(n))
demandGeo <- (geoData %>% filter(as.logical(match(geoData$TRACT, demand$TRACT))))
demand$GEOMETRY <- demandGeo$geometry
demand$NAME <- demandGeo$NAME
demand <- st_as_sf(demand)

time <- trips %>% subset(DATE == "2020-01-07")
# mapview(Spatial) # map data needs GEOID, name, and geometry
mapview(demand, zcol = "average")


