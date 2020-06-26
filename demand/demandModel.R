library(tidyverse)
library(sf)
library(tidycensus)
library(leaflet)
library(mapview)
# setwd("~/Documents/github/pvd_summer/")
# tripsPerTract is imported csv with all trips per tract data
trips <- tripsPerTract %>% select(c('TRACT','event_time','n')) 

# CONSTRUCT TRIPS INFO
dt <- as.data.frame(str_split_fixed(trips$event_time, " ", 2)) # separate date and time
trips <- trips %>% select(c('TRACT','n')) %>% cbind(DATE = dt$V1, TIME = dt$V2) %>%
  filter(TRACT <= 37) %>% group_by(TRACT)
trips$day <- weekdays(as.Date(trips$DATE))

# LOAD GEODATA AND SIMPLIFY TRACT #s
geoData <- readRDS("censusData/riDataGeo.Rds")
geoData$TRACT = as.double(geoData$NAME %>% str_remove_all("\\p{Letter}") %>% 
                            str_remove_all("\\N{Comma}") %>%
                            str_remove_all("\\h")) # white space
geoData <- geoData %>% arrange(geoData$TRACT)

# DAILY DEMAND
# time <- subset(trips, DATE > "2020-01-03" & DATE < "2020-01-10") # filter by time period
demand <- trips %>% subset(DATE > "2019-05-01" & DATE < "2019-09-30") %>% 
  summarize(avgYear=mean(n), stdYear=sd(n)) #  whole period
demand <- demand %>% mutate(
  trips %>% subset(DATE > "2019-05-01" & DATE < "2019-06-30") %>% # spring
    summarize(avgSpring=mean(n), stdSpring=sd(n)),
  trips %>% subset(DATE > "2019-05-01" & DATE < "2019-06-30") %>% # summer
    summarize(avgSpring=mean(n), stdSpring=sd(n)),
  trips %>% subset(DATE > "2019-05-01" & DATE < "2019-06-30") %>% # fall
    summarize(avgSpring=mean(n), stdSpring=sd(n))) 
demandGeo <- geoData %>% filter(as.logical(match(geoData$TRACT, demand$TRACT)))
demand$GEOMETRY <- demandGeo$geometry
demand$NAME <- demandGeo$NAME
demand <- st_as_sf(demand)

time <- trips %>% subset(DATE == "2020-01-07")
# mapview(Spatial) # map data needs GEOID, name, and geometry
mv <- mapview(demand, zcol = "avgYear")


