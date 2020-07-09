library(tidyverse)
library(sf)
library(mapview)
setwd("~/Documents/github/pvd_summer/") # change to your WD
fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"

startD <<- "2019-12-01"
endD <<- "2019-12-31"

getAvail <- function(fol){
  "Open, filter, and clean availability data"
  availIntervals <- read_csv(paste(c(fol, "availIntervals2019.csv"), collapse='')) %>%
    # bind_rows(read_csv(paste(c(fol, "pickupsSummary2018.csv"), collapse=''))) %>%
    filter(DATE >= startD & DATE <= endD) %>%
    group_by(TRACT) %>% 
    arrange(DATE, .by_group=TRUE)
  availTime <- availIntervals %>% 
    select(c(TRACT, DATE, AVAIL)) %>% # time w/o intervals
    group_by(DATE, .add = TRUE) %>% 
    summarize(AVAIL, AVAIL = sum(AVAIL)) %>% # find total avail for day
    distinct()
  return(availTime)
}

getPickups <- function(fol){
  pickups <- read_csv(paste(c(fol, "pickupsSummary2018.csv"), collapse='')) %>%
    bind_rows(read_csv(paste(c(fol, "pickupsSummary2019.csv"), collapse=''))) %>%
    filter(DATE >= startD & DATE <= endD) %>%
    select(-c(X1)) %>% # remove column of index markers
    group_by(TRACT) %>% 
    arrange(DATE, .by_group=TRUE)
  return(pickups)
}

dailyAvg <- function(demand){
  "Find daily average for trip data over period of time.
  demand: demand dataframe
  start: start date
  end: end date
  names: vector of names for columns"
  demand <- demand %>% group_by(TRACT)
  dAvg <- demand %>% drop_na() %>%
    summarize(avgTrips = mean(ADJTRIPS),
              stdTrips = sd(ADJTRIPS), 
              zeroTrips = sum(ADJTRIPS == 0),
              avgAvail = mean(AVAIL),
              stdAvail = sd(AVAIL), 
              zeroAvail = sum(AVAIL == 0)) # days w/ zero avail
  # dAvg <- dAvg %>% add_column
  #                    summarize(demand, naAvail = sum(is.na(AVAIL))))
  # dAvg <- 
  return(dAvg)
}

constDemand <- function(availTime, pickups){
  geoData <- readRDS("censusData/riDataGeo.Rds") %>% 
    arrange(TRACT) %>% 
    filter(as.logical(match(geoData$TRACT, demand$TRACT)))
  
  demand <- availTime %>%
    add_column(ADJTRIPS = pickups$TRIPS/(availTime$AVAIL/960)) %>%
               dailyAvg() %>%
    add_column(GEOMETRY = geoData$geometry, NAME = geoData$NAME) %>%
    st_as_sf()
  return(demand)
}

availTime <- getAvail(fol)
pickups <- getPickups(fol) %>% 
  filter(TRACT %in% availTime$TRACT) # filter to tracts which exist within data
demand <- constDemand(availTime, pickups)

pal <- mapviewPalette("mapviewSpectralColors")
# mv <- mapview(demand, zcol = "avgTrips", col.regions = pal(20))
# print(mv)
mapview(demand, zcol = "avgTrips")