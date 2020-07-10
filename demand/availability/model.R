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
  print(demand)
  dAvg <- demand %>% 
    group_by(TRACT) %>%
    summarize(meanTrips = mean(ADJTRIPS, na.rm = TRUE),
              stdTrips = sd(ADJTRIPS, na.rm = TRUE),
              zeroTrips = sum(ADJTRIPS == 0, na.rm = TRUE),
              meanAvail = mean(AVAIL, na.rm = TRUE),
              stdAvail = sd(AVAIL, na.rm = TRUE),
              zeroAvail = sum(AVAIL == 0, na.rm = TRUE), # days w/ zero avail
              naAvail = sum(is.na(AVAIL))) %>%
    drop_na()
  dAvg[dAvg == Inf] <- NA
  return(dAvg)
}

constDemand <- function(availTime, pickups){
  demand <- availTime %>%
    add_column(ADJTRIPS = pickups$ADJTRIPS) %>% 
    dailyAvg()
  geoData <- readRDS("censusData/riDataGeo.Rds") %>% 
    arrange(TRACT) %>% 
    filter(as.logical(match(TRACT, demand$TRACT)))
  demand <- demand %>%
    add_column(GEOMETRY = geoData$geometry, NAME = geoData$NAME)
  return(st_as_sf(demand))
}

availTime <- getAvail(fol)

pickups <- getPickups(fol) %>% 
  filter(TRACT %in% availTime$TRACT) # filter to tracts which exist within data
pickups$ADJTRIPS <- pickups$TRIPS/(availTime$AVAIL/960)
demand <- constDemand(availTime, pickups)

pal <- mapviewPalette("mapviewSpectralColors")
mv <- mapview(demand, zcol = "meanTrips", col.regions = pal(20))
print(mv)