library(tidyverse)
library(sf)
library(mapview)
setwd("~/Documents/github/pvd_summer/") # change to your WD

getAvail <- function(fol){
  "Open, filter, and clean availability data"
  availIntervals <- read_csv(paste(c(fol, "availIntervals.csv"), collapse='')) %>%
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
  pickups <- read_csv(paste(c(fol, "pickupsSummary.csv"), collapse='')) %>%
    group_by(TRACT) %>% 
    arrange(DATE, .by_group=TRUE)
  return(pickups)
}

geomData <- function(demand){
  "Add geometry data"
  geoData <- readRDS("censusData/riDataGeo.Rds") %>% 
    arrange(TRACT) %>% 
    filter(as.logical(match(TRACT, demand$TRACT)))
  demand <- demand %>%
    add_column(GEOMETRY = geoData$geometry, NAME = geoData$NAME)
  return(st_as_sf(demand))
}

dailyAvg <- function(demand){
  "Find daily average for trip data over period of time.
  demand: demand dataframe"
  print(demand)
  dAvg <- demand %>% group_by(TRACT)
  if("AVAIL" %in% colnames(demand)){
  dAvg <- dAvg %>%
    summarize(meanTrips = mean(ADJTRIPS, na.rm = TRUE),
              medTrips = median(ADJTRIPS, na.rm = TRUE),
              stdTrips = sd(ADJTRIPS, na.rm = TRUE),
              zeroTrips = sum(ADJTRIPS == 0, na.rm = TRUE),
              meanAvail = mean(AVAIL, na.rm = TRUE),
              medAvail = median(AVAIL, na.rm = TRUE),
              stdAvail = sd(AVAIL, na.rm = TRUE),
              zeroAvail = sum(AVAIL == 0, na.rm = TRUE), # days w/ zero avail
              naAvail = sum(is.na(AVAIL))) %>%
    drop_na()
  }
  else {
    dAvg <- dAvg %>%
      summarize(meanTrips = mean(TRIPS, na.rm = TRUE),
                medTrips = median(TRIPS, na.rm = TRUE),
                stdTrips = sd(TRIPS, na.rm = TRUE),
                zeroTrips = sum(TRIPS == 0, na.rm = TRUE)) %>%
      drop_na()
  }
  dAvg[dAvg == Inf] <- NA
  return(geomData(dAvg))
}

constData <- function(fol, avail = FALSE){
  "Construct demand dataframe
  fol: Folder path containing pickupsSummary.csv and availabilitySummary.csv
  avail: Availability only model
  "
  pickups <- getPickups(fol)
  if (!avail){ 
    availTime <- getAvail(fol) %>%
      filter(TRACT %in% pickups$TRACT) # only tracts in data
    pickups <- pickups %>%  
      filter(TRACT %in% availTime$TRACT)
    pickups$ADJTRIPS <- pickups$TRIPS/(availTime$AVAIL/960) # adjust for avail
  }
  else {
    pickups$ADJTRIPS <- pickups$TRIPS    
  }
  demand <- availTime %>%
    add_column(ADJTRIPS = pickups$ADJTRIPS)
  return(demand)
}

# assumes working directory in pvd_summer to grab geo data
fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"
# data available on drive: Data/demand
demand <- constData(fol)
demand <- demand %>%
  # filter() %>%
  dailyAvg()
pal <- mapviewPalette("mapviewSpectralColors")
mv <- mapview(demand, zcol = "meanTrips", col.regions = pal(20))
print(mv)