library(tidyverse)
library(sf)
library(mapview)

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

geomData <- function(trips){
  "Add geometry data"
  geoData <- readRDS("censusData/riDataGeo.Rds") %>% 
    arrange(TRACT) %>% 
    filter(as.logical(match(TRACT, trips$TRACT)))
  trips <- trips %>%
    add_column(GEOMETRY = geoData$geometry, NAME = geoData$NAME)
  return(st_as_sf(trips))
}

constData <- function(fol, pickup = FALSE){
  "Construct demand/pickup dataframe
  fol: Folder path containing pickupsSummary.csv and availIntervals.csv
  pickup: Pickup only model
  "
  pickups <- getPickups(fol)
  if (!pickup){ 
    availTime <- getAvail(fol) %>%
      filter(TRACT %in% pickups$TRACT) # only tracts in data
    pickups <- pickups %>%  
      filter(TRACT %in% availTime$TRACT)
    pickups$ADJTRIPS <- pickups$TRIPS/(availTime$AVAIL/960) # adjust for avail
    return(add_column(availTime, ADJTRIPS = pickups$ADJTRIPS))
  }
  else {
    return(pickups) 
  }
}

dAvg <- function(trips){
  "Find daily average for trip data over period of time.
  demand: demand dataframe"
  dAvg <- trips %>% group_by(TRACT)
  if("AVAIL" %in% colnames(trips)){
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

genMap <- function(trips, colors = 20){
  "Generate mapview for demand/pickup data"
  trips <- trips %>% dAvg()
  pal <- mapviewPalette("mapviewSpectralColors")
  mv <- mapview(trips, zcol = "meanTrips", col.regions = pal(colors))
  return(mv)
}

demandExample <- function(){
  setwd("~/Documents/github/pvd_summer/") # working directory of main gh
  # directory with summary data (availSummary.csv, pickupsSummary.csv)
  fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"
  demand <- constData(fol)
  mv <- demand %>%
    filter(DAY %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
    genMap()
  print(mv)
}

pickupExample <- function(){
  setwd("~/Documents/github/pvd_summer/") # working directory of main gh
  # directory with summary data (availIntervals.csv, pickupsSummary.csv)
  fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"
  pickup <- constData(fol, pickup = TRUE)
  mv <- pickup %>%
    filter(DATE >= "2018-10-15" & DATE <= "2018-10-16") %>%
    genMap()
  print(mv)
}