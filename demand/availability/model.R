library(tidyverse)
library(sf)
library(mapview)
library(pracma)

read <- function(fol, file){
  "Read csv file"
  file <- read_csv(paste(c(fol, file), collapse=''))
  return(file)
}

getPickups <- function(fol, latLng){
  if (latLng){
    pickups  <- read(fol, "pickupsLatLng.csv") %>%
      group_by(LAT) %>%
      group_by(LNG, .add = TRUE)
  }
  else {
    pickups <- read(fol, "pickupsTRACT.csv") %>%
      group_by(TRACT)
  }
  pickups <- pickups
  return(arrange(pickups, DATE, .by_group=TRUE))
}

getAvail <- function(fol, latLng){
  "Open, filter, and clean availability data"
  if (latLng){
    availIntervals <- read(fol, "intervalCountsLATLNG.csv") %>%
      group_by(LAT) %>% 
      group_by(LNG, .add = TRUE)
  }
  else {
    availIntervals <- read(fol, "intervalCountsTRACT.csv") %>%
      group_by(TRACT)
  }
  availIntervals <- availIntervals %>% arrange(DATE, .by_group=TRUE)
  availTime <- availIntervals %>% 
    select(-c(START, END)) %>% # time w/o intervals
    group_by(DATE, .add = TRUE) %>% 
    mutate(COUNTTIME = COUNT*AVAIL) %>%
    filter(COUNT > 0) %>%
    summarize(AVAIL = sum(AVAIL), COUNT = sum(COUNTTIME)) %>%
    distinct()
  return(availTime)
}

constData <- function(fol, pickup = FALSE, latLng = FALSE){
  "Construct demand/pickup dataframe
  fol: Folder path containing availability and pickups data
  pickup: Pickup only model
  latLng: Using latlng dataset
  "
  pickups <- getPickups(fol, latLng)
  # need to join with demand to select the same columns
  if (pickup){
    return(pickups)
  }
  demand <- getAvail(fol, latLng) %>% 
    left_join(pickups)
  demand <- demand %>% 
    add_column(ADJTRIPS = demand$TRIPS/(demand$AVAIL/960))
    
  return(demand)
}

dAvg <- function(trips, latLng){
  "Find daily average for trip data over period of time.
  demand: demand dataframe"
  if (latLng){
    dAvg <- trips %>% group_by(LAT) %>% group_by(LNG, .add = TRUE)
  }
  else {
    dAvg <- trips %>% group_by(TRACT)
  }
  if("AVAIL" %in% colnames(trips)){
    dAvg <- dAvg %>%
      summarize(meanTrips = mean(ADJTRIPS, na.rm = TRUE),
                medTrips = median(ADJTRIPS, na.rm = TRUE),
                stdTrips = sd(ADJTRIPS, na.rm = TRUE),
                zeroTrips = sum(ADJTRIPS == 0, na.rm = TRUE),
                meanAvailTime = mean(AVAIL, na.rm = TRUE),
                medAvailTime = median(AVAIL, na.rm = TRUE),
                stdAvailTime = sd(AVAIL, na.rm = TRUE),
                zeroAvailTime = sum(AVAIL == 0, na.rm = TRUE), # days w/ zero avail
                naAvailTime = sum(is.na(AVAIL)),
                meanAvail = mean(COUNT, na.rm = TRUE), 
                medAvail = median(COUNT, na.rm = TRUE), 
                stdAvail = sd(COUNT, na.rm = TRUE),
                zeroAvail = sum(COUNT == 0, na.rm = TRUE))
  }
  else {
    dAvg <- dAvg %>%
      summarize(meanTrips = mean(TRIPS, na.rm = TRUE),
                medTrips = median(TRIPS, na.rm = TRUE),
                stdTrips = sd(TRIPS, na.rm = TRUE),
                zeroTrips = sum(TRIPS == 0, na.rm = TRUE))
  }
  dAvg <- dAvg %>% mutate_if(is.numeric, round, 3) %>% drop_na()
  dAvg[dAvg == Inf] <- NA
  return(dAvg)
}

geoData <- function(trips){
  "Add geometry data"
  geoData <- readRDS("censusData/riDataGeo.Rds") %>% 
    arrange(TRACT) %>% 
    filter(as.logical(match(TRACT, trips$TRACT)))
  trips <- trips %>%
    add_column(GEOMETRY = geoData$geometry, NAME = geoData$NAME)
  return(st_as_sf(trips))
}

geoLatLng <- function(trips){
  "Build shape data for lat lng trip data"
  #Currently in progress
  return(st_as_sf(trips, coords = c("LNG","LAT"), crs=4326)) #Converts LAT/LON to geometry points
}

genMap <- function(trips, latLng = FALSE, zcol = "meanTrips", colors = 20){
  "Generate mapview for demand/pickup data"
  trips <- trips %>% dAvg(latLng)
  pal <- mapviewPalette("mapviewSpectralColors")
  if (latLng) {
    print(trips)
    print("to implement")
    tripData <- geoLatLng(trips)
  }
  else {
    tripData <- geoData(trips)
  }
  data <- as.data.frame(tripData)[zcol]
  nonzeroData <- filter(data, data[zcol] > 0) #finding the nonzero min makes data with 0s play better. 0 values are always default gray.
  # print(data)
  min <- min(nonzeroData) - 0.0000000001 # the subtraction makes sure the nonzero min is included
  max <- max(nonzeroData) + 0.0000000001 # the addition makes sure the max is included

  # print(min)
  # print(max)
  mv <- mapview(tripData, zcol = zcol, col.regions = pal(colors), at = logseq(min, max), scientific = TRUE) #at controls the color gradient and makes it log
  #The legend colors are wrong. I think the color for a tract with value x is the legend color (ln(x)/ln(max))*max, or something along those lines but incorporating the min of the colorscale.
  return(mv)
}

demandExample <- function(latLng = FALSE, zcol = "meanTrips"){
  #setwd("~/Documents/github/pvd_summer/") # working directory of main gh
  # directory with summary data (availSummary.csv, pickupsSummary.csv)
  fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"
  demand <- constData(fol, latLng = latLng)
  mv <- demand %>%
    # filter(DAY %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
    genMap(latLng, zcol = zcol)
  print(mv)
}

pickupExample <- function(latLng = FALSE, zcol = "meanTrips"){
  setwd("~/Documents/github/pvd_summer/") # working directory of main gh
  fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"
  pickup <- constData(fol, pickup = TRUE, latLng = latLng)
  mv <- pickup %>%
    # filter(DATE >= "2019-7-1" & DATE <= "2019-7-31") %>%
    genMap(latLng, zcol = zcol)
  print(mv)
}

demandExample(zcol = "meanAvail")