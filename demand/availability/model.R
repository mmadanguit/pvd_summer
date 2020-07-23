library(tidyverse)
library(sf)
library(mapview)
library(pracma)
source("demand/availability/createCDF.R")

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
  availTime <- availIntervals %>% arrange(DATE, .by_group=TRUE) %>% 
    select(-c(START, END)) %>% # time w/o intervals
    group_by(DATE, .add = TRUE) %>% 
    mutate(COUNTTIME = COUNT*AVAIL) %>%
    filter(COUNT > 0) %>%
    summarize(AVAIL = sum(AVAIL)/960, COUNT = sum(COUNTTIME)/960) %>%
    distinct() %>%
    filter(AVAIL > .1)
  availCD <- availIntervals %>% 
    # Convert time to seconds
    mutate(START = period_to_seconds(hms(START)),
           END = period_to_seconds(hms(END))) %>%
    # Calculate cumulative distribution of availability interval
    mutate(AVAILCD = cdf(END)-cdf(START)) %>%
    # Sum cumulative distributions 
    filter(COUNT > 0) %>%
    select(-c(START, END)) %>%
    group_by(DATE, .add = TRUE) %>%
    summarise(AVAILCD = sum(AVAILCD)/(cdf(22*60*60)-cdf(6*60*60))) %>%
    distinct() %>%
    filter(AVAILCD > .1)
  return(merge(availTime, availCD))
}

constData <- function(fol, pickup = FALSE, latLng = FALSE){
  "Construct demand/pickup dataframe
  fol: Folder path containing availability and pickups data
  pickup: Pickup only model
  latLng: Using latlng dataset
  "
  pickups <- getPickups(fol, latLng)
  demand <- getAvail(fol, latLng) %>% 
    left_join(pickups)
  demand <- demand %>% 
    mutate(ADJTRIPS = demand$TRIPS/demand$AVAIL,
           ADJTRIPSCD = demand$TRIPS/demand$AVAILCD,
           ADJTRIPSMIN = pmin(ADJTRIPS, ADJTRIPSCD))
  demand$ADJTRIPSCAP <- ifelse(demand$ADJTRIPSMIN > 5*demand$TRIPS,
                               5*demand$TRIPS, demand$ADJTRIPSMIN)
  demand$ADJTRIPSCAP10 <- ifelse(demand$ADJTRIPSMIN > 10*demand$TRIPS,
                               10*demand$TRIPS, demand$ADJTRIPSMIN)
  demand$ADJTRIPSCAPR <- ifelse(demand$ADJTRIPS > 10*demand$TRIPS,
                                 10*demand$TRIPS, demand$ADJTRIPS)
  demand$ADJTRIPSCAPCD <- ifelse(demand$ADJTRIPSCD > 10*demand$TRIPS,
                                10*demand$TRIPS, demand$ADJTRIPSCD)
  # ADJTRIPS min is just the minimum is the minimum of ADJTRIPS or ADJTRIPS CD
  # ADJTRIPS CHECKS if it is greater than 5 times recorded
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
                
                meanTripsCD = mean(ADJTRIPSCD, na.rm = TRUE),
                medTripsCD = median(ADJTRIPSCD, na.rm = TRUE),
                stdTripsCD = sd(ADJTRIPSCD, na.rm = TRUE),
                zeroTripsCD = sum(ADJTRIPSCD == 0, na.rm = TRUE),
                
                meanTripsMIN = mean(ADJTRIPSMIN, na.rm = TRUE),
                medTripsMIN = median(ADJTRIPSMIN, na.rm = TRUE),
                stdTripsMIN = sd(ADJTRIPSMIN, na.rm = TRUE),
                zeroTripsMIN = sum(ADJTRIPSMIN == 0, na.rm = TRUE),
                
                meanTripsCAP = mean(ADJTRIPSCAP, na.rm = TRUE),
                medTripsCAP = median(ADJTRIPSCAP, na.rm = TRUE),
                stdTripsCAP = sd(ADJTRIPSCAP, na.rm = TRUE),
                zeroTripsCAP = sum(ADJTRIPSCAP == 0, na.rm = TRUE),
                
                meanTripsCAP10 = mean(ADJTRIPSCAP10, na.rm = TRUE),
                medTripsCAP10 = median(ADJTRIPSCAP10, na.rm = TRUE),
                stdTripsCAP10 = sd(ADJTRIPSCAP10, na.rm = TRUE),
                zeroTripsCAP10 = sum(ADJTRIPSCAP10 == 0, na.rm = TRUE),
                
                meanTripsCAPR = mean(ADJTRIPSCAPR, na.rm = TRUE),
                medTripsCAPR = median(ADJTRIPSCAPR, na.rm = TRUE),
                stdTripsCAPR = sd(ADJTRIPSCAPR, na.rm = TRUE),
                zeroTripsCAPR = sum(ADJTRIPSCAPR == 0, na.rm = TRUE),
                
                meanTripsCAPCD = mean(ADJTRIPSCAPCD, na.rm = TRUE),
                medTripsCAPCD = median(ADJTRIPSCAPCD, na.rm = TRUE),
                stdTripsCAPCD = sd(ADJTRIPSCAPCD, na.rm = TRUE),
                zeroTripsCAPCD = sum(ADJTRIPSCAPCD == 0, na.rm = TRUE),
                
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
  return(st_as_sf(trips, crs = 4326))
}

latLngRect <- function(westLng, eastLng, southLat, northLat) {
  df <- tibble("lng" = as.numeric(), "lat" = as.numeric(), "ID" = as.numeric())
  for (id in 1:length(westLng)){
    df <- df %>% 
      # goes upper left, bottom left, bottom right, upper right
      add_row(lng = westLng[id], lat = northLat[id], ID = id) %>% # upper left
      add_row(lng = westLng[id], lat = southLat[id], ID = id) %>% # lower left
      add_row(lng = eastLng[id], lat = southLat[id], ID = id) %>% # lower right
      add_row(lng = eastLng[id], lat = northLat[id], ID = id)
  }
  return(df)
}

geoLatLng <- function(trips){
  "Build shape data for lat lng trip data"
  rd <- 0.005 # rounding value
  westLng <- trips$LNG-rd/2
  eastLng <- trips$LNG+rd/2
  southLat <- trips$LAT-rd/2
  northLat <- trips$LAT+rd/2
  df <- latLngRect(westLng, eastLng, southLat, northLat)
  
  ri_tracts <- readRDS("censusData/riDataGeo.Rds")
  points <- st_as_sf(df, coords=c("lng", "lat"),crs = 4326)
  polys <- st_sf(
    aggregate(
      points$geometry,
      list(points$ID),
      function(g){
        st_cast(st_combine(g),"POLYGON")}
    ))
  trips$geometry <- polys$geometry
  return(st_as_sf(trips))
}

genMap <- function(trips, latLng = FALSE, zcol = "meanTrips", colors = 20){
  "Generate mapview for demand/pickup data"
  trips <- trips %>% dAvg(latLng)
  pal <- mapviewPalette("mapviewSpectralColors")
  if (latLng) {
    tripData <- geoLatLng(trips)
  }
  else {
    tripData <- geoData(trips)
  }
  data <- as.data.frame(tripData)[zcol]
  nonzeroData <- filter(data, data[zcol] > 0) #finding the nonzero min makes data with 0s play better. 0 values are always default gray.
  min <- min(nonzeroData) - 0.0000000001 # the subtraction makes sure the nonzero min is included
  max <- max(nonzeroData) + 0.0000000001 # the addition makes sure the max is included
  mv <- mapview(tripData, zcol = zcol, col.regions = pal(colors), at = logseq(min, max), scientific = TRUE) #at controls the color gradient and makes it log
  # The legend colors are wrong. I think the color for a tract with value x is the legend color (ln(x)/ln(max))*max, or something along those lines but incorporating the min of the colorscale.
  return(mv)
}

# demandExample <- function(latLng = FALSE, zcol = "avail"){
#   # setwd("~/Documents/github/pvd_summer/") # working directory of main gh
#   # directory with summary data (availSummary.csv, pickupsSummary.csv)
#   # fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"
#   demand <- constData(fol, latLng = latLng)
#   mv <- demand %>%
#     # filter(DAY %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
#     # filter(DATE >= "2019-5-1" & DATE <= "2019-5-31") %>%
#     genMap(latLng, zcol = zcol)
#   print(mv)
# }
# 
# pickupExample <- function(latLng = FALSE, zcol = "meanTrips"){
#   # setwd("~/Documents/github/pvd_summer/") # working directory of main gh
#   # fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"
#   pickup <- constData(fol, pickup = TRUE, latLng = latLng)
#   mv <- pickup %>%
#     # filter(DATE >= "2019-7-1" & DATE <= "2019-7-31") %>%
#     genMap(latLng, zcol = zcol)
#   print(mv)
# }
# 
# demandExample(zcol = "meanTrips", latLng = FALSE)