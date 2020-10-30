library(tidyverse)
source('/Users/Alice/Dropbox/pvd_summer/demand/availability/createCDF.R')

read <- function(fol, file) {
  "Read csv file"
  file <- read_csv(paste(c(fol, file), collapse=''))
  return(file)
}

getPickups <- function(fol, latLng) {
  "Open pickup data"
  if (latLng) {
    pickups  <- read.csv("pickupsLatLng.csv") %>%
      group_by(LAT, LNG)
  } else {
    pickups <- read.csv("pickupsTRACT.csv") %>%
      group_by(TRACT)
  }
  return(arrange(pickups, DATE, .by_group=TRUE))
}

getAvail <- function(fol, latLng) {
  "Open, filter, and clean availability data"
  if (latLng) {
    availIntervals <- read.csv("intervalCountsLATLNG.csv") %>%
      group_by(LAT, LNG) 
  } else {
    availIntervals <- read.csv("intervalCountsTRACT.csv") %>%
      group_by(TRACT)
  }
  avail <- availIntervals %>% 
    mutate(START = period_to_seconds(hms(START))/60, # Convert start and end time to minutes
           END = period_to_seconds(hms(END))/60) %>%
    mutate(AVAILTIME = as.numeric(COUNT>0)*AVAIL, # Calculate amount of time (in minutes) that at least one scooter was available
           COUNTTIME = COUNT*AVAIL, # Calculate amount of time (in minutes) that scooters were available for in total
           AVAILCD = as.numeric(COUNT>0)*as.numeric(AVAIL>0.1)*(cdf(END)-cdf(START))) # Calculate percent of scooter usage for intervals that were >0.1 min and had at least one scooter available
  return(avail)
}

availTotal <- function(fol, latLng) {
  "Calculate availability per day per tract"
  avail <- getAvail(fol, latLng)
  avail <- avail %>%
    group_by(DATE, .add = TRUE) %>% 
    summarize(AVAILPCT = sum(AVAILTIME)/(16*60), # Calculate percent of day that at least one scooter was available
              COUNTTIME = sum(COUNTTIME)/(16*60), # Calculate amount of time (in days) that scooters were available for in total
              CDSUM = sum(AVAILCD)/(cdf(22*60)-cdf(6*60))) # Calculate percent of daily scooter usage that occurred
  return(avail) 
}

constData <- function(fol, pickup = FALSE, latLng = FALSE){
  "Construct demand/pickup dataframe
  fol: Folder path containing availability and pickups data
  pickup: Pickup only model
  latLng: Using latlng dataset
  "
  pickups <- getPickups(fol, latLng)
  avail <- availTotal(fol, latLng) %>% 
    left_join(pickups)
  
  demand <- avail %>% 
    filter(!is.na(TRIPS)) %>%
    mutate(ADJTRIPS = pmin(5*TRIPS, TRIPS/AVAILPCT, TRIPS/CDSUM)) 
  return(demand)
}

fol <- ""

demandTRACT <- constData(fol, pickup = FALSE, latLng = FALSE)
filename <- "demandTRACT.csv"
#path <- file.path(fol, paste0(filename, ".csv"))
write.csv(demandTRACT, filename)

demandLatLng <- constData(fol, pickup = FALSE, latLng = TRUE)
filename <- "demandLatLng.csv"
#path <- file.path(fol, paste0(filename, ".csv"))
write.csv(demandLatLng, filename)
