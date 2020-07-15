library(tidyverse)
library(lubridate)
source('startDayAvail.R')

roundTime <- function(data) {
  "Round the event time to the nearest 5 minutes (the minutes are subject to change?)"
  data$event_time <- round_date(data$event_time, unit = "minutes")
  return(data)
}

getEventData <-function(data) {
  # create structure for organizing event data
  eventData <- tibble(TRACT = numeric(), 
                      DATE = character(), 
                      INTERVAL_START = character(), 
                      INTERVAL_END = character(), 
                      N = numeric())
  
  # confusion
  
  return (eventData)
}

#################################################
# start day for getting avg scooters
startDay <- read_csv("events_for_multiple_providers_on_18-10-18.csv") # replace file with the one you want as the start day
startAvail <- startDayAvail(startDay)

events <- read_csv("events2018.csv") %>%
          bind_rows(read_csv("events2019.csv"))

events <- events %>%
          roundTime() %>%
          getEventData()