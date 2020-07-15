library(tidyverse)
setwd("~/Documents/github/pvd_summer/demand/availability/dataPrep") # change to your WD
source('mapTo.R')

splitTimeCol <- function(pickups){
  "Splits the data column into seperate day and time"
  split <- str_split_fixed(pickups$event_time, " ", 2)
  pickups <- pickups %>% select(-c(event_time)) %>% 
    add_column(DATE = split[,1], TIME = split[,2])
  return(pickups)
}

relevant <- function(pickups){
  "Select only relevant data"
  pickups <- pickups %>% filter(event_type_reason == "user_pick_up") %>%
    filter(TIME > "06:00:00" & TIME < "22:00:00") %>%
    select(-c(provider, event_type, event_type_reason, device_type))
  return(pickups)
}

fill <- function(pickups, period){
  "Fill in missing information with zeros"
  pickups <- pickups %>% group_by(TRACT) %>% 
          complete(nesting(TRACT), DATE = period, fill = list(TRIPS=0))
  return(pickups)
}

file <- "~/Documents/syncthing/school/summerResearch/data/availDemand/events.csv"

pickups <- read_csv(file) %>%
  splitTimeCol() %>%
  relevant()

period <- as.character(seq(
  as.Date("2018-9-01"), as.Date("2019-10-31"), by = "day"))

pickupsTRACT <- pickups %>% 
  mapToTract() %>% 
  filter(TRACT <= 37) %>%
  group_by(TRACT) %>% 
  group_by(DATE, .add=TRUE) %>% 
  summarize(DATE, TRIPS=n(), .groups="keep") %>% # for each day in each tract
  distinct() %>%
  fill(period) %>%
  mutate(DAY = weekdays(as.Date(DATE))) # enter weekday 
write.csv(pickupsTRACT, "~/Downloads/pickupsTRACT.csv", row.names=FALSE)

print('latLng')
pickupsLatLng <- pickups %>% 
  roundLatLng() %>%
  fakeTract() %>%
  group_by(TRACT) %>% 
  group_by(DATE, .add=TRUE) %>% 
  summarize(DATE, TRIPS=n(), .groups="keep") %>% # for each day in each tract
  distinct() %>%
  fill(period) %>%
  undoFakeTract() %>% ungroup() %>% select(c(LAT, LNG, DATE, TRIPS)) # get rid of fake tracts
write.csv(pickupsLatLng, "~/Downloads/pickupsLatLng.csv", row.names=FALSE)