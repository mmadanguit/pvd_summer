library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)
library(dplyr)
library(hash)
library(collections)

# initialize hashmap
h <- hash()

# test data
test_loc <- read_csv("test_locations_tract_and_interval.csv")
test_loc$demand <- rep(0, length.out=nrow(test_loc))

test_events <- read.csv("test_events_and_tract.csv")
test_events$event_time <- str_sub(test_events$event_time, 1, str_length(test_events$event_time)-6)
test_events$event_time <- ymd_hms(test_events$event_time)
test_events <- arrange(test_events, event_time)


# # read data
# loc_tract <- read_csv("locations_and_tract_18-09-01_to_19-11-01.csv")
# interval_data <- read_csv("intervalData.csv")
# 
# events <- read.csv("events_for_multiple_providers_from_18-10-01_to_19-11-01.csv", stringsAsFactors = FALSE)
# events <- events %>% filter(!grepl("unavailable", event_type)) %>% 
#           filter(!grepl("removed", event_type)) %>% 
#           filter(!grepl("rebalance_drop_off", event_type_reason)) %>% 
#           filter(!grepl("maintenance_drop_off", event_type_reason)) %>% 
#           filter(!grepl("user_drop_off", event_type_reason)) %>% 
#           filter(!grepl("service_start", event_type_reason))
# 
# # remove the time zone indicator (e.g. -05:00) at each observation time
# events$event_time <- str_sub(events$event_time, 1, str_length(events$event_time)-6)
# 
# # convert event_time from string to date/time
# events$event_time <- ymd_hms(events$event_time)
# 
# # order the events data by ascending date/time
# events <- arrange(events, event_time)

###############################################################################
# for every instance of a new date, hash the date and the index of occurrence 
find_startingtime_idx <- function(idx, dataset) {
  k <- as.character(as.Date(dataset$event_time[idx]))
  if (!has.key(k, h)) {
    h[[k]] <- idx
  }
}

# function for calculating demand given a location observation
find_demand <- function(idx, tract, start_int, end_int) {
  count <- 0
  track_idx <- 1
  if (is.null(idx)) {
    return(count)
  }
  
  events <- test_events[idx:nrow(test_events), ]
  while(TRUE) {
    # break if event_time goes over the end interval
    if (difftime(events$event_time[track_idx], end_int) > seconds(0) || track_idx == nrow(events)) { 
      break
    }
    # count scooter if the event occurred in the same tract and within the time interval
    else if (tract == events$TRACT[track_idx] &&
             difftime(events$event_time[track_idx], start_int) > seconds(0) && 
             difftime(events$event_time[track_idx], end_int) < seconds(0)) {
      count <- count + 1
    }
    track_idx <- track_idx + 1
  }
  return(count)
}
###############################################################################

# map new dates to index of occurrence for events_data
offset <- 1
for (i in offset:nrow(test_events)) {
  find_startingtime_idx(i, test_events)
}

# for every observation in the locations.csv
for (j in offset:nrow(test_loc)) {
  # get the index to start looking at the events.csv
  idx <- h[[as.character(as.Date(test_loc$start_time[j]))]]
  test_loc$demand[j] <- find_demand(idx, test_loc$TRACT[j], test_loc$start_time[j], test_loc$end_time[j])
}