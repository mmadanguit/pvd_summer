library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)
library(dplyr)
library(hash)
library(collections)

# test data
test_events <- read.csv("test_events.csv")
test_events$event_time <- str_sub(test_events$event_time, 1, str_length(test_events$event_time)-6)
test_events$event_time <- ymd_hms(test_events$event_time)
test_events <- arrange(test_events, event_time)


events <- read.csv("events_for_multiple_providers_from_18-10-01_to_19-11-01.csv", stringsAsFactors = FALSE)
events <- events %>% filter(!grepl("unavailable", event_type)) %>% 
          filter(!grepl("removed", event_type)) %>% 
          filter(!grepl("rebalance_drop_off", event_type_reason)) %>% 
          filter(!grepl("maintenance_drop_off", event_type_reason)) %>% 
          filter(!grepl("user_drop_off", event_type_reason)) %>% 
          filter(!grepl("service_start", event_type_reason))

# remove the time zone indicator (e.g. -05:00) at each observation time
events$event_time <- str_sub(events$event_time, 1, str_length(events$event_time)-6)

# convert event_time from string to date/time
events$event_time <- ymd_hms(events$event_time)

# order the events data by ascending date/time
events <- arrange(events, event_time)

###############################################################################
# initialize hashmap
h <- hash()
d <- dict()

# for every instance of a new date, hash the date and the index of occurrence 
find_startingtime_idx <- function(idx, dataset) {
  k <- as.character(as.Date(dataset$event_time[idx]))
  if (!has.key(k, h)) {
    h[[k]] <- idx
  }
}
###############################################################################

offset <- 1
for (i in offset:nrow(test_events)) {
  find_startingtime_idx(i, test_events)
}
