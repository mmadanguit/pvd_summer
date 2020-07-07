library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)
library(dplyr)

loc_tract <- read_csv("test_locations.csv")
intervalData <- read_csv("intervalData.csv")

# create empty columns for interval times and availability to be updated later
loc_tract$start_interval <- rep(as.POSIXct("00:00:00",format="%H:%M:%S"), length.out=nrow(loc_tract))
loc_tract$end_interval <- rep(as.POSIXct("00:00:00",format="%H:%M:%S"), length.out=nrow(loc_tract))
loc_tract$availability <- NA

offset <- 1
for (i in offset:nrow(loc_tract)) {
  for (j in offset:nrow(intervalData)) {
    # if TRACTS and the times are identical, fill in loc_tract with data from intervalData
    if (loc_tract$TRACT[i] == intervalData$TRACT && as.Date(loc_tract$start_time[i]) == intervalData$DATE[j]) {
      loc_tract$start_interval[i] <- intervalData$START[j]
      loc_tract$end_interval[i] <- intervalData$END[j]
      loc_tract$availability[i] <- intervalData$AVAIL[j]
    }
  }
}

# take care of the time shift
loc_tract$start_interval <- loc_tract$start_interval + 5*60*60
loc_tract$end_interval <- loc_tract$end_interval + 5*60*60

# only take the time and strip the date
loc_tract$start_interval <- format(loc_tract$start_interval, "%H:%M:%S")
loc_tract$end_interval <- format(loc_tract$end_interval, "%H:%M:%S")

write_csv(loc_tract, "test_locations_tract_and_interval.csv")
