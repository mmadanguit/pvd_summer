library(tidyverse)
library(sf)
library(mapview)

# import data
locData <- locations_and_tract_18_09_01_to_19_11_01 %>% # load
  select(-c(provider, vehicle_status, vehicle_status_reason, 
            device_type, areas, lat, lng)) %>% # remove unwanted columns
  arrange(TRACT) %>% group_by(TRACT) %>% arrange(start_time, .by_group = TRUE) %>%
  filter(TRACT <= 37)# restrict to our TRACTS
# split date and time
start <- str_split_fixed(locData$start_time, " ", 2)
end <- str_split_fixed(locData$end_time, " ", 2)
locData <- locData %>% select(-c(start_time, end_time)) %>% 
  add_column(startDate = start[,1], endDate = end[,1], 
             startTime = start[,2], endTime = end[,2])

procInterval <- function(date, dayInt, startDate, startTime, endDate, endTime){
  #can we package startDate, startTime, endDate, endTime together?
  dayStart <- "06:00:00"
  DayEnd <- "22:00:00"
  if ((startDate < date) | (startTime < dayStart)){
    startTime <- dayStart # bike here before start counting
  }
  if ((endDate > date) | (endTime > dayEnd)){
    endTime <- dayEnd # bike here after starting counting
  }
  if (dayInt == is.NULL){ # no data yet
    interval <- list(startTime, endTime)
    dayInt <- matrix(interval, ncol = 2) # create matrix with interval as 1st item
  }
  else if (startTime < dayInt[nrow(a), 2]) { # before last avail ends
    if (endTime > dayInt[nrow(a), 2]){ # bike around longer than last
      dayInt[nrow(a), 2] <- endTime # extend interval
    } 
  }
  else { # bike arrives after last availability interval
    interval <- list(startTime, endTime)
    dayInt <- rbind(dayInt, interval)
  }
  return(dayInt)
}


days <- as.character(seq(as.Date("2019/1/1 00:00:00"), by = "day", length.out = 365)) # data for a whole year
# NEED TO CHANGE THIS DAY INTERVAL