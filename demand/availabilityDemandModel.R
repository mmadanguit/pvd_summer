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

procInterval <- function(date, dayInt, entry){
  "date: the date being analyzed for availability
  dayInt: the current date's availability matrix
  entry: one row from the location data"
  dayStart <- "06:00:00"
  DayEnd <- "22:00:00"
  if ((entry[["startDate"]] < date) | (entry[["startTime"]] < dayStart)){
    entry[["startTime"]] <- dayStart # bike here before start counting
  }
  if ((entry[["endDate"]] > date) | (entry[["endTime"]] > dayEnd)){
    entry[["endTime"]] <- dayEnd # bike here after starting counting
  }
  if (dayInt == is.NULL){ # no data yet
    interval <- list(entry[["startTime"]], entry[["endTime"]])
    dayInt <- matrix(interval, ncol = 2) # create matrix with interval as 1st item
  }
  else if (entry[["startTime"]] < dayInt[nrow(a), 2]) { # before last avail ends
    if (entry[["endTime"]] > dayInt[nrow(a), 2]){ # bike around longer than last
      dayInt[nrow(a), 2] <- entry[["endTime"]] # extend interval
    } 
  }
  else { # bike arrives after last availability interval
    interval <- list(entry[["startTime"]], entry[["endTime"]])
    dayInt <- rbind(dayInt, interval)
  }
  return(dayInt)
}

proc <- function(entry){
  # print(entry)
  if (entry[["startDate"]] == entry[["endDate"]]){ # only one day
    dates <- list(entry[["startDate"]])
  }
  else { # find days
    dates <- as.character(seq(as.Date(entry[["startDate"]]),
                                      as.Date(entry[["endDate"]]), by='day'))                            
  }
  a <- entry[["startDate"]]
  b <- entry[["endDate"]]
  for (date in dates){
    print(date)
  }
  return(dates)
}
apply(locData, 1, proc)
