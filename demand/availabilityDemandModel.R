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

calcDates <- function(entry){
  "Generate list of dates for an entry"
  if (entry[["startDate"]] == entry[["endDate"]]){ # only one day
    dates <- list(entry[["startDate"]])
  }
  else { # find days
    dates <- as.character(seq(as.Date(entry[["startDate"]]),
                              as.Date(entry[["endDate"]]), by='day'))                            
  }
  return(dates)
}

getDateData <- function(intervalData, tract, date){
  "Load existing interval data if it exists"
  tractIntData -> filter(intervalData, tract)
  if (date %in% tractIntData){
    return(filter(tractIntData, date)$INTERVALS) # intervals for that day
  }
  else {
    return(NA)
  }
}

procEntry <- function(intervalData, entry){
  "Process an entry/AKA row in bike location data"
  dates <- calcDates(entry)
  for (date in dates){
    dateData <- getDateData(data, entry[['TRACT']], date)
    # see if date exists for that tract
    if (dateDataExists(data, entry[['TRACT']], date)){
      
    }
    procInterval(date, entry)
  }
  return(dates)
}

intervalData <- data.frame(TRACT=numeric(), DATE=character(), 
                 INTERVAL=character(), AVAIL=numeric())
for (i in 1:nrow(locData)){
  row <- locData[i,]
  procEntry(row)
}