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
  dayEnd <- "22:00:00"
  lastInt <- dayInt[[length(dayInt)]] # select the previous entry
  if ((entry[["startDate"]] < date) | (entry[["startTime"]] < dayStart)){
    entry[["startTime"]] <- dayStart # bike here before start counting
  }
  if ((entry[["endDate"]] > date) | (entry[["endTime"]] > dayEnd)){
    entry[["endTime"]] <- dayEnd # bike here after starting counting
  }
  if (is.null(dayInt)){ # no data yet
    interval <- list(entry[["startTime"]], entry[["endTime"]])
    dayInt <- list(interval) # lists of lists!
  }
  else if (entry[["startTime"]] < lastInt[2]) { # before last avail ends
    if (entry[["endTime"]] > lastInt[2]){ # bike around longer than last
      lastInt <- list(lastInt[1], entry[["endTime"]])
      dayInt[[length(dayInt)]] <- lastInt # extend interval
    } 
  }
  else { # bike arrives after last availability interval
    interval <- list(entry[["startTime"]], entry[["endTime"]])
    append(dayInt, list(interval)) # add interval to list
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
  if (tract %in% intervalData){
    tractIntData -> filter(intervalData, tract)
  }
  else {
    return(NULL)
  }
  if (date %in% tractIntData){
    return(filter(tractIntData, date)$INTERVALS) # intervals for that day
  }
  else {
    return(NULL)
  }
}

procEntry <- function(intervalData, entry){
  "Process an entry/AKA row in bike location data"
  dates <- calcDates(entry)
  for (date in dates){
    dateData <- getDateData(intervalData, entry[['TRACT']], date)
    intervals <- procInterval(date, dateData, entry)
    print(intervals)
    # write data
  }
}

intervalData <- data.frame(TRACT=numeric(), DATE=character(), 
                 INTERVAL=character(), AVAIL=numeric())
for (i in 1:nrow(locData)){ # for each row
  row <- locData[i,]
  procEntry(intervalData, row)
}