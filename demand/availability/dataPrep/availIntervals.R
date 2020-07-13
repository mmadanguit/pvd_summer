library(tidyverse)
setwd("~/Documents/github/pvd_summer/demand/availability/dataPrep")
source('mapTo.R')
## LOCATION DATA INTAKE ##
available <- function(df){
  "Only select available"
  df <- df %>% drop_na() %>%
    filter(vehicle_status == "available")  %>%
    filter(difftime(end_time, start_time, units = "mins") > 1)
  print('a')
  return(df)
}

selectNeeded <- function(df){
  "Selects only the location data needed"
  df <- df %>% filter(TRACT == 1.01)
  print(df)
  df <- df %>% 
    filter(TRACT <= 37) %>% # only select relevant columns
    # filter(TRACT == 8) %>%
    select(-c(provider, vehicle_status, vehicle_status_reason, 
                         device_type, areas)) %>% # remove unwanted columns
   arrange(TRACT) %>% group_by(TRACT) %>%
   arrange(start_time, .by_group = TRUE)
  # error cause by NA TRACT -> nope
  # could it be caused by declaring a function that matches name of funcs?
  print('b')
  return(df)
}

splitTimeCol <- function(df){
  "Splits the data columns into seperate day and time"
  start <- str_split_fixed(df$start_time, " ", 2)
  end <- str_split_fixed(df$end_time, " ", 2)
  df <- df %>% select(-c(start_time, end_time)) %>% 
    add_column(startDate = start[,1], endDate = end[,1], 
               startTime = start[,2], endTime = end[,2])
  return(df)
}

prepData <- function(locData, period){
  "Prep for finding interval data"
  locData <- locData %>% 
    available123() %>%
    selectNeeded123() %>%
    splitTimeCol() %>%
    filter((startDate %in% period) & (endDate %in% period))
  print('done')
  return(locData)
}

## ANALYSIS FUNCTIONS - UNUSED IN MAIN CODE##
dailySampSize <- function(df){
  "Find the daily sample size for the dataframe and plot"
  sampSize <- df %>% group_by(startDate) %>% 
    summarize(startDate, n()) %>% distinct() %>%
    rename(SAMP = 'n()', DATE = 'startDate')
  sampSize$DATE <- as.Date(sampSize$DATE)
  sampSizeP <- ggplot(data=sampSize, aes(x=DATE, y=SAMP)) + geom_point() + 
    scale_x_date(date_label = "%b/%Y", date_breaks = "1 month")
  return(sampSizeP)
}

## INTERVAL CALCULATIONS ##
getDateData <- function(intervalData, tract, date){
  "Load existing interval data if it exists"
  if (!(tract %in% intervalData$TRACT)){
    return(NULL)
  }
  tractIntData <- filter(intervalData, TRACT == tract)
  if (!(date %in% tractIntData$DATE)){
    return(NULL)
  }
  return(filter(tractIntData, DATE == date)$INTERVALS) # intervals for that day 
}

saveIntervalData <- function(df, tract, date, intervals){
  if (is.null(intervals)){
    return(df)
  }
  if (date %in% filter(df, TRACT == tract)$DATE){ # exists
    df <- df %>% filter(TRACT != tract | DATE != date)
  }
  df <- df %>% add_row(TRACT = tract, DATE = date, INTERVALS = intervals) # add row
  return(df)
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

dateConstrain <- function(entry, date, dates){
  "Create a new entry for that day"
  if (length(dates) == 1){
    return(entry)
  }
  if (date == dates[[1]]){ # first
    entry[["endTime"]] <- dayEnd
  }
  else if (date == dates[[length(dates)]]){ # last
    entry[["startTime"]] <- "00:00:00"
  }
  else { # middle 
    entry[["startTime"]] <- dayStart
    entry[["endTime"]] <- dayEnd
  }
  return(entry)
}

timeConstrain <- function(entry, dayStart, dayEnd){
  "Constrain the entry to day start and end"
  if (entry[["startTime"]] < dayStart){
    entry[["startTime"]] <- dayStart    # start time to day start
  }
  if (entry[["endTime"]] > dayEnd){
    entry[["endTime"]] <- dayEnd    # end time to day end
  }
  return(entry)
}

procInterval <- function(entry, intervals){
  "intervals: the current date's availability matrix
  entry: one row from the location data"
  entry <-  timeConstrain(entry, dayStart, dayEnd)
  lastVal <- intervals[[length(intervals)]]
  if ((entry[["endTime"]] < dayStart) | (entry[["startTime"]] > dayEnd)){
    # not within time bounds
  }
  else if (is.null(intervals)){
    interval <- list(entry[["startTime"]], entry[["endTime"]])
    intervals <- list(interval)    # initialize with interval
  }
  else if (entry[["endTime"]] > lastVal[[2]]){
    if (entry[["startTime"]] < lastVal[[2]]) {
      intervals[[length(intervals)]] <- NULL
      interval <- list(lastVal[[1]], entry[["endTime"]])  # extended interval
    }
    else {
      interval <- list(entry[["startTime"]], entry[["endTime"]])  # new interval
    }
    intervals <- append(intervals, list(interval)) 
  }
  return(intervals)
}

procEntry <- function(intervalData, entry){
  "Process an entry/AKA row in bike location data"
  dates <- calcDates(entry)
  for (date in dates){
    dateData <- getDateData(intervalData, entry[['TRACT']], date)
    entryDate <- dateConstrain(entry, date, dates)
    intervals <- procInterval(entryDate, dateData)
    intervalData <- saveIntervalData(intervalData, entry[['TRACT']], date, intervals)
  }
  return(intervalData)
}

fillClean <- function(intervalData, period){
  "Fill in missing data with NAs, missing per tract with zeros"
  noDataDays <- period[!(period %in% intervalData$DATE)] # no data
  period <- period[!(period %in% noDataDays)]
  intervalData <- intervalData %>% complete(nesting(TRACT),
    DATE = period, fill = list(START=NA, END=NA, AVAIL=0))
  for (date in noDataDays) {
    intervalData <- intervalData %>% 
      add_row(TRACT = unique(intervalData$TRACT), DATE = date, AVAIL = NA)
  }
  return(intervalData)
}

summarizeIntervals <- function(intervalData){
  "Unpack list intervals and write in more usable form factor"
  intervalData$START <- unlist(lapply(intervalData$INTERVALS, '[', 1))
  intervalData$END <- unlist(lapply(intervalData$INTERVALS, '[', 2))
  intervalData$AVAIL <- difftime(paste(intervalData$DATE, intervalData$END),
                                 paste(intervalData$DATE, intervalData$START))
  return(select(intervalData, -c(INTERVALS)))
}

getIntervalData <- function(locData, period){
  print('hi')
  intervalData <- tibble(TRACT=numeric(), DATE=character())
  intervalData$INTERVALS <- list() # some reason needs to be seperate
  for (i in 1:nrow(locData)){
    row <- locData[i,]
    intervalData <- procEntry(intervalData, row)
  }
  intervalData <- summarizeIntervals %>% 
    add_column(DAY = weekdays(as.Date(DATE))) # get weekday
  return(fillClean(intervalData, period))
}

## RUN CODE ##
dayStart <<- "06:00:00"
dayEnd <<- "22:00:00"
period <- as.character(seq(
  as.Date("2018-9-01"), as.Date("2019-10-31"), by = "day"))
file <- "~/Documents/syncthing/school/summerResearch/data/availDemand/locations.csv"

locationData <- read_csv(file)
intervalsTRACT <- locationData %>% 
  mapToTract() %>%
  prepData() %>%
  getIntervalData(period)

intervalsLatLng <- locationData %>%
  roundLatLng() %>%
  fakeTract() %>%
  prepData %>%
  getIntervalData(period)
  
write.csv(intervalData, "~/Downloads/availIntervals.csv", row.names=FALSE)