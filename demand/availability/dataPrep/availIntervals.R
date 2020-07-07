library(tidyverse)

## LOCATION INTAKE ##
clean <- function(df){
  "Selects only the location data needed"
  df <- df %>% select(-c(provider, vehicle_status, vehicle_status_reason, 
                         device_type, areas, lat, lng)) %>% # remove unwanted columns
    arrange(TRACT) %>% group_by(TRACT) %>% 
    arrange(start_time, .by_group = TRUE) %>%
    filter(TRACT <= 37)
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

getIntervalData <- function(locData, period){
  intervalData <- tibble(TRACT=numeric(), DATE=character())
  intervalData$INTERVALS <- list() # some reason needs to be seperate
  for (i in 1:nrow(locData)){ # for each row
    row <- locData[i,]
    intervalData <- procEntry(intervalData, row)
  }
  intervalData$START <- unlist(lapply(intervalData$INTERVALS, '[', 1))
  intervalData$END <- unlist(lapply(intervalData$INTERVALS, '[', 2))
  intervalData$AVAIL <- difftime(paste(intervalData$DATE, intervalData$END),
                                 paste(intervalData$DATE, intervalData$START))
  intervalData <- intervalData %>% select(-c(INTERVALS))
  return(fillClean(intervalData, period))
}

## RUN CODE ##
setwd("~/Documents/github/pvd_summer/demand") # change to your WD
dayStart <<- "06:00:00"
dayEnd <<- "22:00:00"
period <- as.character(seq(
  as.Date("2019-4-1"), as.Date("2019-10-31"), by = "day"))
# prep loc data
locData <- locations_and_tract_and_rounded_latlong_from_oct18_oct19 %>%
  clean() %>% splitTimeCol()
  filter((startDate %in% period) & (endDate %in% period)) # restrict to period
 # compute intervals
intervalData <- getIntervalData(locData, period)