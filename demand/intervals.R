procInterval <- function(date, intervals, entry){
  "date: the date being analyzed for availability
  intervals: the current date's availability matrix
  entry: one row from the location data"
  dayStart <- "06:00:00"
  dayEnd <- "22:00:00"
  lastVal <- intervals[[length(intervals)]]
  if ((entry[["startDate"]] < date) | (entry[["startTime"]] < dayStart)){
    entry[["startTime"]] <- dayStart    # start time to day start
  }
  if ((entry[["endDate"]] > date) | (entry[["endTime"]] > dayEnd)){
    entry[["endTime"]] <- dayEnd    # end time to day end
  }
  if (is.null(intervals)){
    interval <- list(entry[["startTime"]], entry[["endTime"]])
    intervals <- list(interval)    # initialize with interval
  }
  else if ((entry[["startTime"]] < lastVal[2]) & (entry[["endTime"]] > lastVal[2])){
    lastVal <- list(lastVal[1], entry[["endTime"]])
    intervals[[length(intervals)]] <- lastVal     # extend interval
  }
  else if (entry[["startTime"]] > lastVal[2]){
    interval <- list(entry[["startTime"]], entry[["endTime"]])
    append(intervals, list(interval))    # new interval
  }
  intervals <-
    return(intervals)
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
  if (date %in% filter(df, TRACT == tract)$DATE){
    df$INTERVALS[df$TRACT==tract & df$DATE==date] <- intervals # update intervals
  }
  else {
    df <- df %>% add_row(TRACT = tract, DATE = date, INTERVALS = intervals) # add row
  }
  return(df)
}

procEntry <- function(intervalData, entry){
  "Process an entry/AKA row in bike location data"
  dates <- calcDates(entry)
  for (date in dates){
    dateData <- getDateData(intervalData, entry[['TRACT']], date)
    intervals <- procInterval(date, dateData, entry)
    intervalData <- saveIntervalData(intervalData, entry[['TRACT']], date, intervals)
  }
  return(intervalData)
}

getIntervalData <- function(locData){
  intervalData <- data.frame(TRACT=numeric(), DATE=character(), AVAIL=numeric())
  intervalData$INTERVALS <- list() # some reason needs to be seperate
  
  for (i in 1:nrow(locData)){ # for each row
    row <- locData[i,]
    intervalData <- procEntry(intervalData, row)
  }
  return(intervalData)
}