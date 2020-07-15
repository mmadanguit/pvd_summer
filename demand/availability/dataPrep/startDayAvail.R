library(tidyverse)
source('mapToTract.R')


getStartDay <- function(df){
  "Get the date of the start day"
  split <- str_split_fixed(df$event_time[2], " ", 2)
  return(split[,1])
}

splitTimeCol <- function(df){
  "Splits the data column into seperate day and time"
  split <- str_split_fixed(df$event_time, " ", 2)
  df <- df %>% select(-c(event_time)) %>% 
    add_column(DATE = split[,1], TIME = split[,2])
  return(df)
}

preCleanAvail <- function(df){
  "Select only the scooters that became available"
  df <- df %>% filter(event_type == "available") %>%
    filter(TIME > "06:00:00" & TIME < "22:00:00") %>%
    select(-c(provider, event_type, event_type_reason, device_type))
  print(df)
  return(df)
}

postClean <- function(df){
  "Select only relevant data"
  df <- df %>% filter(TRACT <= 37)
  return(df)
}

fill <- function(df, period){
  "Fill in missing information with zeros"
  df <- df %>% group_by(TRACT) %>% 
    complete(nesting(TRACT), DATE = period, fill = list(TRIPS=0))
  return(df)
}

startDayAvail <- function(data) {
  # only get the available scooters
  numAvail <- data %>%
    splitTimeCol() %>%
    preCleanAvail() %>%
    mapToTract() %>%
    postClean()

  period <- getStartDay(data)
  TRACTS <- c(1.01, 1.02, 2:29, 31:35, 36.01, 36.02, 37)
  
  # find the total number of available scooters for each tract
  numAvailSummary <- numAvail %>%
    group_by(TRACT) %>%
    group_by(DATE, .add=TRUE) %>%
    summarize(DATE, TRIPS=n(), .groups="keep") %>% # for each day in each tract
    distinct() %>%
    fill(period)
    
  # fill in missing TRACTS
  numAvailSummary <- numAvailSummary %>%
                     complete(TRACT = TRACTS, fill = list(DATE = period, TRIPS = 0))
  
  # return only the unique rows
  return(unique(numAvailSummary))
}

# start day for getting avg scooters
file <- read_csv("events_for_multiple_providers_on_18-10-18.csv") # replace file with the one you want as the start day
res = startDayAvail(file)


