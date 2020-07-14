library(tidyverse)
source('mapToTract.R')


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

preCleanUnavail <- function(df){
  "Select only the scooters that became unavailable"
  df <- df %>% filter(event_type_reason == "user_pick_up") %>%
    filter(TIME > "06:00:00" & TIME < "22:00:00") %>%
    select(-c(provider, event_type, event_type_reason, device_type))
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

##############################################
file <- read_csv("events2018.csv") %>%
        bind_rows(read_csv("events2019.csv"))

# process to separate avail and unavail scooters
numAvail <- file %>%
                splitTimeCol() %>%
                preCleanAvail() %>%
                mapToTract() %>%
                postClean()

numUnavail <- file %>%
                  splitTimeCol() %>%
                  preCleanUnavail() %>%
                  mapToTract() %>%
                  postClean()

# find the total number for each case of avail and unavail scooters
period <- as.character(seq(
  as.Date("2018-10-01"), as.Date("2019-10-31"), by = "day"))
numAvailSummary <- numAvail %>%
                  group_by(TRACT) %>%
                  group_by(DATE, .add=TRUE) %>%
                  summarize(DATE, TRIPS=n(), .groups="keep") %>% # for each day in each tract
                  distinct() %>%
                  fill(period)

numUnavailSummary <- numUnavail %>% 
                  group_by(TRACT) %>% 
                  group_by(DATE, .add=TRUE) %>% 
                  summarize(DATE, TRIPS=n(), .groups="keep") %>% # for each day in each tract
                  distinct() %>%
                  fill(period) 

# find the average number of available scooters for each TRACT
avgAvail <-numAvailSummary$TRIPS / (numAvailSummary$TRIPS + numUnavailSummary$TRIPS) 

