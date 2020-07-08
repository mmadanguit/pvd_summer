library(tidyverse)
setwd("~/Documents/github/pvd_summer/demand/availability/dataPrep") # change to your WD
source('mapToTract.R')

splitTimeCol <- function(df){
  "Splits the data column into seperate day and time"
  split <- str_split_fixed(df$event_time, " ", 2)
  df <- df %>% select(-c(event_time)) %>% 
    add_column(DATE = split[,1], TIME = split[,2])
  return(df)
}

preClean <- function(df){
  "Select only relevant data"
  df <- df %>% filter(event_type_reason == "user_pick_up") %>%
    filter(TIME > "06:00:00" & TIME < "20:00:00") %>%
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

file <- "~/Documents/syncthing/school/summerResearch/data/availDemand/events2018.csv"

pickups <- read_csv(file) %>%
  splitTimeCol() %>%
  preClean() %>%
  mapToTract() %>%
  postClean()

period <- as.character(seq(
  as.Date("2018-1-01"), as.Date("2018-12-31"), by = "day"))
pickupsSummary <- pickups %>% 
  group_by(TRACT) %>% 
  group_by(DATE, .add=TRUE) %>% 
  summarize(DATE, TRIPS=n(), .groups="keep") %>% # for each day in each tract
  distinct() %>%
  fill(period)

write.csv(pickupsSummary, "~/Downloads/pickupsSummary2018.csv")