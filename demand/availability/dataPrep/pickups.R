library(tidyverse)
setwd("~/Documents/github/pvd_summer/demand/availability/dataPrep") # change to your WD
source('mapToTract.R')

preClean <- function(df){
  "Select only relevant data"
  df <- df %>% filter(event_type_reason == "user_pick_up") %>%
    select(-c(provider, event_type, event_type_reason, device_type))
  return(df)
}

postClean <- function(df){
  "Select only relevant data"
  df <- df %>% filter(TRACT <= 37)
  return(df)
}

splitTimeCol <- function(df){
  "Splits the data column into seperate day and time"
  split <- str_split_fixed(df$event_time, " ", 2)
  df <- df %>% select(-c(event_time)) %>% 
    add_column(DATE = split[,1], TIME = split[,2])
  return(df)
}


file <- "~/Documents/syncthing/school/summerResearch/data/availDemand/events2019.csv"

# pickups <- read_csv(file) %>%
#   preClean() %>%
#   mapToTract() %>%
#   splitTimeCol() %>%
#   postClean() %>%
#   group_by(TRACT)
pickupsSummary <- tibble(TRACT=numeric(), DATE=character(), TRIPS=numeric())
tracts <- pickups %>% distinct(TRACT) %>% pull()
for (i in 1:length(tracts)){
  tract <- tracts[i]
  tractPickups <- pickups %>% filter(TRACT == tract)
  dailyPickups <- tractPickups %>%
    group_by(DATE) %>% summarize(DATE, TRIPS=n())
  dailyPickups$TRACT <- tract
  pickupsSummary <- pickupsSummary %>% bind_rows(dailyPickups)
}

# ao <- pickups %>% group_by(DATE) %>% summarize(n())
  

# Process
# 5. Find # of items for each  date
# 6. Save