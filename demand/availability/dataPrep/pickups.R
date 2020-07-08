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
  print(df)
  return(df)
}

postClean <- function(df){
  "Select only relevant data"
  df <- df %>% filter(TRACT <= 37)
  return(df)
}


file <- "~/Documents/syncthing/school/summerResearch/data/availDemand/events2019.csv"

pickups <- read_csv(file) %>%
  splitTimeCol() %>%
  preClean() %>%
  mapToTract() %>%
  postClean() %>%
  group_by(TRACT)
pickupsSummary <- tibble(DATE=character(), TRIPS=numeric(), TRACT=numeric())
tracts <- pickups %>% distinct(TRACT) %>% pull() # good
for (i in 1:length(tracts)){ # good
  tract <- tracts[i] # good
  tractPickups <- pickups %>% filter(TRACT == tract)
  dailyPickups <- tractPickups %>%
    group_by(DATE) %>% summarize(DATE, TRIPS=n(), .groups="keep")
  dailyPickups$TRACT <- tract
  print(dailyPickups)
  pickupsSummary <- pickupsSummary %>% bind_rows(dailyPickups)
}

# ao <- pickups %>% group_by(DATE) %>% summarize(n())
  

# Process
# 5. Find # of items for each  date
# 6. Save