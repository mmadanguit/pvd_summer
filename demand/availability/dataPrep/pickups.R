library(tidyverse)
setwd("~/Documents/github/pvd_summer/demand/availability/dataPrep") # change to your WD
source('mapToTract.R')

cleanEvents <- function(df){
  "Remove unnecessary events items"
  df <- df %>% filter(event_type_reason == "user_pick_up") %>%
    select(-c(provider, event_type, event_type_reason, device_type))
  return(df)
}

splitTimeCol <- function(df){
  "Splits the data column into seperate day and time"
  split <- str_split_fixed(df$event_time, " ", 2)
  df <- df %>% select(-c(event_time)) %>% 
    add_column(date = split[,1], time = split[,2])
  return(df)
}


file <- "~/Documents/syncthing/school/summerResearch/data/availDemand/events2019.csv"

pickups <- read_csv(file) %>%
  cleanEvents() %>%
  mapToTract() %>%
  splitTimeCol() %>%
  group_by(TRACT)


  

# Process
# 1. Import
# 2. Map lat/long to tract
# 3. Clean
# 4. Group by tract
# 5. Find # of items for each  date
# 6. Save