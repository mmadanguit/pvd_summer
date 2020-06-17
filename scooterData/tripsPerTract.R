#Import relevant libraries
library(lubridate)
library(tidyverse)

#Import vehicle event data
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filenames <- c("vehicleEventsWithTracts")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

#Clean and organize event data 
cleanData <- function(data){
  data %>%
    mutate(event_time = paste(event_date, event_time)) %>%
    select(TRACT, event_time) %>%
    mutate(event_time = as.POSIXct(event_time, tz = "EST")) %>%
    mutate(event_time = floor_date(event_time, "hour"))
}

trips <- cleanData(vehicleEventsWithTracts)

#Calculate number of trips per hour per day per census tract
createStats <- function(data){
  data %>%
    group_by(TRACT, event_time) %>%
    count()
}

tripsPerTract <- createStats(trips)

#Save csv
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filename <- "tripsPerTract"
path <- file.path(dir, paste(filename, ".csv", sep = ""))

write.csv(tripsPerTract, path)