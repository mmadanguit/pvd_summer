#Import relevant libraries
library(lubridate)
library(tidyverse)

#Import vehicle event data
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filename <- "vehicleEventsWithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))

assign(filename, read.csv(path))

#Clean and organize event data 
cleanData <- function(data){
  data %>%
    filter(event_type_reason %in% c("rebalance_drop_off", "rebalance_pick_up")) %>%
    select(TRACT, event_time, event_type_reason) %>%
    mutate(event_time = as.POSIXct(event_time, tz = "EST")) %>%
    filter(event_time <= "2019-09-20")
}

data <- cleanData(vehicleEventsWithTracts)

#Save csv
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filename <- "rebalanceYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))

write.csv(data, path)