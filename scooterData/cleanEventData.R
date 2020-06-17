#Import relevant libraries
library(arsenal)
library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)

#Import Remix event data
dir <- "/home/marion/PVDResearch/Data/mobilityData/originalData"
filenames <- c("vehicleEvents")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

#Clean and organize event data
cleanData <- function(data){
  data %>% 
    as_tibble() %>%
    filter(device_type %in% "electric_scooter" & event_type_reason %in% "user_drop_off") %>%
    mutate(event_date = substr(event_time, 1, 10), .after = provider) %>%
    mutate(event_time = substr(event_time, 12, 19)) %>%
    select(provider, event_date, event_time, lng, lat)
}

data <- cleanData(vehicleEvents)

#Get map of RI census tracts
censusTracts <- tracts("RI", class = "sf") %>%
  select(GEOID, TRACT = NAME)

#Convert event coordinates to same CRS as census tracts
eventCoords <- data.frame(lng = data$lng, lat = data$lat) %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(censusTracts))

#Map event coordinates to census tracts
eventTracts <- st_join(eventCoords, censusTracts)

#Remove redundant observation (determined using comparedf function from arsenal package)
eventTracts <- eventTracts[-(83353),]

#Add corresponding GEOIDs and census tracts to original data
data <- mutate(data, GEOID = eventTracts$GEOID, TRACT = eventTracts$TRACT)

#Save cleaned event data
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filename <- "vehicleEventsWithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))

write.csv(data, path)