#Import relevant libraries
library(readxl)
library(sf)
library(tidyverse)
library(tigris)

#Import year 1 trip data
dir <- "/home/marion/PVDResearch/Data/mobilityData/originalData"
filename <- "tripsYear1"
path <- file.path(dir, paste(filename, ".xlsx", sep = ""))

assign(filename, read_xlsx(path))

#Omit rows missing end coordinates (21 total)
data <- tripsYear1[complete.cases(tripsYear1$end_latitude),]

#Get map of RI census tracts
censusTracts <- tracts("RI", class = "sf") %>%
  select(GEOID, TRACT = NAME)

#Convert start and end coordinates to same CRS as census tracts
startCoords <- data.frame(lng = data$start_longitude, lat = data$start_latitude) %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(censusTracts))
endCoords <- data.frame(lng = data$end_longitude, lat = data$end_latitude) %>%
  st_as_sf(coords = c("lng", "lat"), crs = st_crs(censusTracts))

#Map start and end coordinates to census tracts
startTracts <- st_join(startCoords, censusTracts)
endTracts <- st_join(endCoords, censusTracts)

#Add corresponding GEOIDs and census tracts to original data
data <- mutate(data, start_tract = startTracts$TRACT, end_tract = endTracts$TRACT)

#Save cleaned year 1 trip data
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))

write.csv(data, path)