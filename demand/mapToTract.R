library(arsenal)
library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)


# read csv
loc_data <- read_csv("locations_for_multiple_providers_from_18-09-01_to_19-11-01.csv")

filtered_loc_data <- loc_data %>% filter(!grepl("unavailable", vehicle_status))  %>% 
  filter(!grepl("rebalance_drop_off", vehicle_status_reason)) %>% 
  filter(end_time-start_time != 0) %>% 
  filter(difftime(end_time, start_time, units = "mins") > 1)

#Get map of RI census tracts
censusTracts <- tracts("RI", class = "sf") %>%
  select(GEOID, TRACT = NAME)

#Convert event coordinates to same CRS as census tracts
eventCoords <- data.frame(lng = filtered_loc_data$lng, lat = filtered_loc_data$lat) %>%
               st_as_sf(coords = c("lng", "lat"), crs = st_crs(censusTracts))

#Map event coordinates to census tracts
eventTracts <- st_join(eventCoords, censusTracts)

#Add corresponding GEOIDs and census tracts to original data
data <- mutate(filtered_loc_data, GEOID = eventTracts$GEOID, TRACT = eventTracts$TRACT)

write_csv(data, "locations_and_tract_18-09-01_to_19-11-01.csv")