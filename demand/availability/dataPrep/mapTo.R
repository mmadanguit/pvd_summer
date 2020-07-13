library(sf)
library(tidyverse)
library(tigris)
library(plyr)

# TRACT
getTracts <- function(state){
  censusTracts <- tracts(state, class = "sf") %>%
    select(GEOID, TRACT = NAME)
  return(censusTracts)
}

convCoord <- function(lng, lat, censusTracts){
  "Convert coordinates to same CRS as census tracts"
  coords <- tibble(lng, lat) %>%  
    st_as_sf(coords = c("lng", "lat"), crs = st_crs(censusTracts))
  return(coords)
}

mapCoord <- function(df, tracts){
  "Map coordinates to tract
  coords: coords to map
  censusTracts: tracts of the region"
  mappedTracts <- convCoord(df$lng, df$lat, tracts) %>% # get right format
    st_join(tracts) # find tracts
  df <- mutate(df, GEOID = mappedTracts$GEOID, 
               TRACT = as.numeric(mappedTracts$TRACT))
  return(df)
}

mapToTract <- function(df){
  tracts <- getTracts("Ri")
  df <- mapCoord(df, tracts)
  return(df)
}

# LAT LNG
roundLatLng <- function(df){
  "Round lat lng"
 df <- df %>% 
   add_column(latR = round_any(df$lat, 0.005), lngR = round_any(df$lng, 0.005)) %>%
   select(-c(lat, lng))
 return(df)
}

fakeTract <- function(df){
  "Create fake tract number for rounded lat lng"
  # get decimal portion of lat/lng
  latD <- str_sub(as.character(df$latR), 4, -1) 
  lngD <- str_sub(as.character(df$lngR), 5, -1)
  df$fakeTRACT <- as.numeric(paste(latD, lngD, sep = "."))
  return(df)
}
# file <- "~/Documents/syncthing/school/summerResearch/data/availDemand/locations.csv"
# locations <- read_csv(file)
# a <- roundLatLng(locations) %>% fakeTract()