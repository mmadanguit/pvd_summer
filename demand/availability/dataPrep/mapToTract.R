library(sf)
library(tidyverse)
library(tigris)

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