library(sf)
library(tidyverse)
library(tigris)
# library(plyr)

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
  mappedTracts <- convCoord(df$lng, df$lat, tracts) #%>% # get right format
  df <- mutate(df, TRACT = as.numeric(st_within(mappedTracts, tracts))) # find tracts
  return(df)
}

mapToTract <- function(df){
  tracts <- getTracts("Ri")
  df <- mapCoord(df, tracts)
  return(df)
}

# LAT LNG
round_any <- function(x, accuracy) {
  round(x / accuracy) * accuracy
}

roundLatLng <- function(df){
  "Round lat lng"
 df <- df %>% 
   mutate(lat = round_any(df$lat, 0.005), lng = round_any(df$lng, 0.005))
 return(df)
}

fakeTract <- function(df){
  "Create fake tract number for rounded lat lng"
  # get decimal portion of lat/lngh  
  latD <- str_sub(as.character(df$lat), 4, -1) 
  lngD <- str_sub(as.character(df$lng), 5, -1)
  df$TRACT <- as.numeric(paste(latD, lngD, sep = "."))
  return(df)
}

undoFakeTract <- function(df){
  "Undo fake tract into seperate columns again"
  a <- str_split_fixed(as.character(df$TRACT), "\\.", 2)
  df$LAT <- as.numeric(paste("41", a[,1], sep="."))
  df$LNG <- as.numeric(paste("-71", a[,2], sep="."))
  print(df)
  return(df)
}
# file <- "~/Documents/syncthing/school/summerResearch/data/availDemand/locations.csv"
# locations <- read_csv(file)
# a <- roundLatLng(locations) %>% fakeTract()