library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(stringr)


ri_pop_data <- read.csv("censusData/riData.csv") #Load Maeve's dataset
ri_pop_geo <- get_acs(geography = "tract", #Load a single variable from get_acs because Maeve's dataset does not contain the geodata
                      state = "RI", 
                      county = 'Providence County',
                      variables = "B03002_003",
                      geometry = TRUE,
                      cache_table = TRUE) 
ri_pop_geo = ri_pop_geo[ri_pop_geo$GEOID < 44007010000,] %>% #Only get the census tracts within Providence City
  select(c('GEOID','geometry')) #Only keep the GEOID, which corresponds to the census tract, and the geometry
ri_pop <- merge(ri_pop_geo, ri_pop_data) #Merge the sf dataframe with just the geometry with the dataframe loaded from Maeve's CSV