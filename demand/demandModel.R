# Steps:
# Generate basic map
# look at Nolan's code
# load dataset csv
# basically Nolan's code

# Create summary tract data from csv 
#  load Marion csv
# find summaries for each tract, total number of rows in that tract

# Start map with summary data

# Resources:
# generating map with census data: https://map-rfun.library.duke.edu/02_choropleth.html

library(tidyverse)
library(sf)
library(tidycensus)
library(leaflet)
library(mapview)
census_api_key("04304157f38cc4cfab56cff53eaf01c0e2577e86")

# setwd("~/Documents/github/pvd_summer/censusData")
read.table("riPopGeo.table", sep=":")

nc_pop <-
  get_acs(geography = "tract",
          state = "RI",
          county = 'Providence County',
          variables = "B01003_001",
          geometry = TRUE)
data2 = nc_pop %>% select(c('GEOID','NAME','geometry'))
print(data2)
mapview(data2) # map data needs GEOID, name, and geometry