library(tidyverse)
library(sf)
library(tidycensus)
library(leaflet)
library(mapview)
census_api_key("04304157f38cc4cfab56cff53eaf01c0e2577e86")

# setwd("~/Documents/github/pvd_summer/censusData")
geoData = readRDS("riDataGeo.Rds")
mapview(geoData) # map data needs GEOID, name, and geometry