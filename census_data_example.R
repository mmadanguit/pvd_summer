library(tidyverse)
library(sf)
library(lwgeom)

ri_pop_data <- read.csv("censusData/riData.csv") #Load Maeve's dataset of census data
ri_pop_geo <- readRDS("censusData/riDataGeo.Rds") #Load Maeve's dataset of geometry data
ri_pop <- merge(ri_pop_geo, ri_pop_data) #Merge the sf dataframe with just the geometry with the dataframe loaded from Maeve's CSV
ri_pop$area <- as.double(units::set_units(st_area(ri_pop$geometry), 'mi^2'))
ri_pop$pop_density <- ri_pop$Pop/ri_pop$area