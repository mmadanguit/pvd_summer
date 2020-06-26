library(tidyverse)
library(sf)

ri_pop_data <- read.csv("censusData/riData.csv") #Load Maeve's dataset of census data
ri_pop_geo <- readRDS("censusData/riDataGeo.Rds") #Load Maeve's dataset of geometry data
ri_pop <- merge(ri_pop_geo, ri_pop_data) #Merge the sf dataframe with just the geometry with the dataframe loaded from Maeve's CSV