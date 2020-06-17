library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(stringr)


#Old stuff from when we were loading all from get_acs, now uing Maeve's dataset

# race_vars <- c(
#   White = "B03002_003", 
#   Black = "B03002_004", 
#   Native = "B03002_005", 
#   Asian = "B03002_006", 
#   HIPI = "B03002_007", 
#   Hispanic = "B03002_012",
#   
#   Age = "B01001_001"
#   )
# 
# ri_pop <- get_acs(geography = "tract", 
#                      state = "RI", 
#                      county = 'Providence County',
#                      variables = race_vars,
#                      geometry = TRUE,
#                   cache_table = TRUE) 
# 
# # reshape and select within city boundaries
# ri_pop = ri_pop %>% select(c('GEOID','NAME','variable','estimate')) %>%
#   spread(key='variable',value='estimate')


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

#write.csv(ri_pop,file='ri_data.csv')


# I believe this is all old stuff to do a Leaflet display, now in app.R
# pal <- colorQuantile(palette = "viridis", domain = ri_pop$White, n = 10)
# 
# # plot
# ri_pop %>%
#   st_transform(crs = "+init=epsg:4326") %>%
#   leaflet(width = "100%") %>%
#   addProviderTiles(provider = "CartoDB.Positron") %>%
#   addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
#               stroke = FALSE,
#               smoothFactor = 0,
#               fillOpacity = 0.7,
#               color = ~ pal(White)) %>%
#   addLegend("bottomright", 
#             pal = pal, 
#             values = ~ White,
#             title = "Population percentiles",
#             opacity = 1)
# 
