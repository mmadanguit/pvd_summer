library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(stringr)


race_vars <- c(White = "B03002_003", Black = "B03002_004", Native = "B03002_005", Asian = "B03002_006"
               , HIPI = "B03002_007", Hispanic = "B03002_012")

ri_pop <- get_acs(geography = "tract", 
                     state = "RI", 
                     county = 'Providence County',
                     variables = race_vars,
                     geometry = TRUE,
                  cache_table = TRUE) 

# reshape and select within city boundaries
ri_pop = ri_pop %>% select(c('GEOID','NAME','variable','estimate')) %>%
  spread(key='variable',value='estimate')
ri_pop = ri_pop[ri_pop$GEOID < '44007003700',]

write.csv(ri_pop,file='ri_data.csv')

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
