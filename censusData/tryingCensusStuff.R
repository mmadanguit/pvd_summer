library(tidyverse)
library(tidycensus)
census_api_key("03bb123468322b2ab83d39c381c860d859ad5bab",install = TRUE)
library(dplyr)
library(ggplot2)
ri_pop <- get_acs(geography = "tract",
                     variables = "B01003_001",
                     state = "RI",
                     county = "Providence County",
                     geometry = TRUE) %>%
  filter(GEOID<4400701000)

# ri_pop


p <- ri_pop %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  coord_sf(crs = 26911) +
  scale_fill_viridis_c(option = "magma")
print(p)

