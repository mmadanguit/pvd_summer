library(tidyverse)

get_colleges <- function() {
  "
  make a tibble of different college buildings LAT LNG
  "
  colleges <- tibble(LAT = numeric(), LNG = numeric())
  colleges <- colleges %>%
    add_row(LAT = 41.845351, LNG = -71.439382) %>% # Providence College
    add_row(LAT = 41.843263, LNG = -71.439938) %>%
    add_row(LAT = 41.845980, LNG = -71.436430) %>%
    add_row(LAT = 41.846308, LNG = -71.433431) %>%
    add_row(LAT = 41.843905, LNG = -71.430307) %>%
    add_row(LAT = 41.841995, LNG = -71.430516) %>% 
    add_row(LAT = 41.819823, LNG = -71.413150) %>% # Johnson & Wales - Providence Campus
    add_row(LAT = 41.824997, LNG = -71.424496) %>% # Johnson & Wales - Online
    add_row(LAT = 41.826122, LNG = -71.407627) %>% # RISD
    add_row(LAT = 41.826879, LNG = -71.407774) %>%
    add_row(LAT = 41.826458, LNG = -71.409120) %>%
    add_row(LAT = 41.825717, LNG = -71.408387) %>%
    add_row(LAT = 41.823247, LNG = -71.413910) %>% # URI
    add_row(LAT = 41.826685, LNG = -71.404147) %>% # Brown University
    add_row(LAT = 41.825497, LNG = -71.403982) %>% 
    add_row(LAT = 41.824172, LNG = -71.403766) %>%
    add_row(LAT = 41.824029, LNG = -71.401868) %>%
    add_row(LAT = 41.824209, LNG = -71.400679) %>%
    add_row(LAT = 41.825396, LNG = -71.400977) %>%
    add_row(LAT = 41.826922, LNG = -71.401098)
  
  return(colleges)
}