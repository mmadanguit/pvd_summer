library(tidyverse)

get_downtown <- function() {
  "
  make a tibble of different downtown locations LAT LNG
  "
  colleges <- tibble(LAT = numeric(), LNG = numeric())
  colleges <- colleges %>%
    add_row(LAT = 41.831588, LNG = -71.416880) %>%
    add_row(LAT = 41.831140, LNG = -71.414069) %>%
    add_row(LAT = 41.830964, LNG = -71.410743) %>%
    add_row(LAT = 41.830061, LNG = -71.417459) %>%
    add_row(LAT = 41.829733, LNG = -71.413843) %>%
    add_row(LAT = 41.829677, LNG = -71.410066) %>%
    add_row(LAT = 41.828382, LNG = -71.417651) %>%
    add_row(LAT = 41.828174, LNG = -71.414347) %>%
    add_row(LAT = 41.828134, LNG = -71.409540) %>%
    add_row(LAT = 41.826054, LNG = -71.418369) %>%
    add_row(LAT = 41.825914, LNG = -71.414085) %>%
    add_row(LAT = 41.826113, LNG = -71.408494) %>%
    add_row(LAT = 41.824026, LNG = -71.419289) %>%
    add_row(LAT = 41.823776, LNG = -71.414988) %>%
    add_row(LAT = 41.823733, LNG = -71.410950) %>%
    add_row(LAT = 41.823505, LNG = -71.406608) %>%
    add_row(LAT = 41.819868, LNG = -71.418772) %>%
    add_row(LAT = 41.820157, LNG = -71.414604) %>%
    add_row(LAT = 41.820374, LNG = -71.409957) %>%
    add_row(LAT = 41.820390, LNG = -71.406535) %>%
    add_row(LAT = 41.817476, LNG = -71.415103) %>%
    add_row(LAT = 41.818401, LNG = -71.410164) %>%
    add_row(LAT = 41.819062, LNG = -71.405755) %>%
    add_row(LAT = 41.815808, LNG = -71.411938) %>%
    add_row(LAT = 41.816560, LNG = -71.408204) %>%
    add_row(LAT = 41.817767, LNG = -71.404760)
  
  return(colleges)
}