library(tidyverse)
setwd("~/Documents/github/pvd_summer/demand/availability") # change to your WD
fol <- "~/Documents/syncthing/school/summerResearch/data/availDemand/"

start <- "2019-12-01"
end <- "2019-12-31"

availIntervals <- read_csv(paste(c(fol, "availIntervals2019.csv"), collapse='')) %>%
  # bind_rows(read_csv(paste(c(fol, "pickupsSummary2018.csv"), collapse=''))) %>%
  filter(DATE >= start & DATE <= end) %>%
  group_by(TRACT) %>% 
  arrange(DATE, .by_group=TRUE)
availTime <- availIntervals %>% 
  select(c(TRACT, DATE, AVAIL)) %>% # time w/o intervals
  group_by(DATE, .add = TRUE) %>% 
  summarize(AVAIL, AVAIL = sum(AVAIL)) %>% # find total avail for day
  distinct()

pickups <- read_csv(paste(c(fol, "pickupsSummary2018.csv"), collapse='')) %>%
  bind_rows(read_csv(paste(c(fol, "pickupsSummary2019.csv"), collapse=''))) %>%
  filter(DATE >= start & DATE <= end) %>%
  group_by(TRACT) %>% 
  arrange(DATE, .by_group=TRUE)
pickups <- pickups %>% filter(TRACT %in% availTime$TRACT) # filter to tracts which exist, due to limitide data issue
  

# demand <- 