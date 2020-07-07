library(tidyverse)
source("intervals.R")
setwd("~/Documents/github/pvd_summer/demand") # change to your WD

dailySampSize <- function(df){
  "Find the daily sample size for the dataframe and plot"
  sampSize <- df %>% group_by(startDate) %>% 
    summarize(startDate, n()) %>% distinct() %>%
    rename(SAMP = 'n()', DATE = 'startDate')
  sampSize$DATE <- as.Date(sampSize$DATE)
  sampSizeP <- ggplot(data=sampSize, aes(x=DATE, y=SAMP)) + geom_point() + 
    scale_x_date(date_label = "%b/%Y", date_breaks = "1 month")
  return(sampSizeP)
}

period <- as.character(
  seq(as.Date("2019-4-1"), as.Date("2019-10-31"), by = "day"))
locData <- locations_and_tract_and_rounded_latlong_from_oct18_oct19 %>%
  clean() %>% splitTimeCol()#  %>%
  # filter((startDate %in% period) & (endDate %in% period)) # restrict to period

# find the samp size for each day
sampSize <- dailySampSize(locData)
sampSize

# intervalData <- getIntervalData(locData, period)