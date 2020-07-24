library(ggplot2)
library(lubridate)
library(tidyverse)

# Import and clean trip data ---------------------------------------------------
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste0(filename, ".csv"))
assign(filename, read.csv(path))

tripData <- tripsYear1WithTracts %>%
    # Consider only trips that last at least 3 minutes
    filter(minutes >= 3) %>% 
    # Select time range
    mutate(startTime = as.POSIXct(start_time, tz = "EST")) %>%
    filter(startTime > "2018-10-17" & startTime < "2019-09-19") %>%
    # Separate start time into date and time
    mutate(date = as.Date(startTime),
           startTime = format(startTime, format = "%H:%M:%S")) %>%
    # Convert time to minutes
    mutate(startMin = period_to_seconds(hms(startTime))/60) %>%
    # Consider only trips that began within operating hours
    filter(startMin >= 6*60 & startMin < 22*60) %>%
    # Select only relevant columns
    select(date, startMin)

# Plot hourly average usage ----------------------------------------------------
usageData <- tripData %>%
  mutate(startMin = round(startMin, digits = -2)) %>%
  group_by(date, startMin) %>%
  summarise(count = n()) %>%
  group_by(startMin) %>%
  summarise(count = mean(count))

ggplot(usageData, aes(x = startMin, y = count, group = 1)) +
  geom_line() + 
  labs(title = "Hourly Average Scooter Usage")

# Create CDF -------------------------------------------------------------------
cdf <- ecdf(tripData$startMin)
plot(cdf)