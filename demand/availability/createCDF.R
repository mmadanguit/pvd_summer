library(ggplot2)
library(lubridate)
library(tidyverse)

# Import trip data ---------------------------------------------------
dir <- "~/Documents/syncthing/school/summerResearch/data/availDemand"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
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
    # Convert time to seconds
    mutate(startSec = period_to_seconds(hms(startTime))) %>%
    # Consider only trips that began within operating hours
    filter(startSec >= 6*60*60 & startSec < 22*60*60) %>%
    # Select only relevant columns
    select(date, startTime, startSec)

# Plot hourly average usage ----------------------------------------------------
usageData <- tripData %>%
  mutate(startSec = round(startSec, digits = -4)) %>%
  group_by(date, startSec) %>%
  summarise(count = n()) %>%
  group_by(startSec) %>%
  summarise(count = mean(count))

usagePlot <- ggplot(usageData, aes(x = startSec, y = count, group = 1)) +
  geom_line() + 
  labs(title = "Hourly Average Scooter Usage")

# Create CDF -------------------------------------------------------------------
cdf <- ecdf(tripData$startSec)
plot(cdf)

# smoothFunc <- function(adj = 0.1) {
#   "Smooth cdf by generating a kernel density estimate"
#   d = tripData$startSec
#   # Generate kernel density estimate of data
#   dens = density(d, adjust = adj, from = min(d), to = max(d))
#   dens = data.frame(x = dens$x, y = dens$y)
#   # Plot kernel density
#   ggplot(as.data.frame(d), aes(d)) + 
#     geom_line(data = dens, aes(x = x, y = cumsum(y)/sum(y))) +
#     labs(title = paste("adj =", adj))
#   return(data.frame(x = dens$x, y = cumsum(dens$y)/sum(dens$y)))
# }
# 
# cdfSmooth <- function(times){
#   "Calculates cd estimates based on smoothed cdf
#   times: vector storing trip times in seconds"
#   d <- smoothFunc()
#   estimates <- c()
#   for (i in 1:length(times)) {
#     ind <- which.min(abs(d$x-times[i])) # Find closest value
#     estimates[i] <- d$y[ind]
#   }
#   return(estimates)
# }