#Import relevant libraries
library(tidyverse)
library(reshape2)

#Import clean trip data
dir <- "/home/marion/PVDResearch/Data/tripData/cleanData"
filenames <- c("avgTripsProvider", "avgTripsRegion")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

#Plot average number of daily trips per month by provider
plotProvider <- ggplot(avgTripsProvider, aes(x = Date, y = avg, color = Provider, group = Provider)) + 
  geom_path() +
  ggtitle("Average Number of Daily Trips by Month by Provider") + 
  scale_colour_discrete("Providers") +
  ylab("Average Number of Daily Trips") + 
  xlab("Month") + 
  theme(axis.text.x = element_text(angle = 90))

#Plot average number of daily trips per month by region
plotRegion <- ggplot(avgTripsRegion, aes(x = Date, y = value, color = variable, group = variable)) +
  geom_path() + 
  ggtitle("Average Number of Daily Trips by Month by Region") + 
  scale_colour_discrete("Regions") +
  ylab("Average Number of Daily Trips") + 
  xlab("Month") + 
  theme(axis.text.x = element_text(angle = 90))

#Save plots
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
filenames <- c("avg_daily_trips_by_month_by_provider", "avg_daily_trips_by_month_by_region")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}