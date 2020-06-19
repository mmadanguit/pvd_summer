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
  ggtitle("Average Number of Daily Trips by Provider") + 
  scale_colour_discrete("Providers") +
  ylab("Trips per Day") + 
  xlab("Month") + 
  scale_x_discrete(breaks = c("2018-07", "2018-09", "2018-11", "2019-01", "2019-03", "2019-05", "2019-07", "2019-09", "2019-11", "2020-01", "2020-03")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot average number of daily trips per month by region
plotRegion <- ggplot(avgTripsRegion, aes(x = Date, y = value, color = variable, group = variable)) +
  geom_path() + 
  ggtitle("Average Number of Daily Trips by Region") + 
  scale_colour_discrete("Regions") +
  ylab("Trips per Day") + 
  xlab("Month") + 
  scale_x_discrete(breaks = c("2018-07", "2018-09", "2018-11", "2019-01", "2019-03", "2019-05", "2019-07", "2019-09", "2019-11", "2020-01", "2020-03")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Save plots
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
filenames <- c("tripsProvider", "tripsRegion")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}