# Install relevant libraries ---------------------------------------------------
library(ggplot2)
library(sf)
library(tidyverse)
library(tigris)
library(kernlab)

# Import trip data -------------------------------------------------------------
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
# dir <- "/Users/Alice/Documents"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# Import census tract data -----------------------------------------------------
dir <- "/home/marion/PVDResearch/PVDResearch/censusData"
# dir <- "/Users/Alice/Dropbox/pvd_summer/censusData"
filename <- "riData"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# Clean and filter trip data ---------------------------------------------------
cleanData <- function(data, start_date, end_date) {
  data <- data %>%
    filter(minutes >= 3) %>% 
    # Select time range
    mutate(start_time = as.POSIXct(start_time, tz = "EST")) %>%
    filter(start_time > start_date & start_time < end_date) %>%
    # Round coordinates
    mutate(start_lat = 0.005*round(start_latitude/0.005, digits = 0),
           start_long = 0.005*round(start_longitude/0.005, digits = 0),
           end_lat = 0.005*round(end_latitude/0.005, digits = 0),
           end_long = 0.005*round(end_longitude/0.005, digits = 0),
           from = paste("(", start_lat, ", ", start_long, ")", sep = ""),
           to = paste("(", end_lat, ", ", end_long, ")", sep = "")) %>%
    select(from, to, start_lat, start_long, end_lat, end_long)
  return(data)
}

dataYear <- cleanData(tripsYear1WithTracts, start_date = "2018-10-17", end_date = "2019-09-19")

# Use spectral clustering to group by geographical information -----------------
clusterByGeo <- function(data, numClusters) {
  # Summarize data by geographical information
  data <- data %>%
    group_by(from) %>%
    summarise(lat = mean(start_lat), 
              long = mean(start_long),
              weight = 0.1*n()) %>%
    select(lat, long, weight)
  # Create groups using spectral clustering
  sc <- specc(as.matrix(data), centers = numClusters)
  data <- data %>% 
    mutate(from = paste("(", lat, ", ", long, ")", sep = ""), 
           sc = as.factor(sc)) %>%
    select(from, lat, long, weight, sc)
  return(data)
}

geoYear <- clusterByGeo(dataYear, numClusters = 8)

# Use spectral clustering to group by usage pattern ----------------------------
calculateUsage <- function(data, geoData) {
  # Map trip data to geographical clusters
  data$end_sc <- NA
  for (i in 1:nrow(geoData)) {
    coord <- geoData[i,]$from
    sc <- geoData[i,]$sc
    index <- which(data$to == coord)
    data$end_sc[index] <- sc
  } 
  # Remove end coordinates that do not correspond to a geographical cluster
  # HELP: Is there a way to group these in with a nearby cluster so that we don't have to get rid of them?
  data <- data[!is.na(data$end_sc),]
  # Count number of scooters that travel from each start coordinate to each cluster
  data <- data %>%
    group_by(from, end_sc) %>%
    summarise(lat = mean(start_lat), 
              long = mean(start_long), 
              count = n()) %>%
    spread(end_sc, count)
  # Replace NA values with 0
  data[is.na(data)] <- 0
  # Convert scooter counts to proportions
  data[-1:-3] <- round(data[-1:-3] / rowSums(data[-1:-3]), digits = 2)
  # Combine scooter proportions into a single string
  # data <- unite(data, usage, -from, sep = ", ")
  return(data)
}

clusterByUsage <- function(data, geoData, numClusters) {
  # Summarize data by usage pattern
  usageData <- calculateUsage(data, geoData) 
  clusterData <- usageData[-1:-3]
  # Create groups using spectral clustering
  sc <- specc(as.matrix(clusterData), centers = numClusters)
  usageData <- clusterData %>% 
    mutate(sc = as.factor(sc), 
           lat = usageData$lat,
           long = usageData$long)
  return(usageData)
}

usageYear <- clusterByUsage(dataYear, geoYear, numClusters = 7)

# Cluster splitting ------------------------------------------------------------
# TO DO

# Cluster label renewing -------------------------------------------------------
# TO DO

# Plot clusters ----------------------------------------------------------------
createPlot <- function(data, title){
  # Get map of Providence County census tracts
  censusTracts <- tracts("RI", class = "sf") %>%
    select(GEOID) %>%
    filter(GEOID %in% riData$GEOID)
  # Plot clusters over map of census tracts
  plot <- ggplot(censusTracts) +
    geom_sf() +
    # Plot clusters
    geom_point(data = data, aes(x = long, y = lat, color = sc), size = 1) + #Color clusters
    # Label plot
    scale_color_discrete(name = "Clusters") +
    labs(title = title) +
    # Remove gray background
    theme_bw() + 
    # Remove grid
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    # Rotate x axis labels
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

plotYearGeo <- createPlot(geoYear, "Spectral clustering by geographical information")
plotYearUsage <- createPlot(usageYear, "Spectral clustering by usage pattern")


