# Install relevant libraries ---------------------------------------------------
library(ggplot2)
library(igraph)
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
    # Remove infrequent trips
    group_by(from, to) %>%
    summarise(start_lat = mean(start_lat), 
              start_long = mean(start_long),
              end_lat = mean(end_lat),
              end_long = mean(end_long),
              count = n()) %>%
    filter(count >= 5)
  return(data)
}

dataYear <- cleanData(tripsYear1WithTracts, start_date = "2018-10-17", end_date = "2019-09-19")

# Use spectral clustering to group by geographical information -----------------
clusterByGeo <- function(data, numClusters) {
  # Summarize data by geographical information
  data <- data %>%
    group_by(from) %>%
    summarise(lat = mean(start_lat), 
              long = mean(start_long)) %>%
    select(lat, long)
  # Create groups using spectral clustering
  sc <- specc(as.matrix(data), centers = numClusters)
  data <- data %>% 
    mutate(from = paste("(", lat, ", ", long, ")", sep = ""), 
           sc = as.factor(sc)) %>%
    select(from, lat, long, sc)
  # Track number of nodes per cluster
  numNodes <- data %>%
    group_by(sc) %>%
    summarise(count = n()) 
  numNodes <- numNodes$count
  return(list(clusters = data, numNodes = numNodes))
}

numGeo <- 8
geoYear <- clusterByGeo(dataYear, numClusters = numGeo)

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
  data <- usageData[-1:-3]
  # Create groups using spectral clustering
  sc <- specc(as.matrix(data), centers = numClusters)
  data <- data %>% 
    mutate(sc = as.factor(sc), 
           lat = usageData$lat,
           long = usageData$long)
  # Track number of nodes per cluster
  
  numNodes <- data %>%
    group_by(sc) %>%
    summarise(count = n()) 
  numNodes <- numNodes$count
  return(list(clusters = data, numNodes = numNodes))
}

numUsage <- 7
usageYear <- clusterByUsage(dataYear, geoYear$clusters, numClusters = numUsage)

# Adjust pattern clustering result to obtain numGeo clusters -------------------
splitClusters <- function(data, numGeo, numUsage) {
  for (i in 1:(numGeo-numUsage)) {
    # Find biggest cluster in pattern clustering result
    max <- which.max(data$numNodes)
    clusterData <- data$clusters %>%
      filter(sc == max) %>%
      mutate(start_lat = lat,
             start_long = long,
             from = paste("(", lat, ", ", long, ")", sep = ""))
    # Use spectral clustering to split it into two based on geographical information
    clusterData <- clusterByGeo(clusterData, 2)
    # Combine clustering result with the original pattern clustering result
    clusterData <- clusterData$clusters
    clusterData$sc <- as.character(clusterData$sc)
    clusterData$sc[clusterData$sc == "2"] <- length(data$numNodes)+1
    clusterData$sc[clusterData$sc == "1"] <- max
    clusterData <- clusterData %>%
      select(lat, long, sc)
    data <- data$clusters %>%
      filter(sc != max) %>%
      select(lat, long, sc)
    data <- rbind(data, clusterData)
    # Track number of nodes per cluster
    numNodes <- data %>%
      group_by(sc) %>%
      summarise(count = n()) 
    numNodes <- numNodes$count
    # Replace original pattern clustering result with new clustering result
    data <- (list(clusters = data, numNodes = numNodes))
  } 
  return(data)
}

splitYear <- splitClusters(usageYear, numGeo, numUsage)

# Use LPA to make clustering result more reasonable ----------------------------
g <- graph_from_data_frame(splitYear$clusters[1:2], directed = FALSE)
labels <- as.numeric(unlist(splitYear$clusters[3]))
c <- cluster_label_prop(g, initial = labels)


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
    geom_point(data = data$clusters, aes(x = long, y = lat, color = sc), size = 1) + #Color clusters
    # Label plot
    scale_color_discrete(name = "Number of Nodes per Cluster", labels = data$numNodes) +
    guides(color = guide_legend(ncol = 1)) +
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
plotYearSplit <- createPlot(splitYear, "Usage pattern clustering split by geographical information")

# Save plots -------------------------------------------------------------------
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
# dir <- "/Users/Alice/Dropbox/pvd_summer"
filenames <- c("Spectral_clusters_by_geo", "Spectral_clusters_by_usage_split", "Spectral_clusters_by_usage")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}
