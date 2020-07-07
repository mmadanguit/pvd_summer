# Install relevant libraries ---------------------------------------------------
library(geosphere)
library(ggplot2)
library(sf)
library(tidyverse)
library(tigris)
library(kernlab)

# Import relevant functions ----------------------------------------------------
path <- "/home/marion/PVDResearch/PVDResearch/scooterData/"
source(paste(path, "clusterSpectralEvaluate.R", sep = ""))

# Declare global variables -----------------------------------------------------
# Declare number of groups to create in geographic clustering
numGeo <- 8
# Declare number of groups to create in usage pattern clustering
numUsage <- 6
# Declare number of neighboring nodes required to relabel a cluster
neighbors <- 8
neighborCutoff <- 5

# Import trip data -------------------------------------------------------------
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
# dir <- "/Users/Alice/Documents"
# dir <- "/Users/nolan/Documents"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# Import census tract data -----------------------------------------------------
dir <- "/home/marion/PVDResearch/PVDResearch/censusData"
# dir <- "/Users/Alice/Dropbox/pvd_summer/censusData"
# dir <- "./censusData"
filename <- "riData"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# Clean and filter trip data ---------------------------------------------------
cleanData <- function(data, start_date, end_date) {
  data <- data %>%
    # Consider only trips that last at least 3 minutes
    filter(minutes >= 3) %>% 
    # Select time range
    mutate(start_time = as.POSIXct(start_time, tz = "EST")) %>%
    filter(start_time > start_date & start_time < end_date) %>%
    # Round coordinates to the nearest 5/1000 place
    mutate(start_lat = 0.005*round(start_latitude/0.005, digits = 0),
           start_long = 0.005*round(start_longitude/0.005, digits = 0),
           end_lat = 0.005*round(end_latitude/0.005, digits = 0),
           end_long = 0.005*round(end_longitude/0.005, digits = 0),
           from = paste("(", start_lat, ", ", start_long, ")", sep = ""),
           to = paste("(", end_lat, ", ", end_long, ")", sep = "")) %>%
    # Consider only trips that occurred at least 5 times 
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

# Use spectral clustering to group by geographic information -------------------
numNodes <- function(data) {
  # Count number of nodes per cluster
  numNodes <- data %>%
    group_by(sc) %>%
    summarise(count = n()) 
  return(numNodes$count)
}

clusterByGeo <- function(data, numClusters) {
  # Summarize data by geographic information for clustering
  data <- data %>%
    group_by(from) %>%
    summarise(start_lat = mean(start_lat), 
              start_long = mean(start_long)) %>%
    select(start_lat, start_long)
  # Create groups using spectral clustering
  sc <- specc(as.matrix(data), centers = numClusters)
  data <- data %>% 
    mutate(from = paste("(", start_lat, ", ", start_long, ")", sep = ""), 
           sc = as.factor(sc)) 
  # Count number of nodes per cluster
  numNodes <- numNodes(data)
  # Calculate intra-cluster similarity based on geographic information
  clustersGeo <- data %>% 
    select(start_long, start_lat, sc)
  sim <- round(calculateSim(clustersGeo, numClusters), digits = 3)
  return(list(clusters = data, numNodes = numNodes, sim = sim))
}

geoYear <- clusterByGeo(dataYear, numGeo)

# Use spectral clustering to group by usage pattern ----------------------------
calculateUsage <- function(data, geoData) {
  # Map trip data to geographic clusters
  data$end_sc <- NA
  for (i in 1:nrow(geoData)) {
    coord <- geoData[i,]$from
    sc <- geoData[i,]$sc
    ind <- which(data$to == coord)
    data$end_sc[ind] <- sc
  } 
  # Remove trips whose end coordinates do not correspond to a geographic cluster
  data <- data[!is.na(data$end_sc),]
  # Count number of scooters that travel from each start coordinate to each cluster
  data <- data %>%
    group_by(from, end_sc) %>%
    summarise(start_lat = mean(start_lat), 
              start_long = mean(start_long), 
              count = n()) %>%
    spread(end_sc, count)
  # Replace NA values with 0
  data[is.na(data)] <- 0
  # Convert scooter counts to proportions
  data[-1:-3] <- round(data[-1:-3] / rowSums(data[-1:-3]), digits = 2)
  return(data)
}

clusterByUsage <- function(data, geoData, numClusters) {
  # Summarize data by usage pattern for clustering
  usageData <- calculateUsage(data, geoData) 
  data <- usageData %>%
    ungroup() %>%
    select(-c(from, start_long, start_lat))
  # Create groups using spectral clustering
  sc <- specc(as.matrix(data), centers = numClusters)
  data <- data %>% 
    mutate(start_lat = usageData$start_lat,
           start_long = usageData$start_long,
           from = paste("(", start_lat, ", ", start_long, ")", sep = ""),
           sc = as.factor(sc))
  # Count number of nodes per cluster
  numNodes <- numNodes(data)
  # Calculate intra-cluster similarity based on usage pattern
  clustersUsage <- data %>% 
    select(-c(start_lat, start_long, from))
  sim <- round(calculateSim(clustersUsage, numClusters), digits = 3)
  return(list(clusters = data, numNodes = numNodes, sim = sim))
}

usageYear <- clusterByUsage(dataYear, geoYear$clusters, numUsage)

# Adjust pattern clustering result to obtain numGeo clusters -------------------
splitClusters <- function(data, numGeo, numUsage) {
  # Keep for later use
  original <- data$clusters
  for (i in 1:(numGeo-numUsage)) {
    # Find biggest cluster in pattern clustering result
    max <- which.max(data$numNodes)
    clusterData <- data$clusters %>%
      filter(sc == max) 
    # Use spectral clustering to split it into two based on geographical information
    clusterData <- clusterByGeo(clusterData, 2)
    # Combine clustering result with the original pattern clustering result
    clusterData <- clusterData$clusters
    clusterData$sc <- as.character(clusterData$sc)
    clusterData$sc[clusterData$sc == "2"] <- length(data$numNodes)+1
    clusterData$sc[clusterData$sc == "1"] <- max
    clusterData <- clusterData %>%
      mutate(from = paste("(", start_lat, ", ", start_long, ")", sep = "")) %>%
      select(start_lat, start_long, from, sc)
    data <- data$clusters %>%
      filter(sc != max) %>%
      select(start_lat, start_long, from, sc)
    data <- rbind(data, clusterData)
    # Count number of nodes per cluster
    numNodes <- numNodes(data)
    # Replace original pattern clustering result with new clustering result
    data <- (list(clusters = data, numNodes = numNodes))
  } 
  # Merge usage pattern data with cluster data for similarity calculation
  data$clusters <- merge(data$clusters, 
                         select(original, -c(start_lat, start_long, sc)), 
                         by = "from")
  # Calculate intra-cluster similarity based on geographic information and usage pattern
  clustersBoth <- data$clusters %>% 
    select(-c(from))
  sim <- round(calculateSim(clustersBoth, numGeo), digits = 3)
  return(list(clusters = data$clusters, numNodes = data$numNodes, sim = sim))
}

splitYear <- splitClusters(usageYear, numGeo, numUsage)

# Use LPA to make clustering result more reasonable ----------------------------
mode <- function(x) {
  # Calculate mode of a set of data
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

relabelClusters <- function(data, numGeo, neighbors, neighborCutoff) {
  # Create distance matrix from coordinate nodes
  coord <- data$clusters %>%
    select(start_long, start_lat)
  dist <- as.data.frame(distm(coord, coord, distGeo))
  dist[dist == 0] <- NA
  # Initialize data frame to hold relabeled data
  relabeledData <- data$clusters
  # Loop through each coordinate node
  for (i in 1:nrow(data$clusters)) {
    # Determine 8 nearest nodes (neighbors), which for most, will be the 4 cardinal directions and 4 corners around points that are surrounded by other points
    ind <- sort(dist[,i], na.last = TRUE, index.return = TRUE)$ix
    ind <- ind[1:neighbors] 
    # Get the rows of the neighbors from the original data
    neighborNodes <- data$clusters[ind,]
    # Determine most common cluster label of neighbors
    cluster <- mode(neighborNodes$sc)
    # If the number of neighbors with the same cluster as the mode is above the neighbor cutoff value,
    if (nrow(neighborNodes %>% filter(sc == as.integer(cluster))) >= neighborCutoff) { 
      # Relabel selected node based on neighbors
      relabeledData$sc[i] <- as.integer(cluster)
    }
  }
  # Count number of nodes per cluster
  numNodes <- numNodes(relabeledData)
  # Calculate intra-cluster similarity based on geographic information and usage pattern
  clustersBoth <- relabeledData %>% 
    select(-c(from))
  sim <- round(calculateSim(clustersBoth, numGeo), digits = 3)
  return(list(clusters = relabeledData, numNodes = numNodes, sim = sim))
}

relabelYear <- relabelClusters(splitYear, numGeo, neighbors, neighborCutoff)

# Plot clusters ----------------------------------------------------------------
createPlot <- function(data, title, numGeo, numUsage){
  # Get map of Providence County census tracts
  censusTracts <- tracts("RI", class = "sf") %>%
    select(GEOID) %>%
    filter(GEOID %in% riData$GEOID)
  # Plot clusters over map of census tracts
  plot <- ggplot(censusTracts) +
    geom_sf() +
    # Plot clusters
    geom_point(data = data$clusters, aes(x = start_long, y = start_lat, color = as.factor(sc)), size = 2) + #Color clusters
    # Label plot
    scale_color_discrete(name = "Nodes per Cluster", labels = data$numNodes) +
    guides(color = guide_legend(ncol = 2)) +
    labs(title = title,
         subtitle = paste("numGeo =", numGeo, "and numUsage =", numUsage,
                          "\navgSimilarity =", data$sim)) +
    # Remove gray background
    theme_bw() + 
    # Remove grid
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    # Rotate x axis labels
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

plotYearGeo <- createPlot(geoYear, "Spectral clustering by \ngeographical information", numGeo, numUsage)
plotYearUsage <- createPlot(usageYear, "Spectral clustering \nby usage pattern", numGeo, numUsage)
plotYearSplit <- createPlot(splitYear, "Usage pattern clustering split \nby geographical information", numGeo, numUsage)
plotYearLPA <- createPlot(relabelYear, "Clustering result after LPA", numGeo, numUsage)

# Save plots -------------------------------------------------------------------
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
# dir <- "/Users/Alice/Dropbox/pvd_summer"
# dir <- "/Users/nolan/Dropbox/pvd_summer_plots"
filenames <- c("Spectral_clusters_by_geo_8", 
               "Spectral_cluster_after_LPA_8",
               "Spectral_clusters_by_usage_split_8", 
               "Spectral_clusters_by_usage_8")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}
