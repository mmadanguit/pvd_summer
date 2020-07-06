# Install relevant libraries ---------------------------------------------------
library(geosphere)
library(ggplot2)
library(sf)
library(tidyverse)
library(tigris)
library(kernlab)

# Declare number of groups to create in initial and secondary clustering -------
numGeo <- 10
numUsage <- 7
neighbors <- 8
neighborCutoff <- 5

# Import trip data -------------------------------------------------------------
# dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
# dir <- "/Users/Alice/Documents"
dir <- "/Users/nolan/Documents"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# Import census tract data -----------------------------------------------------
# dir <- "/home/marion/PVDResearch/PVDResearch/censusData"
# dir <- "/Users/Alice/Dropbox/pvd_summer/censusData"
dir <- "./censusData"
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
numNodes <- function(data) {
  # Count number of nodes per cluster
  numNodes <- data %>%
    group_by(sc) %>%
    summarise(count = n()) 
  return(numNodes$count)
}

clusterByGeo <- function(data, numClusters) {
  # Summarize data by geographical information
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
  return(list(clusters = data, numNodes = numNodes))
}

geoYear <- clusterByGeo(dataYear, numClusters = numGeo)

# Use spectral clustering to group by usage pattern ----------------------------
calculateUsage <- function(data, geoData) {
  # Map trip data to geographical clusters
  data$end_sc <- NA
  for (i in 1:nrow(geoData)) {
    coord <- geoData[i,]$from
    sc <- geoData[i,]$sc
    ind <- which(data$to == coord)
    data$end_sc[ind] <- sc
  } 
  # Remove end coordinates that do not correspond to a geographical cluster
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
  # Summarize data by usage pattern
  usageData <- calculateUsage(data, geoData) 
  data <- usageData[-1:-3]
  # Create groups using spectral clustering
  sc <- specc(as.matrix(data), centers = numClusters)
  data <- data %>% 
    mutate(start_lat = usageData$start_lat,
           start_long = usageData$start_long,
           from = paste("(", start_lat, ", ", start_long, ")", sep = ""),
           sc = as.factor(sc))
  # Count number of nodes per cluster
  numNodes <- numNodes(data)
  return(list(clusters = data, numNodes = numNodes))
}

usageYear <- clusterByUsage(dataYear, geoYear$clusters, numClusters = numUsage)

# Adjust pattern clustering result to obtain numGeo clusters -------------------
splitClusters <- function(data, numGeo, numUsage) {
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
  return(data)
}

splitYear <- splitClusters(usageYear, numGeo, numUsage)

# Use LPA to make clustering result more reasonable ----------------------------
mode <- function(x) {
  # Calculate mode of a set of data
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

relabelClusters <- function(data) {
  # Create distance matrix from coordinate nodes
  coord <- data$clusters %>%
    select(start_long, start_lat)
  dist <- as.data.frame(distm(coord, coord, distGeo))
  dist[dist == 0] <- NA
  # Initialize data frame to hold relabeled data
  relabeledData <- data$clusters
  # Loop through each coordinate node
  for (i in 1:nrow(data$clusters)) {
    # Determine 8 nearest nodes (neighbors), which will be the 4 cardinal directions and 4 corners around points that are surrounded by other points
    ind <- sort(dist[,i], na.last = TRUE, index.return = TRUE)$ix
    ind <- ind[1:neighbors] 
    # Determine most common cluster label of neighbors
    cluster <- data$clusters[ind,] %>%
      summarise(mode = mode(sc))
    neighborNodes <- data$clusters[ind,] #Get the rows of the neighbors from the original data
    if(nrow(neighborNodes %>% filter(sc==as.integer(cluster)))>=neighborCutoff){ #If the number of neighbors with the same cluster as the mode is above the neighbor cutoff value,
      # Relabel selected node based on neighbors
      relabeledData$sc[i] <- cluster[[1]]
    }
  }
  # Count number of nodes per cluster
  numNodes <- numNodes(relabeledData)
  return(list(clusters = relabeledData, numNodes = numNodes))
}

relabelYear <- relabelClusters(splitYear)

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
    geom_point(data = data$clusters, aes(x = start_long, y = start_lat, color = as.factor(sc)), size = 1) + #Color clusters
    # Label plot
    scale_color_discrete(name = "Nodes per Cluster", labels = data$numNodes) +
    guides(color = guide_legend(ncol = 2)) +
    labs(title = title,
         subtitle = paste("numGeo =", numGeo, "and numUsage =", numUsage)) +
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
# dir <- "/home/marion/PVDResearch/Plots"
# dir <- "/Users/Alice/Dropbox/pvd_summer"
dir <- "/Users/nolan/Dropbox/pvd_summer_plots"
filenames <- c("Spectral_clusters_by_geo", 
               "Spectral_cluster_after_LPA",
               "Spectral_clusters_by_usage_split", 
               "Spectral_clusters_by_usage")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}
