# Install relevant libraries ---------------------------------------------------
library(igraph)
library(ggplot2)
library(grid)
library(sf)
library(tidyverse)
library(tigris)

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

# Clean and organize trip data to cluster --------------------------------------
cleanData <- function(data, start_date, end_date) {
  data %>%
    filter(minutes >= 3) %>% 
    # Select time range 
    mutate(start_time = as.POSIXct(start_time, tz = "EST")) %>%
    filter(start_time > start_date & start_time < end_date) %>%
    # Round coordinates
    mutate(start_latitude = 0.005*round(start_latitude/0.005, digits = 0),
           start_longitude = 0.005*round(start_longitude/0.005, digits = 0),
           end_latitude = 0.005*round(end_latitude/0.005, digits = 0),
           end_longitude = 0.005*round(end_longitude/0.005, digits = 0)) %>%
    # Create data frame of edges for clustering algorithm
    mutate(from = paste("(", start_latitude, ", ", start_longitude, ")", sep = ""),
           to = paste("(", end_latitude, ", ", end_longitude, ")", sep = "")) %>%
    group_by(from, to) %>%
    summarise(lat = start_latitude, 
              long = start_longitude,
              weight = n()) %>%
    # Remove infrequent trips
    filter(weight > 1)
}

dataWeek <- cleanData(tripsYear1WithTracts, start_date = "2019-07-01", end_date = "2019-07-08")
dataMonth <- cleanData(tripsYear1WithTracts, start_date = "2019-07-01", end_date = "2019-08-01")
dataYear <- cleanData(tripsYear1WithTracts, start_date = "2018-10-17", end_date = "2019-09-19")
  
# Create clusters using Louvain algorithm --------------------------------------
createClusters <- function(data){
  # Create graph for clustering algorithm
  g <- graph_from_data_frame(data, directed = FALSE)
  # Create clusters
  lc <- cluster_louvain(g)
  clusters <- stack(membership(lc))
  colnames(clusters) <- c("group", "coordinates")
  # Map trip data to clusters
  data$cluster <- NA
  for (i in 1:nrow(clusters)) {
    coord <- clusters[i,]$coordinates
    group <- clusters[i,]$group
    index <- which(data$from == coord)
    data$cluster[index] <- group
  } 
  # Track modularity, number of clusters, and number of nodes per cluster
  modularity = round(modularity(lc), digits = 2)
  numClusters = max(data$cluster)
  numNodes <- data.frame(data) %>%
    distinct(from, .keep_all = TRUE) %>%
    group_by(cluster) %>%
    summarise(count = n()) %>% 
    filter(count > 1)
  # Remove clusters with only one node
  filteredClusters <- numNodes$cluster
  data <- data %>% filter(cluster %in% filteredClusters)
    
  list(data, 
       modularity = modularity, 
       numClusters = numClusters, 
       numNodes = numNodes$count)
}

dataWeek <- createClusters(dataWeek)
dataMonth <- createClusters(dataMonth)
dataYear <- createClusters(dataYear)

# Plot clusters ----------------------------------------------------------------
createPlot <- function(data, title){
  # Get map of Providence County census tracts
  censusTracts <- tracts("RI", class = "sf") %>%
    select(GEOID) %>%
    filter(GEOID %in% riData$GEOID)
  # Plot clusters over map of census tracts
  ggplot(censusTracts) +
    geom_sf() +
    # Plot clusters
    geom_point(data = data.frame(data[1]), aes(x = long, y = lat, color = as.factor(cluster)), size = 2) + 
    # Label plot
    scale_color_discrete(name = "Number of Nodes per Cluster", labels = data$numNodes) +
    guides(color = guide_legend(ncol = 2)) +
    labs(title = title, 
         subtitle = paste("(with one-node clusters and infrequent trips filtered out)",
                          "\nModularity:", data$modularity, 
                          "\nClusters:", data$numClusters)) +
    # Remove gray background
    theme_bw() + 
    # Remove grid
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    # Rotate x axis labels
    theme(axis.text.x = element_text(angle = 90))
}

plotWeek <- createPlot(dataWeek, "Louvain clusters on one week of data") 
plotMonth <- createPlot(dataMonth, "Louvain clusters on one month of data")
plotYear <- createPlot(dataYear, "Louvain clusters on one year of data")

# Save plots -------------------------------------------------------------------
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
# dir <- "/Users/Alice/Dropbox/pvd_summer"
filenames <- c("Louvain_clusters_on_one_month_with_filtering", 
               "Louvain_clusters_on_one_week_with_filtering", 
               "Louvain_clusters_on_one_year_with_filtering")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}