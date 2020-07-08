# Install clustering functions -------------------------------------------------
path <- "/home/marion/PVDResearch/PVDResearch/scooterData/"
source(paste(path, "clusterSpectralAll.R", sep = ""))

# Calculate intra-cluster similarity for multiple numGeo values ----------------
calculateSimGeo <- function(numTrials, numGeo) {
  # Initialize dataframe to store data
  data <- data.frame(matrix(ncol = numTrials+1, nrow = numGeo-1))
  colnames(data) <- c("num", 1:numTrials)
  data$num <- 2:numGeo
  for (i in 1:numTrials){
    for (j in 2:numGeo) {
      # Create clusters
      clustersGeo <- clusterByGeo(dataYear, j)$clusters
      clustersGeo <- clustersGeo %>% 
        select(start_long, start_lat, sc)
      # Calculate average intra-cluster similarity over all clusters
      sim <- calculateSim(clustersGeo, j)
      data[j-1, i+1] <- sim
    }
  }
  # Calculate average intra-cluster similarity over all trials
  data$avgSim = rowMeans(data[,-1])
  return(data)
}

numGeo <- 25
dataGeo <- calculateSimGeo(10, numGeo)

# Calculate intra-cluster similarity for multiple numUsage values --------------
calculateSimUsage <- function(numTrials, numGeo, numUsage) {
  # Initialize dataframe to store data
  data <- data.frame(matrix(ncol = numTrials+1, nrow = numUsage-1))
  colnames(data) <- c("num", 1:numTrials)
  data$num <- 2:numUsage
  for (i in 1:numTrials){
    clustersGeo <- clusterByGeo(dataYear, numGeo)$clusters
    for (j in 2:numUsage) {
      # Create clusters
      clustersUsage <- clusterByUsage(dataYear, clustersGeo, j)$clusters
      clustersUsage <- clustersUsage %>% 
        select(-c(start_lat, start_long, from))
      # Calculate average distance over all clusters
      sim <- calculateSim(clustersUsage, j)
      data[j-1, i+1] <- sim
    }
  }
  data$avgSim = rowMeans(data[,-1])
  return(data)
}

numGeo <- 6
numUsage <- numGeo-1
dataUsage_6 <- calculateSimUsage(30, numGeo, numUsage)

numGeo <- 8
numUsage <- numGeo-1
dataUsage_8 <- calculateSimUsage(30, numGeo, numUsage)

numGeo <- 10
numUsage <- numGeo-1
dataUsage_10 <- calculateSimUsage(30, numGeo, numUsage)

# Plot intra-cluster similarity vs number of clusters --------------------------
createPlot <- function(data, type) {
  ggplot(data, aes(x = num, y = avgSim)) + 
    geom_path() +
    labs(title = paste("Finding the optimal", type, "value"),
         subtitle = "Using Euclidean distance to measure intra-cluster similarity") +
    xlab(type) +
    scale_x_continuous(breaks = seq(2, numGeo, by = 2))
}

plotGeo <- createPlot(dataGeo, "numGeo")
plotUsage_6 <- createPlot(dataUsage_6, "numUsage") +
  labs(subtitle = paste("Using Euclidean distance to measure intra-cluster similarity and numGeo = 6"))
plotUsage_8 <- createPlot(dataUsage_8, "numUsage") +
  labs(subtitle = paste("Using Euclidean distance to measure intra-cluster similarity and numGeo = 8"))
plotUsage_10 <- createPlot(dataUsage_10, "numUsage") +
  labs(subtitle = paste("Using Euclidean distance to measure intra-cluster similarity and numGeo = 10"))

# Save plots -------------------------------------------------------------------
dir <- "/home/marion/PVDResearch/Plots"
filename <- "Finding_the_optimal_numGeo_value_using_euclidean"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path, plotGeo)

filename <- "Finding_the_optimal_numUsage_value_using_euclidean_6"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path, plotUsage_6)

filename <- "Finding_the_optimal_numUsage_value_using_euclidean_8"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path, plotUsage_8)

filename <- "Finding_the_optimal_numUsage_value_using_euclidean_10"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path, plotUsage_10)