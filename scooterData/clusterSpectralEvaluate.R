# Install relevant libraries ---------------------------------------------------
path <- "/home/marion/PVDResearch/PVDResearch/scooterData/"
source(paste(path, "clusterSpectralAll.R", sep = ""))

# Determine optimal numGeo based on intra-cluster similarity -------------------
calculateSim <- function(clusters, numGeo) {
  # Initialize vector to store each cluster's average distance
  avgs <- c()
  # Loop through each cluster
  for (i in 1:numGeo) { 
    points <- clusters %>% filter(sc == i)
    # Create distance matrix between all points in cluster
    coord <- points %>%
      select(start_long, start_lat)
    dist <- as.data.frame(distm(coord, coord, distGeo))
    # dist <- dist(coord, upper = TRUE)
    # dist[dist == 0] <- NA
    # For each point, find average distance to other points in the cluster
    pointAvg <- rowMeans(dist, na.rm = TRUE)
    # Find average distance over all points
    clusterAvg <- mean(pointAvg)
    avgs[i] <- clusterAvg
  }
  # Return average distance over all clusters
  return(0.01*mean(avgs))
}

clusters <- clusterByGeo(dataYear, 3)$clusters
# Calculate average distance over all clusters
sim <- calculateSim(clusters, 3)

# Initialize data frame to store similarity values
data <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(data) <- c("numGeo", "sim")
# Loop through numGeo values 
for (i in 2:25) {
  # Create numGeo clusters
  clusters <- clusterByGeo(dataYear, i)$clusters
  # Calculate average distance over all clusters
  sim <- calculateSim(clusters, i)
  data[i-1, ] <- c(i, sim)
}

# Plot similarity vs number of clusters
ggplot(data, aes(x = numGeo, y = sim)) +
  geom_path() +
  labs(title = "Finding the optimal numGeo value",
       subtitle = "(Based on intra-cluster similarity using the geodesic distance)")

# Save plot
dir <- "/home/marion/PVDResearch/Plots"
filename <- c("Finding_the_optimal_numGeo_value_using_geodesic")
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path)