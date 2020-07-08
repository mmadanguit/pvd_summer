# Calculate intra-cluster similarity using Euclidean distance ------------------
calculateSim <- function(clusters, i) {
  points <- clusters %>% filter(sc == i)
  points <- points[,!names(points) %in% "sc"]
  # Create distance matrix between all points in cluster
  # dist <- as.data.frame(distm(points, points, distGeo))
  dist <- as.matrix(dist(points))
  dist[dist == 0] <- NA
  # For each point, find average distance to other points in the cluster
  pointAvg <- rowMeans(dist, na.rm = TRUE)
  # Find average distance over all points
  clusterAvg <- mean(pointAvg)
  return(clusterAvg)
}

avgSim <- function(clusters, numClusters) {
  # Initialize vector to store each cluster's average distance
  avgs <- c()
  # Loop through each cluster
  for (i in 1:numClusters) { 
    clusterAvg <- calculateSim(clusters, i)
    avgs[i] <- clusterAvg
  }
  # Replace one-node cluster NA distances with 0 
  avgs[is.na(avgs)] <- 0
  # Return average distance over all clusters
  return(mean(avgs))
}