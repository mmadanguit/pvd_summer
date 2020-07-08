# Install clustering functions -------------------------------------------------
path <- "/home/marion/PVDResearch/PVDResearch/scooterData/"
source(paste(path, "clusterSpectralAll.R", sep = ""))

# Set clustering parameters ----------------------------------------------------
numGeo <- 10 # Number of groups to create in geographic clustering
numUsage <- 8 # Number of groups to create in usage pattern clustering
neighbors <- 8 # Number of neighboring nodes to based relabeling off of 

# Determine optimal seed to minimize intra-cluster similarity ------------------
sim <- 1
seed <- 0
numTrial <- 100
for (i in 1:numTrial) {
  # Set random seed
  set.seed(i)
  # Run through clustering steps
  geo <- clusterByGeo(dataYear, numGeo)
  usage <- clusterByUsage(dataYear, geo$clusters, numUsage)
  split <- splitClusters(usage, numGeo, numUsage)
  relabel <- relabelClusters(split, numGeo, neighbors)
  # Store clustering result only if similarity value is less than all previous similarity values
  if (relabel$sim < sim) {
    sim <- relabel$sim
    geoYear <- geo
    usageYear <- usage
    splitYear <- split
    relabelYear <- relabel
    seed <- i
  }
}