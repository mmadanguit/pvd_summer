#Install relevant libraries
library(igraph)
library(ggplot2)
library(sf)
library(tidyverse)
library(tigris)
library(kernlab)

#Import trip data
# dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
dir <- "/Users/nolan/Documents"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# #Import census tract data
# dir <- "/home/marion/PVDResearch/PVDResearch/censusData"
dir <- "./censusData"
filename <- "riData"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))
set.seed(44)
sampleData <- function(data){
  data %>%
    mutate(start_time = as.POSIXct(start_time, tz = "EST")) %>%
    filter(minutes >= 3) %>%
    sample_n(1000)
}

data1 <- sampleData(tripsYear1WithTracts)
data2 <- sampleData(tripsYear1WithTracts)
data3 <- sampleData(tripsYear1WithTracts)
data4 <- sampleData(tripsYear1WithTracts)

# createClusters <- function(data, EPS, MINPTS){
#   #Store start coordinates and end coordinates in same column
#   trips <- data %>% select(trip_id, lat = start_latitude, long = start_longitude)
#   
#   #Run the DBSCAN algorithm on the coordinates
#   clusters <- dbscan(select(trips, lat, long), eps = EPS, minPts = MINPTS)
#   trips$cluster <- clusters$cluster
#   
#   #Split the original data into two according to whether DBSCAN has assigned as cluster or noise
#   groups <- trips %>% filter(cluster != 0)
#   noise <- trips %>% filter(cluster == 0)
#   
#   list(trips, groups, noise)
# }
# 
# #Cluster different samplings of data
# cluster1 <- createClusters(data1, EPS = 0.005, MINPTS = 3)
# cluster2 <- createClusters(data2, EPS = 0.005, MINPTS = 3)
# cluster3 <- createClusters(data3, EPS = 0.005, MINPTS = 3)
# cluster4 <- createClusters(data4, EPS = 0.005, MINPTS = 3)
# 
# #Cluster with different parameters
# cluster5 <- createClusters(data1, EPS = 0.01, MINPTS = 3)
# cluster6 <- createClusters(data1, EPS = 0.005, MINPTS = 5)
# cluster7 <- createClusters(data1, EPS = 0.001, MINPTS = 3)

trips1 <- data1 %>% select(lat = start_latitude, long = start_longitude) %>% as.matrix()
trips2 <- data2 %>% select(lat = start_latitude, long = start_longitude) %>% as.matrix()
trips3 <- data3 %>% select(lat = start_latitude, long = start_longitude) %>% as.matrix()
trips4 <- data4 %>% select(lat = start_latitude, long = start_longitude) %>% as.matrix()

sc1 <- specc(trips1, centers=5)
sc2 <- specc(trips2, centers=5)
sc3 <- specc(trips3, centers=5)
sc4 <- specc(trips4, centers=5)
# sc1 <- kkmeans(trips1, centers=5)
# sc2 <- kkmeans(trips2, centers=5)
# sc3 <- kkmeans(trips3, centers=5)
# sc4 <- kkmeans(trips4, centers=5)
# plot(trips1,col=sc1, pch=16)
# plot(trips2,col=sc2, pch=16)
# plot(trips3,col=sc3, pch=16)
# plot(trips4,col=sc4, pch=16)
pal <- rainbow(5)
createPlot <- function(sc, trips){
  #Get map of Providence County census tracts
  censusTracts <- tracts("RI", class = "sf") %>%
    select(GEOID) %>%
    filter(GEOID %in% riData$GEOID)
  
  #Plot clusters over map of census tracts
  p <- ggplot(censusTracts) +
    geom_sf() +
    # geom_point(aes(x = trips[,1], y = trips[,2], colour = as.factor(sc %>% as.data.frame())), size = 1) + #Color clusters
    theme_bw() + #Remove gray background
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #Remove grid
    theme(legend.position = "none") + #Remove legend 
    theme(axis.text.x = element_text(angle = 90))
    # ggtitle("Louvain clusters") +
    # theme(plot.title = element_text(face = "bold"))
  p <- p + geom_point(data = data.frame(sc %>% as.data.frame(), trips[,2], trips[,1]), aes(x = trips[,2], y = trips[,1], alpha = 0.5, color = 1))
  return(p)
}

#Plot different samplings of trip data
plot1 <- createPlot(sc1, trips1)
plot2 <- createPlot(sc2, trips2)
plot3 <- createPlot(sc3, trips3)
plot4 <- createPlot(sc4, trips4)
# 
# diffSamplings <- ggarrange(plot1, plot2, plot3, plot4,
#                            ncol = 2, nrow = 2)
# diffSamplings <- annotate_figure(diffSamplings, 
#                                  top = text_grob("DBSCAN clusters on different samplings", face = "bold"))
# 
# #Plot one sampling of trip data with different parameters
# plot5 <- createPlot(cluster5, EPS = 0.01, MINPTS = 3)
# plot6 <- createPlot(cluster6, EPS = 0.005, MINPTS = 5)
# plot7 <- createPlot(cluster7, EPS = 0.001, MINPTS = 3)
# 
# diffParameters <- ggarrange(plot5, plot6, plot1, plot7,
#                             ncol = 2, nrow = 2)
# diffParameters <- annotate_figure(diffParameters, 
#                                   top = text_grob("DBSCAN clusters using different parameters", face = "bold"))
# 
# #Save plots
# plots <- mget(ls(pattern="diff"))
# dir <- "/home/marion/PVDResearch/Plots"
# filenames <- c("DBSCAN_clusters_using_different_parameters", "DBSCAN_clusters_on_different_samplings")
# paths <- file.path(dir, paste(filenames, ".png", sep = ""))
# 
# for(i in 1:length(plots)){
#   invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
# }