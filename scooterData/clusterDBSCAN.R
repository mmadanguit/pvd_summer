#Install relevant libraries
library(dbscan)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(tigris)
library(sf)

#Import trip data
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
# dir <- "/Users/nolan/Documents"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# #Import census tract data
dir <- "/home/marion/PVDResearch/PVDResearch/censusData"
# dir <- "./censusData"
filename <- "riData"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

#Choose random sampling of trip data to cluster 
sampleData <- function(data){
  data %>%
    mutate(start_time = as.POSIXct(start_time, tz = "EST")) %>%
    filter(minutes >= 3) %>%
    sample_n(100)
}

data1 <- sampleData(tripsYear1WithTracts)
data2 <- sampleData(tripsYear1WithTracts)
data3 <- sampleData(tripsYear1WithTracts)
data4 <- sampleData(tripsYear1WithTracts)

createClusters <- function(data, EPS, MINPTS){
  #Store start coordinates and end coordinates in same column
  trips <- data %>% select(trip_id, lat = start_latitude, long = start_longitude)
  
  #Run the DBSCAN algorithm on the coordinates
  clusters <- dbscan(select(trips, lat, long), eps = EPS, minPts = MINPTS)
  trips$cluster <- clusters$cluster
  
  #Split the original data into two according to whether DBSCAN has assigned as cluster or noise
  groups <- trips %>% filter(cluster != 0)
  noise <- trips %>% filter(cluster == 0)
  
  list(trips, groups, noise)
}

#Cluster different samplings of data
cluster1 <- createClusters(data1, EPS = 0.005, MINPTS = 3)
cluster2 <- createClusters(data2, EPS = 0.005, MINPTS = 3)
cluster3 <- createClusters(data3, EPS = 0.005, MINPTS = 3)
cluster4 <- createClusters(data4, EPS = 0.005, MINPTS = 3)

#Cluster with different parameters
cluster5 <- createClusters(data2, EPS = 0.002, MINPTS = 3)
cluster6 <- createClusters(data2, EPS = 0.002, MINPTS = 2)
cluster7 <- createClusters(data2, EPS = 0.0015, MINPTS = 3)
cluster8 <- createClusters(data2, EPS = 0.0015, MINPTS = 2)

createPlot <- function(cluster, EPS, MINPTS){
  #Get map of Providence County census tracts
  censusTracts <- tracts("RI", class = "sf") %>%
    select(GEOID) %>%
    filter(GEOID %in% riData$GEOID)
  
  #Plot clusters over map of census tracts
  ggplot(censusTracts) +
    geom_sf() +
    geom_point(data = data.frame(cluster[1]), aes(x = long, y = lat, alpha = 0.5)) + #Plot sampling of trip data
    geom_point(aes(x = long, y = lat, colour = as.factor(cluster)), data.frame(cluster[2]), size = 0.75) + #Color clusters
    #geom_point(aes(x = long, y = lat, fill = "grey"), data.frame(cluster[3])) + #Color outliers
    theme_bw() + #Remove gray background
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #Remove grid
    theme(legend.position = "none") + #Remove legend
    theme(axis.text.x = element_text(angle = 90)) + #Rotate x axis labels
    ggtitle(paste("EPS = ", EPS, " and MINPTS = ", MINPTS, sep = "")) + 
    theme(plot.title = element_text(size = 7))
}

#Plot different samplings of trip data
plot1 <- createPlot(cluster1, EPS = 0.005, MINPTS = 3)
plot2 <- createPlot(cluster2, EPS = 0.005, MINPTS = 3)
plot3 <- createPlot(cluster3, EPS = 0.005, MINPTS = 3)
plot4 <- createPlot(cluster4, EPS = 0.005, MINPTS = 3)

diffSamplings <- ggarrange(plot1, plot2, plot3, plot4,
                           ncol = 2, nrow = 2)
diffSamplings <- annotate_figure(diffSamplings, 
                                 top = text_grob("DBSCAN clusters on different samplings", face = "bold"))
                
#Plot one sampling of trip data with different parameters
plot5 <- createPlot(cluster5, EPS = 0.002, MINPTS = 3)
plot6 <- createPlot(cluster6, EPS = 0.002, MINPTS = 2)
plot7 <- createPlot(cluster7, EPS = 0.0015, MINPTS = 3)
plot8 <- createPlot(cluster8, EPS = 0.0015, MINPTS = 2)
plot9 <- createPlot(cluster9, EPS = 0.001, MINPTS = 3)

diffParameters <- ggarrange(plot5, plot6, plot7, plot8, 
                            ncol = 2, nrow = 2)
diffParameters <- annotate_figure(diffParameters, 
                                  top = text_grob("DBSCAN clusters using different parameters", face = "bold"))

#Save plots
plots <- mget(ls(pattern="diff"))
dir <- "/home/marion/PVDResearch/Plots"
filenames <- c("DBSCAN_clusters_using_different_parameters", "DBSCAN_clusters_on_different_samplings")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}