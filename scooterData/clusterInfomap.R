#Install relevant libraries
library(igraph)
library(ggplot2)
library(sf)
library(tidyverse)
library(tigris)

#Import trip data
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

#Import census tract data
dir <- "/home/marion/PVDResearch/PVDResearch/censusData"
filename <- "riData"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

#Clean and organize trip data to cluster
cleanData <- function(data){
  data %>%
    filter(minutes >= 3) %>%
    sample_n(100) %>% #Choose random sampling of trip data to cluster 
    mutate(from = paste("(", start_longitude, ", ", start_latitude, ")", sep = ""),
           to = paste("(", end_longitude, ", ", end_latitude, ")", sep = "")) %>%
    group_by(from, to) %>%
    summarise(long = start_longitude,
              lat = start_latitude,  
              weight = n())
}

data <- cleanData(tripsYear1WithTracts)

#Create graph for the algorithms
g <- graph_from_data_frame(data, directed = FALSE)

#Create clusters using Louvain algorithm
lc <- cluster_louvain(g)
clusters <- stack(membership(lc))
colnames(clusters) <- c("group", "coordinates")

#Map trip data to clusters
data$cluster <- NA

for (i in 1:nrow(clusters)){
  coord <- clusters[i,]$coordinates
  group <- clusters[i,]$group
  index <- which(data$from == coord)
  data$cluster[index] <- group
}

#Plot clusters
createPlot <- function(data){
  #Get map of Providence County census tracts
  censusTracts <- tracts("RI", class = "sf") %>%
    select(GEOID) %>%
    filter(GEOID %in% riData$GEOID)
  
  #Plot clusters over map of census tracts
  ggplot(censusTracts) +
    geom_sf() +
    geom_point(data = data, aes(x = long, y = lat, alpha = 0.5)) + #Plot sampling of trip data
    geom_point(aes(x = long, y = lat, colour = as.factor(cluster)), data, size = 1) + #Color clusters
    theme_bw() + #Remove gray background
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #Remove grid
    theme(legend.position = "none") + #Remove legend 
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Louvain clusters") +
    theme(plot.title = element_text(face = "bold"))
}

plotLouvain <- createPlot(data)

#Save plot
dir <- "/home/marion/PVDResearch/Plots"
filename <- "Louvain_clusters_100"
path <- file.path(dir, paste(filename, ".png", sep = ""))

ggsave(file = path, plot = plotLouvain)

#Evaluate clusters using modularity metric
modularity(lc)

#Infomap
imc <- cluster_infomap(g)
membership(imc)
communities(imc)
plot(lc, g)