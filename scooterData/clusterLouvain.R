#Install relevant libraries
library(igraph)
library(ggplot2)
library(ggpubr)
library(grid)
library(sf)
library(tidyverse)
library(tigris)

#Import trip data
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
# dir <- "/Users/Alice/Documents"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# #Import census tract data
dir <- "/home/marion/PVDResearch/PVDResearch/censusData"
# dir <- "/Users/Alice/Dropbox/pvd_summer/censusData"
filename <- "riData"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

#Clean and organize trip data to cluster
cleanData <- function(data, start_date, end_date){
  data %>%
    filter(minutes >= 3) %>% 
    #Select time range
    mutate(start_time = as.POSIXct(start_time, tz = "EST")) %>%
    filter(start_time > start_date & start_time < end_date) %>%
    #Create data frame of edges for clustering algorithm
    mutate(start_latitude = 0.005*round(start_latitude/0.005, digits = 0),
           start_longitude = 0.005*round(start_longitude/0.005, digits = 0),
           end_latitude = 0.005*round(end_latitude/0.005, digits = 0),
           end_longitude = 0.005*round(end_longitude/0.005, digits = 0)) %>%
    mutate(from = paste("(", start_latitude, ", ", start_longitude, ")", sep = ""),
           to = paste("(", end_latitude, ", ", end_longitude, ")", sep = "")) %>%
    group_by(from, to) %>%
    summarise(lat = start_latitude, 
              long = start_longitude,
              weight = n())
}

dataWeek <- cleanData(tripsYear1WithTracts, start_date = "2019-07-01", end_date = "2019-07-08")
dataMonth <- cleanData(tripsYear1WithTracts, start_date = "2019-07-01", end_date = "2019-08-01")
dataYear <- cleanData(tripsYear1WithTracts, start_date = "2018-10-17", end_date = "2019-09-19")
  
#Create clusters using Louvain algorithm
createClusters <- function(data){
  #Create graph for clustering algorithm
  g <- graph_from_data_frame(data, directed = FALSE)
  #Create clusters
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
  list(data, modularity = round(modularity(lc), digits = 2), numGroups = max(data$cluster))
}

dataWeek <- createClusters(dataWeek)
dataMonth <- createClusters(dataMonth)
dataYear <- createClusters(dataYear)

#Plot clusters
createPlot <- function(data, title){
  #Get map of Providence County census tracts
  censusTracts <- tracts("RI", class = "sf") %>%
    select(GEOID) %>%
    filter(GEOID %in% riData$GEOID)
  #Plot clusters over map of census tracts
  ggplot(censusTracts) +
    geom_sf() +
    geom_point(data = data.frame(data[1]), aes(x = long, y = lat, alpha = 0.5)) + #Plot sampling of trip data
    geom_point(aes(x = long, y = lat, colour = as.factor(cluster)), data.frame(data[1]), size = 2) + #Color clusters
    theme_bw() + #Remove gray background
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #Remove grid
    theme(legend.position = "none") + #Remove legend 
    theme(axis.text.x = element_text(angle = 90)) + #Rotate x axis labels
    labs(title = title,
         subtitle = paste("Modularity: ", data$modularity, 
                          "\n", "Clusters: ", data$numGroups,
                          sep = ""))
}

plotWeek <- createPlot(dataWeek, "One Week") 
plotMonth <- createPlot(dataMonth, "One Month")
plotYear <- createPlot(dataYear, "One Year")

plotOverall <- ggarrange(plotWeek, plotMonth, plotYear,
                         ncol = 3, nrow = 1,
                         heights = c(4, 4, 4))
plotOverall <- annotate_figure(plotOverall, 
                               top = text_grob("(with coordinates rounded to nearest hundredths place)"))
plotOverall <- annotate_figure(plotOverall, 
                               top = text_grob("Louvain clusters on different samplings", face = "bold"))

#Save plot
#dir <- "/Users/Alice/Dropbox/pvd_summer"
filename <- "Louvain_clusters_on_different_samplings"
path <- file.path(dir, paste(filename, ".png", sep = ""))

ggsave(file = path, plot = plotOverall)
