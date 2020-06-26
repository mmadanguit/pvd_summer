#Install relevant libraries
library(igraph)
library(ggplot2)
library(sf)
library(tidyverse)
library(tigris)
library(kernlab)

#Import trip data
# dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
dir <- "/Users/Alice/Documents"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# #Import census tract data
# dir <- "/home/marion/PVDResearch/PVDResearch/censusData"
dir <- "/Users/Alice/Dropbox/pvd_summer/censusData"
filename <- "riData"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

cleanData <- function(data, start_date, end_date){
  data %>%
    filter(minutes >= 3) %>% 
    #Select time range
    mutate(start_time = as.POSIXct(start_time, tz = "EST")) %>%
    filter(start_time > start_date & start_time < end_date) %>%
    #Create data frame of edges for clustering algorithm
    mutate(lat = 0.005*round(start_latitude/0.005, digits = 0),
           long = 0.005*round(start_longitude/0.005, digits = 0)) %>%
    select(c(lat, long)) %>%
    mutate(from = paste("(", lat, ", ", long, ")", sep = "")) %>%
    group_by(from) %>%
    summarise(weight= .1*n(),lat = mean(lat), long = mean(long)) %>%
    select(c(lat, long, weight))
}

#dataWeek <- cleanData(tripsYear1WithTracts, start_date = "2019-07-01", end_date = "2019-07-08")
#dataMonth <- cleanData(tripsYear1WithTracts, start_date = "2019-07-01", end_date = "2019-08-01")
dataYear <- cleanData(tripsYear1WithTracts, start_date = "2018-10-17", end_date = "2019-09-19")

#matWeek <- as.matrix(dataWeek)
#matMonth <- as.matrix(dataMonth)
matYear <- as.matrix(dataYear)

#scWeek <- specc(matWeek, centers=8)
#scMonth <- specc(matMonth, centers=8)
scYear <- specc(matYear, centers=8)

#dataWeek <- dataWeek %>% mutate(sc=as.factor(scWeek))
#dataMonth <- dataMonth %>% mutate(sc=as.factor(scMonth))
dataYear <- dataYear %>% mutate(sc=as.factor(scYear))

createPlot <- function(trips){
  #Get map of Providence County census tracts
  censusTracts <- tracts("RI", class = "sf") %>%
    select(GEOID) %>%
    filter(GEOID %in% riData$GEOID)
  
  #Plot clusters over map of census tracts
  p <- ggplot(censusTracts) +
    geom_sf() +
    geom_point(data=trips, aes(x = long, y = lat, colour = sc), size = 1) + #Color clusters
    theme_bw() + #Remove gray background
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #Remove grid
    theme(legend.position = "none") + #Remove legend 
    theme(axis.text.x = element_text(angle = 90))
    # ggtitle("Louvain clusters") +
    # theme(plot.title = element_text(face = "bold"))
  #p <- p + geom_point(data = data.frame(sc %>% as.data.frame(), trips[,2], trips[,1]), aes(x = trips[,2], y = trips[,1], alpha = 0.5, color = 1))
  return(p)
}

#Plot different samplings of trip data
#plotWeek <- createPlot(dataWeek)
#plotMonth <- createPlot(dataMonth)
plotYear <- createPlot(dataYear)
