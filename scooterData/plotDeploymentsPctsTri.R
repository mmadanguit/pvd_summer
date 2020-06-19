#Import relevant libraries
library(tidyverse)
library(reshape2)

#Import clean deployment data
dir <- "/home/marion/PVDResearch/Data/deploymentData/cleanData"
filenames <- c("avgDeploymentsBird", "avgDeploymentsLime", "avgDeploymentsSpin", "avgDeploymentsVeoride")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

#Import clean trip  data
dir <- "/home/marion/PVDResearch/Data/tripData/cleanData"
filenames <- c("avgTripsProvider")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

#Organize average number of daily trips per month by provider
avgTripsBird <- avgTripsProvider %>% filter(Provider %in% "Bird")
avgTripsLime <- avgTripsProvider %>% filter(Provider %in% "Lime")
avgTripsSpin <- avgTripsProvider %>% filter(Provider %in% "Spin")
avgTripsVeoride <- avgTripsProvider %>% filter(Provider %in% "Veoride")

#Calculate deployments as percentage of daily trips
createStats <- function(deploymentData, tripData){
  data <- deploymentData %>% 
    dcast(Date ~ variable) %>%
    as.tibble() %>%
    mutate(Total = tripData$avg) %>%
    group_by(Date) %>%
    summarize(Downtown = Downtown / Total,
              EastSide = EastSide / Total, 
              Elmhurt_Charles_etc = Elmhurt_Charles_etc / Total,
              SouthSide = SouthSide / Total, 
              WestSide_Olneyville_etc = WestSide_Olneyville_etc / Total)
  is.na(data) <- sapply(data, is.infinite)
  data %>%
    replace(is.na(.), 0) %>%
    melt(id = "Date")
  }

pctDeploymentsBird <- createStats(avgDeploymentsBird, avgTripsBird)
pctDeploymentsLime <- createStats(avgDeploymentsLime, avgTripsLime)
pctDeploymentsSpin <- createStats(avgDeploymentsSpin, avgTripsSpin)
pctDeploymentsVeoride <- createStats(avgDeploymentsVeoride, avgTripsVeoride)

#Plot daily deployments as percentage of daily trips per month by provider and region
createPlot <- function(data, provider){
  ggplot(data, aes(x = Date, y = value, color = variable, group = variable)) +
    geom_path() + 
    ggtitle(paste("Average Number of Daily Deployments for", provider)) + 
    scale_colour_discrete("Regions") +
    ylab("Deployments per Day (% of Total Trips)") + 
    scale_y_continuous(labels = percent) +
    xlab("Month") +
    scale_x_discrete(breaks = c("2018-07", "2018-09", "2018-11", "2019-01", "2019-03", "2019-05", "2019-07", "2019-09", "2019-11", "2020-01", "2020-03")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plotBird <- createPlot(pctDeploymentsBird, "Bird")
plotLime <- createPlot(pctDeploymentsLime, "Lime")
plotSpin <- createPlot(pctDeploymentsSpin, "Spin")
plotVeoride <- createPlot(pctDeploymentsVeoride, "Veoride")

#Save plots
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
filenames <- c("deploymentsBird_pctTri", "deploymentsLime_pctTri", "deploymentsSpin_pctTri", "deploymentsVeoride_pctTri")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}