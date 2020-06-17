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

#Plot average number of daily deployments per month by provider and region
createPlot <- function(data, provider){
  ggplot(data, aes(x = Date, y = value, color = variable, group = variable)) +
    geom_path() + 
    ggtitle(paste("Average Number of Daily Deployments for", provider)) + 
    scale_colour_discrete("Regions") +
    ylab("Deployments per Day") + 
    xlab("Month") +
    scale_x_discrete(breaks = c("2018-07", "2018-09", "2018-11", "2019-01", "2019-03", "2019-05", "2019-07", "2019-09", "2019-11", "2020-01", "2020-03")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plotBird <- createPlot(avgDeploymentsBird, "Bird")
plotLime <- createPlot(avgDeploymentsLime, "Lime")
plotSpin <- createPlot(avgDeploymentsSpin, "Spin")
plotVeoride <- createPlot(avgDeploymentsVeoride, "Veoride")

#Save plots
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
filenames <- c("deploymentsBird", "deploymentsLime", "deploymentsSpin", "deploymentsVeoride")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}