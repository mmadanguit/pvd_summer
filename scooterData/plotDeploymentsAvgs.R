#Import relevant libraries
library(ggplot2)
library(ggpubr)
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
    ggtitle(provider) + 
    scale_colour_discrete("Regions") +
    ylab("Daily Deps") + 
    xlab("Month") +
    theme(axis.text.x = element_text(angle = 90))
}

plotBird <- createPlot(avgDeploymentsBird, "Bird")
plotLime <- createPlot(avgDeploymentsLime, "Lime")
plotSpin <- createPlot(avgDeploymentsSpin, "Spin")
plotVeoride <- createPlot(avgDeploymentsVeoride, "Veoride")

plotOverall <- ggarrange(plotBird, plotLime, plotSpin, plotVeoride,
                         ncol = 1, nrow = 4, 
                         common.legend = TRUE, legend = "right")
plotOverall <- annotate_figure(plotOverall, 
                               top = text_grob("Average Number of Daily Deployments by Month by Provider by Region", face = "bold"))


#Save plots
dir <- "/home/marion/PVDResearch/Plots"
filename <- c("avg_daily_deployments_by_month_by_provider_by_region")
path <- file.path(dir, paste(filename, ".png", sep = ""))

ggsave(file = path, plot = plotOverall)