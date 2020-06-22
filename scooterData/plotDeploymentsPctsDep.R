#Import relevant libraries
library(ggplot2)
library(ggpubr)
library(reshape2)
library(scales)
library(tidyverse)

#Import clean deployment data
dir <- "/home/marion/PVDResearch/Data/deploymentData/cleanData"
filenames <- c("avgDeploymentsBird", "avgDeploymentsLime", "avgDeploymentsSpin", "avgDeploymentsVeoride")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

#Calculate deployments as percentage of total deployments
createStats <- function(deploymentData){
  deploymentData %>% 
    dcast(Date ~ variable) %>%
    as.tibble() %>%
    mutate(Total = Downtown + EastSide + Elmhurt_Charles_etc + SouthSide + WestSide_Olneyville_etc) %>%
    group_by(Date) %>%
    summarize(Downtown = Downtown / Total,
              EastSide = EastSide / Total, 
              Elmhurt_Charles_etc = Elmhurt_Charles_etc / Total,
              SouthSide = SouthSide / Total, 
              WestSide_Olneyville_etc = WestSide_Olneyville_etc / Total) %>%
    replace(is.na(.), 0) %>%
    melt(id = "Date")
}

pctDeploymentsBird <- createStats(avgDeploymentsBird)
pctDeploymentsLime <- createStats(avgDeploymentsLime)
pctDeploymentsSpin <- createStats(avgDeploymentsSpin)
pctDeploymentsVeoride <- createStats(avgDeploymentsVeoride)

#Plot daily deployments as percentage of total deployments per month by provider and region
createPlot <- function(data, provider){
  ggplot(data, aes(x = Date, y = value, color = variable, group = variable)) +
    geom_path() + 
    ggtitle(provider) + 
    scale_colour_discrete("Regions") +
    ylab("Daily Deps") + 
    scale_y_continuous(labels = percent) +
    xlab("Month") +
    theme(axis.text.x = element_text(angle = 90))
}

plotBird <- createPlot(pctDeploymentsBird, "Bird")
plotLime <- createPlot(pctDeploymentsLime, "Lime")
plotSpin <- createPlot(pctDeploymentsSpin, "Spin")
plotVeoride <- createPlot(pctDeploymentsVeoride, "Veoride")

plotOverall <- ggarrange(plotBird, plotLime, plotSpin, plotVeoride,
                         ncol = 1, nrow = 4, 
                         common.legend = TRUE, legend = "right")
plotOverall <- annotate_figure(plotOverall, 
                               top = text_grob("(as % of Provider's Total Deployments)"))
plotOverall <- annotate_figure(plotOverall, 
                               top = text_grob("Average Number of Daily Deployments by Month by Provider by Region", face = "bold"))

#Save plots
dir <- "/home/marion/PVDResearch/Plots"
filename <- c("avg_daily_deployments_by_month_by_provider_by_region_as_pct_total_deployments")
path <- file.path(dir, paste(filename, ".png", sep = ""))

ggsave(file = path, plot = plotOverall)