#Import relevant libraries
library(tidyverse)
library(reshape2)
library(scales)

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
    ggtitle(paste("Average Number of Daily Deployments for", provider)) + 
    scale_colour_discrete("Regions") +
    ylab("Deployments per Day (% of Total Deployments)") + 
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
filenames <- c("deploymentsBird_pctDep", "deploymentsLime_pctDep", "deploymentsSpin_pctDep", "deploymentsVeoride_pctDep")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}