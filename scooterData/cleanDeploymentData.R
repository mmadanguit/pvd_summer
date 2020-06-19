#Import relevant libraries
library(tidyverse)

#Import Remix deployment data
dir <- "/home/marion/PVDResearch/Data/deploymentData/originalData"
filenames <- c("deploymentsBird", "deploymentsLime", "deploymentsSpin", "deploymentsVeoride")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

#Clean and organize deployment data
cleanData <- function(data){
  cols <- c("Date", "Deployments.in.Downtown", "Deployments.in.East.Side", "Deployments.in.Elmhurst.Charles.etc", "Deployments.in.South.Side", "Deployments.in.West.Side.Olneyville.etc")
  data <- data %>% 
    as_tibble() %>%
    select(all_of(cols)) %>%
    mutate(Date = as.Date(Date)) %>%
    complete(Date = seq.Date(min(as.Date("2018-07-01")), max(as.Date("2020-03-01")), by = "month")) %>%
    replace(is.na(.), 0) 
  data$Date <- format(data$Date, "%Y-%m")
  data
}

deploymentsBird <- cleanData(deploymentsBird)
deploymentsLime <- cleanData(deploymentsLime)
deploymentsSpin <- cleanData(deploymentsSpin)
deploymentsVeoride <- cleanData(deploymentsVeoride)

#Calculate average number of daily deployments per month by provider and region
createStats <- function(data){
  name <- deparse(substitute(data))
  data %>%
    group_by(Date) %>%
    summarize(Downtown = mean(Deployments.in.Downtown), 
              EastSide = mean(Deployments.in.East.Side),
              Elmhurt_Charles_etc = mean(Deployments.in.Elmhurst.Charles.etc),
              SouthSide = mean(Deployments.in.South.Side),
              WestSide_Olneyville_etc = mean(Deployments.in.West.Side.Olneyville.etc)) %>%
    melt(id = "Date")
}

avgDeploymentsBird <- createStats(deploymentsBird)
avgDeploymentsLime <- createStats(deploymentsLime)
avgDeploymentsSpin <- createStats(deploymentsSpin)
avgDeploymentsVeoride <- createStats(deploymentsVeoride)

#Save cleaned deployment data
datasets <- list(deploymentsBird, deploymentsLime, deploymentsSpin, deploymentsVeoride, avgDeploymentsBird, avgDeploymentsLime, avgDeploymentsSpin, avgDeploymentsVeoride)
dir <- "/home/marion/PVDResearch/Data/deploymentData/cleanData"
filenames <- c("deploymentsBird", "deploymentsLime", "deploymentsSpin", "deploymentsVeoride", "avgDeploymentsBird", "avgDeploymentsLime", "avgDeploymentsSpin", "avgDeploymentsVeoride")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(datasets)){
  write.csv(datasets[i], paths[i])
}
