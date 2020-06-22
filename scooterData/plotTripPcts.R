#Import relevant libraries
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(reshape2)

#Import clean trip  data
dir <- "/home/marion/PVDResearch/Data/tripData/cleanData"
filename <- c("tripsOverall")
path <- file.path(dir, paste(filename, ".csv", sep = ""))

assign(filename, read.csv(path))

#Organize trip data by provider
tripsBird <- tripsOverall %>% filter(Provider %in% "Bird")
tripsLime <- tripsOverall %>% filter(Provider %in% "Lime")
tripsSpin <- tripsOverall %>% filter(Provider %in% "Spin")
tripsVeoride <- tripsOverall %>% filter(Provider %in% "Veoride")

#Calculate percent of trips in each region
createStats <- function(data){
  data %>%
    as.tibble() %>%
    group_by(Date) %>%
    summarize(Downtown = mean(Trips.ending.in.Downtown) / mean(Total.trips),
              EastSide = mean(Trips.ending.in.East.Side) / mean(Total.trips), 
              Elmhurt_Charles_etc = mean(Trips.ending.in.Elmhurst.Charles.etc) / mean(Total.trips),
              SouthSide = mean(Trips.ending.in.South.Side) / mean(Total.trips), 
              WestSide_Olneyville_etc = mean(Trips.ending.in.West.Side.Olneyville.etc) / mean(Total.trips)) %>%
    replace(is.na(.), 0) %>%
    melt(id = "Date")
}

pctTripsBird <- createStats(tripsBird)
pctTripsLime <- createStats(tripsLime)
pctTripsSpin <- createStats(tripsSpin)
pctTripsVeoride <- createStats(tripsVeoride)

#Plot percent of trips in each region by provider
createPlot <- function(data, provider){
  ggplot(data, aes(x = Date, y = value, color = variable, group = variable)) +
    geom_path() + 
    ggtitle(provider) + 
    scale_colour_discrete("Regions") +
    ylab("Daily Trips") + 
    scale_y_continuous(labels = scales::percent) +
    xlab("Month") +
    theme(axis.text.x = element_text(angle = 90))
}

plotBird <- createPlot(pctTripsBird, "Bird")
plotLime <- createPlot(pctTripsLime, "Lime")
plotSpin <- createPlot(pctTripsSpin, "Spin")
plotVeoride <- createPlot(pctTripsVeoride, "Veoride")

plotOverall <- ggarrange(plotBird, plotLime, plotSpin, plotVeoride,
                         ncol = 1, nrow = 4, 
                         common.legend = TRUE, legend = "right")
plotOverall <- annotate_figure(plotOverall, 
                               top = text_grob("(as % of Total Trips)"))
plotOverall <- annotate_figure(plotOverall, 
                               top = text_grob("Average Number of Daily Trips by Month by Provider by Region", face = "bold"))


#Save plots
dir <- "/home/marion/PVDResearch/Plots"
filename <- c("avg_daily_trips_by_month_by_provider_by_region")
path <- file.path(dir, paste(filename, ".png", sep = ""))

ggsave(file = path, plot = plotOverall)