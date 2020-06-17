#Import relevant libraries
library(tidyverse)
library(reshape2)

#Import clean trip  data
dir <- "/home/marion/PVDResearch/Data/tripData/cleanData"
filenames <- c("tripsOverall")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

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
    ggtitle(paste("Average Number of Daily Trips for", provider)) + 
    scale_colour_discrete("Regions") +
    ylab("Trips per Day (% of Total Trips)") + 
    scale_y_continuous(labels = percent) +
    xlab("Month") +
    scale_x_discrete(breaks = c("2018-07", "2018-09", "2018-11", "2019-01", "2019-03", "2019-05", "2019-07", "2019-09", "2019-11", "2020-01", "2020-03")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plotBird <- createPlot(pctTripsBird, "Bird")
plotLime <- createPlot(pctTripsLime, "Lime")
plotSpin <- createPlot(pctTripsSpin, "Spin")
plotVeoride <- createPlot(pctTripsVeoride, "Veoride")

#Save plots
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
filenames <- c("tripsBird_pct", "tripsLime_pct", "tripsSpin_pct", "tripsVeoride_pct")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}