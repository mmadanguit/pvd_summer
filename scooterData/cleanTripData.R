#Import relevant libraries
library(tidyverse)

#Import Remix deployment data
dir <- "/home/marion/PVDResearch/Data/tripData/originalData"
filenames <- c("tripsBird", "tripsLime", "tripsSpin", "tripsVeoride")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(filenames)){
  assign(filenames[i], read.csv(paths[i]))
}

#Clean and organize trip data
cleanData <- function(data, provider){
  cols <- c("Total.trips", "Trips.ending.in.Downtown", "Trips.ending.in.East.Side", "Trips.ending.in.Elmhurst.Charles.etc", "Trips.ending.in.South.Side", "Trips.ending.in.West.Side.Olneyville.etc")
  data <- data %>% 
    as_tibble() %>%
    select(Date, all_of(cols)) %>%
    mutate_all(funs(str_replace(., "< 5", "0")))
  data[cols] <- sapply(data[cols], as.numeric)
  data <- data %>%
    mutate(Date = as.Date(Date)) %>%
    complete(Date = seq.Date(min(as.Date("2018-07-01")), max(as.Date("2020-03-01")), by = "month")) %>%
    replace(is.na(.), 0) 
  data$Date <- format(data$Date, "%Y-%m")
  data %>% add_column(Provider = provider, .after = "Date")
}

tripsBird <- cleanData(tripsBird, "Bird")
tripsLime <- cleanData(tripsLime, "Lime")
tripsSpin <- cleanData(tripsSpin, "Spin")
tripsVeoride <- cleanData(tripsVeoride, "Veoride")
tripsOverall <- rbind(tripsBird, tripsLime, tripsSpin, tripsVeoride)

#Calculate average number of daily trips per month by provider
avgTripsProvider <- tripsOverall %>%
  group_by(Date, Provider) %>%
  summarize(avg = mean(Total.trips)) 

#Calculate average number of daily trips per month by region
avgTripsRegion <- tripsOverall %>%
  group_by(Date) %>%
  summarize(Downtown = mean(Trips.ending.in.Downtown), 
            EastSide = mean(Trips.ending.in.East.Side),
            Elmhurt_Charles_etc = mean(Trips.ending.in.Elmhurst.Charles.etc),
            SouthSide = mean(Trips.ending.in.South.Side),
            WestSide_Olneyville_etc = mean(Trips.ending.in.West.Side.Olneyville.etc)) %>%
  melt(id = "Date")

#Save cleaned trip data
datasets <- list(tripsBird, tripsLime, tripsSpin, tripsVeoride, tripsOverall, avgTripsProvider, avgTripsRegion)
dir <- "/home/marion/PVDResearch/Data/tripData/cleanData"
filenames <- c("tripsBird", "tripsLime", "tripsSpin", "tripsVeoride", "tripsOverall", "avgTripsProvider", "avgTripsRegion")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(datasets)){
  write.csv(datasets[i], paths[i])
}
