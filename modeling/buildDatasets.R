# Install relevant libraries ---------------------------------------------------
library(lubridate)
library(tidyverse)

# Import data ------------------------------------------------------------------
dir <- "/home/marion/PVDResearch/Data/mobilityData/cleanData"
filename <- "tripsYear1WithTracts"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

dir <- "/home/marion/PVDResearch/Data/weatherData"
filename <- "clean_weather_dataset"
path <- file.path(dir, paste(filename, ".csv", sep = ""))
assign(filename, read.csv(path))

# Clean and filter trip data ---------------------------------------------------
tripData <- tripsYear1WithTracts %>%
  # Consider only trips that last at least 3 minutes
  filter(minutes >= 3) %>% 
  # Round time down to the nearest hour
  mutate(start_time = floor_date(as.POSIXct(start_time), "hour")) %>%
  # Separate start time into date and time
  mutate(date = as.Date(start_time),
         time = format(start_time, format = "%H:%M:%S")) %>%
  # Determine whether date is a weekday or a weekend
  mutate(day = weekdays(date),
         weekday = as.factor(ifelse(day == "Saturday" | day == "Sunday", 0, 1))) %>%
  # Determine what season date is in
  mutate(season = factor(quarters(date+31), labels = c(1, 2, 3, 4))) %>%
  # Round coordinates to the nearest 5/1000 place
  mutate(start_lat = 0.005*round(start_latitude/0.005, digits = 0),
         start_long = 0.005*round(start_longitude/0.005, digits = 0),
         end_lat = 0.005*round(end_latitude/0.005, digits = 0),
         end_long = 0.005*round(end_longitude/0.005, digits = 0)) %>%
  select(date, time, weekday, season, start_lat, start_long, end_lat, end_long)

# Clean and filter weather data ------------------------------------------------
weatherData <- clean_weather_dataset %>%
  # Convert date to Date object
  mutate(date = as.Date(DATE)) %>%
  # Select average wind speed, precipitation, snow, and average temperature
  select(date, AWND, PRCP, SNOW, TAVG)

# Merge data sets --------------------------------------------------------------
features <- merge(tripData, weatherData, by = "date")

# Prepare data for entire check-out number prediction --------------------------
data <- features %>%  
  # Count number of trips per day per hour
  group_by(date, time) %>%
  summarise(weekday = weekday,
            season = season,
            AWND = mean(AWND),
            PRCP = mean(PRCP),
            TAVG = mean(TAVG),
            count = n()) 

# Create a last_count column that stores the count from the previous time slot
data$last_count <- c(0, data$count[-length(data$count)]) 
# Make last_count zero at the start of every day
indStart <- match(unique(data$date), data$date)
data$last_count[indStart] <- 0

# Split data into train and test data sets -------------------------------------
# Randomly select 75% of the dates to put into the train data set
dates <- unique(data$date)
ind <- sample(length(dates), round(0.75*length(dates)))
trainDates <- dates[ind]
# Put the remaining dates into the test data set
testDates <- dates[-ind]

trainData <- filter(data, date %in% trainDates)
testData <- filter(data, date %in% testDates)

# Save data sets ---------------------------------------------------------------
datasets <- list(features, trainData, testData)
dir <- "/home/marion/PVDResearch/Data/modelingData"
filenames <- c("features", "trainData", "testData")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for(i in 1:length(datasets)){
  write.csv(datasets[i], paths[i])
}