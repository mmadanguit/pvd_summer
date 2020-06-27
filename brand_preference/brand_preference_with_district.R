library(ggplot2)
library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)
library(pracma)

# read data
events_data <- read_csv("events_for_multiple_providers_from_19-06-11_to_19-06-18.csv")
loc_data <- read_csv("locations_for_multiple_providers_from_19-06-11_to_19-06-18.csv")

# get rid of rows that contain NA in areas column
loc_data <- loc_data[rowSums(is.na(loc_data)) == 0,]

# get the first word from area/address info
loc_data$areas <- gsub("([A-Za-z]+).*", "\\1", loc_data$areas)

# filter data
filtered_events_data <- events_data %>% filter(!grepl("unavailable", event_type)) %>% 
  filter(!grepl("removed", event_type)) %>% 
  filter(!grepl("rebalance_drop_off", event_type_reason)) %>% 
  filter(!grepl("maintenance_drop_off", event_type_reason)) %>% 
  filter(!grepl("user_drop_off", event_type_reason)) %>% 
  filter(!grepl("service_start", event_type_reason))

filtered_loc_data <- loc_data %>% filter(!grepl("unavailable", vehicle_status))  %>% 
  filter(!grepl("rebalance_drop_off", vehicle_status_reason)) %>% 
  filter(!grepl("maintenance_drop_off", vehicle_status_reason)) %>%
  filter(end_time-start_time != 0) %>% 
  filter(difftime(end_time, start_time, units = "mins") > 1)

# adjust the time correctly 
filtered_events_data$event_time <- filtered_events_data$event_time - 4*60*60
filtered_loc_data$start_time <- filtered_loc_data$start_time - 4*60*60
filtered_loc_data$end_time <- filtered_loc_data$end_time - 4*60*60

# sort data in ascending order
filtered_events_data <- filtered_events_data[order(filtered_events_data$event_time),]
filtered_loc_data <- filtered_loc_data[order(filtered_loc_data$start_time),]

# set times as POSIXct
filtered_events_data$event_time <- as.POSIXct(filtered_events_data$event_time)
filtered_loc_data$start_time <- as.POSIXct(filtered_loc_data$start_time)
filtered_loc_data$end_time <- as.POSIXct(filtered_loc_data$end_time)

################################################################################

# set global variable
MI_RADIUS <- 3956

# function for converting degree to radian
deg_to_rad <- function(degree) {
  return (degree * pi / 100)
}

# function for finding distance between two points
find_dist <- function(lat1, long1, lat2, long2) {
  dist_lat <- deg_to_rad(lat1 - lat2)
  dist_long <- deg_to_rad(long1 - long2)
  
  value <- '^'(sin(dist_lat/2), 2) + '^'(sin(dist_long/2), 2) * cos(lat1) * cos(lat2)
  c <- 2 * atan2(sqrt(value), sqrt(1-value))
  
  return (MI_RADIUS * c)
}

# create empty columns
brand_pref_1 <- c()
brand_pref_2 <- c()
brand_pref_3 <- c()
brand_pref_4 <- c()

# function for finding the number of scooters pickedup for a stagnant scooter
find_pickedup_status <- function(data, brand, start, end, lat, lng) {
  var1 <- 0
  var2 <- 0
  var3 <- 0
  var4 <- 0
  j <- 1
  while(TRUE) {
    # find distance between the two scooters
    # dist <- find_dist(lat, lng, data$lat[j], data$lng[j])
    dist <- haversine(c(lat, lng), c(data$lat[j], data$lng[j]), MI_RADIUS)
    
    # break if event_time occurred after end (i.e if event_time is greater than end)
    if (difftime(data$event_time[j], end) > seconds(0) || j == nrow(data)) { 
      break
    }
    else if (dist <= 0.25 && difftime(data$event_time[j], start) > seconds(0) && difftime(data$event_time[j], end) < seconds(0)) {
      if (brand == "Lime" && data$provider[j] == "Lime") {
        var1 <- var1 + 1
      } else if (brand == "Lime" && data$provider[j] == "Bird"){
        var2 <- var2 + 1
      } else if (brand == "Bird" && data$provider[j] == "Lime"){
        var3 <- var3 + 1
      } else if (brand == "Bird" && data$provider[j] == "Bird"){
        var4 <- var4 + 1
      }
    }
    j <- j+1
  }
  col <- c(var1, var2, var3, var4)
  return (col)
}
################################################################################

# generate tibble
preference_data <- tibble(end_time = filtered_loc_data$end_time, 
                          stagnant_time = difftime(filtered_loc_data$end_time, filtered_loc_data$start_time, units = "hours"),
                          provider = filtered_loc_data$provider,
                          area = filtered_loc_data$areas)

preference_data$num_pickedup_LimeLime <- rep(0, length.out=nrow(filtered_loc_data))
preference_data$num_pickedup_LimeBird <- rep(0, length.out=nrow(filtered_loc_data))
preference_data$num_pickedup_BirdLime <- rep(0, length.out=nrow(filtered_loc_data))
preference_data$num_pickedup_BirdBird <- rep(0, length.out=nrow(filtered_loc_data))

offset <- 1
find_pickedup_status_vec <- Vectorize(find_pickedup_status, vectorize.args = "data")
for (i in offset:nrow(filtered_loc_data)) {
  # find the number of scooters picked up during the stagnant time
  v <- find_pickedup_status(filtered_events_data, filtered_loc_data$provider[i],
                            filtered_loc_data$start_time[i], filtered_loc_data$end_time[i],
                            filtered_loc_data$lat[i], filtered_loc_data$lng[i])
  
  # add values to columns
  brand_pref_1 <- c(brand_pref_1, v[1])
  brand_pref_2 <- c(brand_pref_2, v[2])
  brand_pref_3 <- c(brand_pref_3, v[3])
  brand_pref_4 <- c(brand_pref_4, v[4])
  print(length(brand_pref_1))
}

preference_data$num_pickedup_LimeLime <- brand_pref_1
preference_data$num_pickedup_LimeBird <- brand_pref_2
preference_data$num_pickedup_BirdLime <- brand_pref_3
preference_data$num_pickedup_BirdBird <- brand_pref_4

# save preference_data as csv
write_csv(preference_data, "preference_data_with_district_corrected.csv")