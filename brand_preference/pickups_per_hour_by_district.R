library(ggplot2)
library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)
library(plyr)

# read data
preference_data <- read_csv("preference_data_with_district.csv")

# add new column of total scooters picked up within range
preference_data <- preference_data %>%
  mutate(total_pickedup_in_range = num_pickedup_LimeLime + num_pickedup_LimeBird + num_pickedup_BirdLime + num_pickedup_BirdBird)

# separate preference_data into the zones & providers
downtown_lime_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                                 filter(grepl("Downtown", area))
downtown_bird_preference_data <- preference_data %>% filter(grepl("Bird", provider)) %>%
                                 filter(grepl("Downtown", area))
east_lime_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                             filter(grepl("East", area))
east_bird_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                             filter(grepl("East", area))
south_lime_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                              filter(grepl("South", area))
south_bird_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                              filter(grepl("South", area))
west_lime_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                             filter(grepl("west", area))
west_bird_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                             filter(grepl("west", area))
elm_lime_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                            filter(grepl("Elmhurst", area))
elm_bird_preference_data <- preference_data %>% filter(grepl("Lime", provider)) %>%
                            filter(grepl("Elmhurst", area))


# make a new table of total number of pickups for each day
downtown_lime_freq_pickups_each_day <- count(downtown_lime_preference_data, "end_time")
downtown_bird_freq_pickups_each_day <- count(downtown_bird_preference_data, "end_time")
east_lime_freq_pickups_each_day <- count(east_lime_preference_data, "end_time")
east_bird_freq_pickups_each_day <- count(east_bird_preference_data, "end_time")
south_lime_freq_pickups_each_day <- count(south_lime_preference_data, "end_time")
south_bird_freq_pickups_each_day <- count(south_bird_preference_data, "end_time")
west_lime_freq_pickups_each_day <- count(west_lime_preference_data, "end_time")
west_bird_freq_pickups_each_day <- count(west_bird_preference_data, "end_time")
elm_lime_freq_pickups_each_day <- count(elm_lime_preference_data, "end_time")
elm_bird_freq_pickups_each_day <- count(elm_bird_preference_data, "end_time")

# replace NA (resulting from 0 pickups) with 0
downtown_lime_preference_data <- replace(downtown_lime_preference_data, is.na(downtown_lime_preference_data), 0)
downtown_bird_preference_data <- replace(downtown_bird_preference_data, is.na(downtown_bird_preference_data), 0)
east_lime_preference_data <- replace(east_lime_preference_data, is.na(east_lime_preference_data), 0)
east_bird_preference_data <- replace(east_bird_preference_data, is.na(east_bird_preference_data), 0)
south_lime_preference_data <- replace(south_lime_preference_data, is.na(south_lime_preference_data), 0)
south_bird_preference_data <- replace(south_bird_preference_data, is.na(south_bird_preference_data), 0)
west_lime_preference_data <- replace(west_lime_preference_data, is.na(west_lime_preference_data), 0)
west_bird_preference_data <- replace(west_bird_preference_data, is.na(west_bird_preference_data), 0)
elm_lime_preference_data <- replace(elm_lime_preference_data, is.na(elm_lime_preference_data), 0)
elm_bird_preference_data <- replace(elm_bird_preference_data, is.na(elm_bird_preference_data), 0)

# strip out the time from the end_time and just keep the date
downtown_lime_preference_data$end_time <- as.Date(downtown_lime_preference_data$end_time)
downtown_bird_preference_data$end_time <- as.Date(downtown_bird_preference_data$end_time)
east_lime_preference_data$end_time <- as.Date(east_lime_preference_data$end_time)
east_bird_preference_data$end_time <- as.Date(east_bird_preference_data$end_time)
south_lime_preference_data$end_time <- as.Date(south_lime_preference_data$end_time)
south_bird_preference_data$end_time <- as.Date(south_bird_preference_data$end_time)
west_lime_preference_data$end_time <- as.Date(west_lime_preference_data$end_time)
west_bird_preference_data$end_time <- as.Date(west_bird_preference_data$end_time)
elm_lime_preference_data$end_time <- as.Date(elm_lime_preference_data$end_time)
elm_bird_preference_data$end_time <- as.Date(elm_bird_preference_data$end_time)

# group by end_time and sum the number of pickups and the stagnant time for each group
downtown_limelime_preference_data <- downtown_lime_preference_data %>% group_by(end_time) %>%
                                     summarize_at(vars(num_pickedup_LimeLime, stagnant_time), funs(sum))
downtown_limebird_preference_data <- downtown_lime_preference_data %>% group_by(end_time) %>%
                                     summarise_at(vars(num_pickedup_LimeBird, stagnant_time), funs(sum))
downtown_birdlime_preference_data <- downtown_bird_preference_data %>% group_by(end_time) %>%
                                     summarize_at(vars(num_pickedup_BirdLime, stagnant_time), funs(sum))
downtown_birdbird_preference_data <- downtown_bird_preference_data %>% group_by(end_time) %>%
                                     summarize_at(vars(num_pickedup_BirdBird, stagnant_time), funs(sum))
east_limelime_preference_data <- east_lime_preference_data %>% group_by(end_time) %>%
                                     summarize_at(vars(num_pickedup_LimeLime, stagnant_time), funs(sum))
east_limebird_preference_data <- east_lime_preference_data %>% group_by(end_time) %>%
                                 summarise_at(vars(num_pickedup_LimeBird, stagnant_time), funs(sum))
east_birdlime_preference_data <- east_bird_preference_data %>% group_by(end_time) %>%
                                 summarize_at(vars(num_pickedup_BirdLime, stagnant_time), funs(sum))
east_birdbird_preference_data <- east_bird_preference_data %>% group_by(end_time) %>%
                                  summarize_at(vars(num_pickedup_BirdBird, stagnant_time), funs(sum))
south_limelime_preference_data <- south_lime_preference_data %>% group_by(end_time) %>%
                                  summarize_at(vars(num_pickedup_LimeLime, stagnant_time), funs(sum))
south_limebird_preference_data <- south_lime_preference_data %>% group_by(end_time) %>%
                                  summarise_at(vars(num_pickedup_LimeBird, stagnant_time), funs(sum))
south_birdlime_preference_data <- south_bird_preference_data %>% group_by(end_time) %>%
                                  summarize_at(vars(num_pickedup_BirdLime, stagnant_time), funs(sum))
south_birdbird_preference_data <- south_bird_preference_data %>% group_by(end_time) %>%
                                  summarize_at(vars(num_pickedup_BirdBird, stagnant_time), funs(sum))
west_limelime_preference_data <- west_lime_preference_data %>% group_by(end_time) %>%
                                 summarize_at(vars(num_pickedup_LimeLime, stagnant_time), funs(sum))
west_limebird_preference_data <- west_lime_preference_data %>% group_by(end_time) %>%
                                 summarise_at(vars(num_pickedup_LimeBird, stagnant_time), funs(sum))
west_birdlime_preference_data <- west_bird_preference_data %>% group_by(end_time) %>%
                                 summarize_at(vars(num_pickedup_BirdLime, stagnant_time), funs(sum))
west_birdbird_preference_data <- west_bird_preference_data %>% group_by(end_time) %>%
                                 summarize_at(vars(num_pickedup_BirdBird, stagnant_time), funs(sum))
elm_limelime_preference_data <- elm_lime_preference_data %>% group_by(end_time) %>%
                                summarize_at(vars(num_pickedup_LimeLime, stagnant_time), funs(sum))
elm_limebird_preference_data <- elm_lime_preference_data %>% group_by(end_time) %>%
                                summarise_at(vars(num_pickedup_LimeBird, stagnant_time), funs(sum))
elm_birdlime_preference_data <- elm_bird_preference_data %>% group_by(end_time) %>%
                                summarize_at(vars(num_pickedup_BirdLime, stagnant_time), funs(sum))
elm_birdbird_preference_data <- elm_bird_preference_data %>% group_by(end_time) %>%
                                summarize_at(vars(num_pickedup_BirdBird, stagnant_time), funs(sum))

## do the same for preference_data to get the total pickups per hour
# strip out the time from end_time
preference_data$end_time <- as.Date(preference_data$end_time)

# group by end_time for total number of pickups per hour
total_pickedup_per_hour <- preference_data %>% group_by(end_time) %>%
  summarize_at(vars(total_pickedup_in_range, stagnant_time), funs(sum))

# find the number of pickups per hour by different situations
downtown_limelime_preference_data <- downtown_limelime_preference_data %>% mutate(limelime_per_hour = num_pickedup_LimeLime / stagnant_time)
downtown_limebird_preference_data <- downtown_limebird_preference_data %>% mutate(limebird_per_hour = num_pickedup_LimeBird / stagnant_time)
downtown_birdlime_preference_data <- downtown_birdlime_preference_data %>% mutate(birdlime_per_hour = num_pickedup_BirdLime / stagnant_time)
downtown_birdbird_preference_data <- downtown_birdbird_preference_data %>% mutate(birdbird_per_hour = num_pickedup_BirdBird / stagnant_time)

south_limelime_preference_data <- south_limelime_preference_data %>% mutate(limelime_per_hour = num_pickedup_LimeLime / stagnant_time)
south_limebird_preference_data <- south_limebird_preference_data %>% mutate(limebird_per_hour = num_pickedup_LimeBird / stagnant_time)
south_birdlime_preference_data <- south_birdlime_preference_data %>% mutate(birdlime_per_hour = num_pickedup_BirdLime / stagnant_time)
south_birdbird_preference_data <- south_birdbird_preference_data %>% mutate(birdbird_per_hour = num_pickedup_BirdBird / stagnant_time)

west_limelime_preference_data <- west_limelime_preference_data %>% mutate(limelime_per_hour = num_pickedup_LimeLime / stagnant_time)
west_limebird_preference_data <- west_limebird_preference_data %>% mutate(limebird_per_hour = num_pickedup_LimeBird / stagnant_time)
west_birdlime_preference_data <- west_birdlime_preference_data %>% mutate(birdlime_per_hour = num_pickedup_BirdLime / stagnant_time)
west_birdbird_preference_data <- west_birdbird_preference_data %>% mutate(birdbird_per_hour = num_pickedup_BirdBird / stagnant_time)

east_limelime_preference_data <- east_limelime_preference_data %>% mutate(limelime_per_hour = num_pickedup_LimeLime / stagnant_time)
east_limebird_preference_data <- east_limebird_preference_data %>% mutate(limebird_per_hour = num_pickedup_LimeBird / stagnant_time)
east_birdlime_preference_data <- east_birdlime_preference_data %>% mutate(birdlime_per_hour = num_pickedup_BirdLime / stagnant_time)
east_birdbird_preference_data <- east_birdbird_preference_data %>% mutate(birdbird_per_hour = num_pickedup_BirdBird / stagnant_time)

elm_limelime_preference_data <- elm_limelime_preference_data %>% mutate(limelime_per_hour = num_pickedup_LimeLime / stagnant_time)
elm_limebird_preference_data <- elm_limebird_preference_data %>% mutate(limebird_per_hour = num_pickedup_LimeBird / stagnant_time)
elm_birdlime_preference_data <- elm_birdlime_preference_data %>% mutate(birdlime_per_hour = num_pickedup_BirdLime / stagnant_time)
elm_birdbird_preference_data <- elm_birdbird_preference_data %>% mutate(birdbird_per_hour = num_pickedup_BirdBird / stagnant_time)

total_pickedup_per_hour <- total_pickedup_per_hour %>% mutate(pickedup_per_hour = total_pickedup_in_range / stagnant_time)

g_downtown <- ggplot(data = downtown_limelime_preference_data, aes(x=end_time, y=limelime_per_hour, group=1, colour = "limelime")) +
  geom_line(data = downtown_limebird_preference_data, aes(x=end_time, y=limebird_per_hour, group=1, colour = "limebird")) +
  geom_line(data = downtown_birdlime_preference_data, aes(x=end_time, y=birdlime_per_hour, group=1, colour = "birdlime")) +
  geom_line(data = downtown_birdbird_preference_data, aes(x=end_time, y=birdbird_per_hour, group=1, colour = "birdbird")) +
  geom_line(data = total_pickedup_per_hour, aes(x=end_time, y=pickedup_per_hour, group=1, colour = "total")) + geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Day", y="Number of Scooters PickedUp / Hour") +
  ggtitle("Downtown Brand Preference")

g_south <- ggplot(data = south_limelime_preference_data, aes(x=end_time, y=limelime_per_hour, group=1, colour = "limelime")) +
  geom_line(data = south_limebird_preference_data, aes(x=end_time, y=limebird_per_hour, group=1, colour = "limebird")) +
  geom_line(data = south_birdlime_preference_data, aes(x=end_time, y=birdlime_per_hour, group=1, colour = "birdlime")) +
  geom_line(data = south_birdbird_preference_data, aes(x=end_time, y=birdbird_per_hour, group=1, colour = "birdbird")) +
  geom_line(data = total_pickedup_per_hour, aes(x=end_time, y=pickedup_per_hour, group=1, colour = "total")) + geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Day", y="Number of Scooters PickedUp / Hour") +
  ggtitle("South Brand Preference")

g_west <- ggplot(data = west_limelime_preference_data, aes(x=end_time, y=limelime_per_hour, group=1, colour = "limelime")) +
  geom_line(data = west_limebird_preference_data, aes(x=end_time, y=limebird_per_hour, group=1, colour = "limebird")) +
  geom_line(data = west_birdlime_preference_data, aes(x=end_time, y=birdlime_per_hour, group=1, colour = "birdlime")) +
  geom_line(data = west_birdbird_preference_data, aes(x=end_time, y=birdbird_per_hour, group=1, colour = "birdbird")) +
  geom_line(data = total_pickedup_per_hour, aes(x=end_time, y=pickedup_per_hour, group=1, colour = "total")) + geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Day", y="Number of Scooters PickedUp / Hour") +
  ggtitle("West Brand Preference")

g_east <- ggplot(data = east_limelime_preference_data, aes(x=end_time, y=limelime_per_hour, group=1, colour = "limelime")) +
  geom_line(data = east_limebird_preference_data, aes(x=end_time, y=limebird_per_hour, group=1, colour = "limebird")) +
  geom_line(data = east_birdlime_preference_data, aes(x=end_time, y=birdlime_per_hour, group=1, colour = "birdlime")) +
  geom_line(data = east_birdbird_preference_data, aes(x=end_time, y=birdbird_per_hour, group=1, colour = "birdbird")) +
  geom_line(data = total_pickedup_per_hour, aes(x=end_time, y=pickedup_per_hour, group=1, colour = "total")) + geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Day", y="Number of Scooters PickedUp / Hour") +
  ggtitle("East Brand Preference")

g_elm <- ggplot(data = elm_limelime_preference_data, aes(x=end_time, y=limelime_per_hour, group=1, colour = "limelime")) +
  geom_line(data = elm_limebird_preference_data, aes(x=end_time, y=limebird_per_hour, group=1, colour = "limebird")) +
  geom_line(data = elm_birdlime_preference_data, aes(x=end_time, y=birdlime_per_hour, group=1, colour = "birdlime")) +
  geom_line(data = elm_birdbird_preference_data, aes(x=end_time, y=birdbird_per_hour, group=1, colour = "birdbird")) +
  geom_line(data = total_pickedup_per_hour, aes(x=end_time, y=pickedup_per_hour, group=1, colour = "total")) + geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Day", y="Number of Scooters PickedUp / Hour") +
  ggtitle("Elmhurst Brand Preference")


# print(g_downtown)
# print(g_south)
# print(g_west)
# print(g_east)
print(g_elm)