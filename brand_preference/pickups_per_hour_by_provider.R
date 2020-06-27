library(ggplot2)
library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)
library(plyr)
# library(ggpubr)

# read data
preference_data <- read_csv("preference_data.csv")

# add new column of total scooters picked up within range
preference_data <- preference_data %>%
  mutate(total_pickedup_in_range = num_pickedup_LimeLime + num_pickedup_LimeBird + num_pickedup_BirdLime + num_pickedup_BirdBird)


# separate preference_data into the providers
lime_preference_data <- preference_data %>% filter(grepl("Lime", provider))
bird_preference_data <- preference_data %>% filter(grepl("Bird", provider))

# make a new table of total number of pickups for each day
lime_freq_pickups_each_day <- count(lime_preference_data, "end_time")
bird_freq_pickups_each_day <- count(bird_preference_data, "end_time")

# replace NA (resulting from 0 pickups) with 0
lime_preference_data <- replace(lime_preference_data, is.na(lime_preference_data), 0)
bird_preference_data <- replace(bird_preference_data, is.na(bird_preference_data), 0)

# strip out the time from the end_time and just keep the date
lime_preference_data$end_time <- as.Date(lime_preference_data$end_time)
bird_preference_data$end_time <- as.Date(bird_preference_data$end_time)


# group by end_time and sum the number of pickups and the stagnant time for each group
limelime_preference_data <- lime_preference_data %>% group_by(end_time) %>%
  summarize_at(vars(num_pickedup_LimeLime, stagnant_time), funs(sum))
limebird_preference_data <- lime_preference_data %>% group_by(end_time) %>%
  summarise_at(vars(num_pickedup_LimeBird, stagnant_time), funs(sum))

birdlime_preference_data <- bird_preference_data %>% group_by(end_time) %>%
  summarize_at(vars(num_pickedup_BirdLime, stagnant_time), funs(sum))
birdbird_preference_data <- bird_preference_data %>% group_by(end_time) %>%
  summarize_at(vars(num_pickedup_BirdBird, stagnant_time), funs(sum))

## do the same for preference_data to get the total pickups per hour
# strip out the time from end_time
preference_data$end_time <- as.Date(preference_data$end_time)

# group by end_time for total number of pickups per hour
total_pickedup_per_hour <- preference_data %>% group_by(end_time) %>%
  summarize_at(vars(total_pickedup_in_range, stagnant_time), funs(sum))


# find the number of pickups per hour by different situations
limelime_preference_data <- limelime_preference_data %>% mutate(limelime_per_hour = num_pickedup_LimeLime / stagnant_time)
limebird_preference_data <- limebird_preference_data %>% mutate(limebird_per_hour = num_pickedup_LimeBird / stagnant_time)
birdlime_preference_data <- birdlime_preference_data %>% mutate(birdlime_per_hour = num_pickedup_BirdLime / stagnant_time)
birdbird_preference_data <- birdbird_preference_data %>% mutate(birdbird_per_hour = num_pickedup_BirdBird / stagnant_time)

total_pickedup_per_hour <- total_pickedup_per_hour %>% mutate(pickedup_per_hour = total_pickedup_in_range / stagnant_time)
# the code below was for the same purpose but for some reason doesn't work?
# num_pickedup_per_hour <- mutate(limelime_per_hour = limelime_preference_data$num_pickedup_LimeLime / limelime_preference_data$stagnant_time)
# num_pickedup_per_hour <- mutate(limebird_per_hour = limebird_preference_data$num_pickedup_LimeBird / limebird_preference_data$stagnant_time)
# num_pickedup_per_hour <- mutate(birdlime_per_hour = birdlime_preference_data$num_pickedup_BirdLime / birdlime_preference_data$stagnant_time)
# num_pickedup_per_hour <- mutate(birdbird_per_hour = birdbird_preference_data$num_pickedup_BirdBird / birdbird_preference_data$stagnant_time)


date_range <- seq.Date(from = as.Date('2019-06-13'), to = as.Date('2019-06-22'), by = 'days')

# plot
g_lime <- ggplot(data = limelime_preference_data, aes(x=end_time, y=limelime_per_hour, group=1, colour = "limelime")) +
  geom_line(data = limebird_preference_data, aes(x=end_time, y=limebird_per_hour, group=1, colour = "limebird")) +
  geom_line(data = total_pickedup_per_hour, aes(x=end_time, y=pickedup_per_hour, group=1, colour = "total")) + geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Day", y="Number of Scooters PickedUp / Hour") +
  ggtitle("Brand Preference For Lime") +
  scale_colour_manual("",
                      breaks = c("limelime", "limebird", "total"),
                      values = c("green", "steelblue", "purple"))

g_bird <- ggplot(data = birdlime_preference_data, aes(x=end_time, y=birdlime_per_hour, group=1, colour = "birdlime")) +
  geom_line(data = birdbird_preference_data, aes(x=end_time, y=birdbird_per_hour, group=1, colour = "birdbird")) +
  geom_line(data = total_pickedup_per_hour, aes(x=end_time, y=pickedup_per_hour, group=1, colour = "total")) + geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Day", y="Number of Scooters PickedUp / Hour") +
  ggtitle("Brand Preference For Bird") +
  scale_colour_manual("",
                      breaks = c("birdlime", "birdbird", "total"),
                      values = c("green", "steelblue", "purple"))

# ggarrange(g_lime, g_bird, 
#           labels = c("Lime", "Bird"),
#           ncol = 1, nrow = 2)

print(g_lime)
# print(g_bird)