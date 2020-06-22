library(ggplot2)
library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)

# read data
preference_data <- read_csv("preference_data.csv")

# separate preference_data into the providers
lime_preference_data <- preference_data %>% filter(grepl("Lime", provider))
bird_preference_data <- preference_data %>% filter(grepl("Bird", provider))


# replace NA (resulting from 0 pickups) with 0
lime_preference_data <- replace(lime_preference_data, is.na(lime_preference_data), 0)
bird_preference_data <- replace(bird_preference_data, is.na(bird_preference_data), 0)


# strip out the time from the end_time and just keep the date
lime_preference_data$end_time <- as.Date(lime_preference_data$end_time)
bird_preference_data$end_time <- as.Date(bird_preference_data$end_time)


# group by end_time and count the number of pickups for each time group
lime_preference_data <- lime_preference_data %>% group_by(end_time) %>%
                        summarize_at(vars(num_pickedup_LimeLime, num_pickedup_LimeBird), funs(sum))
bird_preference_data <- bird_preference_data %>% group_by(end_time) %>%
                        summarize_at(vars(num_pickedup_BirdLime, num_pickedup_BirdBird), funs(sum))


# calculate the percent of each brand's pickup for each scooter idle time
lime_preference_data <- lime_preference_data %>% 
                        mutate(limelime_percent = num_pickedup_LimeLime / (num_pickedup_LimeLime + num_pickedup_LimeBird)) %>%
                        mutate(limebird_percent = num_pickedup_LimeBird / (num_pickedup_LimeLime + num_pickedup_LimeBird))
bird_preference_data <- bird_preference_data %>%
                        mutate(birdlime_percent = num_pickedup_BirdLime / (num_pickedup_BirdLime + num_pickedup_BirdBird)) %>%
                        mutate(birdbird_percent = num_pickedup_BirdBird / (num_pickedup_BirdLime + num_pickedup_BirdBird))


# plot
# g <- ggplot(data = lime_preference_data, aes(x=end_time, y=limelime_percent, group=1, colour = "limelime")) + 
#   geom_line(data = lime_preference_data, aes(x=end_time, y=limebird_percent, group=1, colour = "limebird")) + geom_line() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
#   labs(x="Day", y="Percent of Scooters Chosen (%)") + 
#   ggtitle("Brand Preference For Lime") + 
#   scale_colour_manual("", 
#                       breaks = c("limelime", "limebird"),
#                       values = c("green", "steelblue"))

g <- ggplot(data = bird_preference_data, aes(x=end_time, y=birdlime_percent, group=1, colour = "birdlime")) +
  geom_line(data = bird_preference_data, aes(x=end_time, y=birdbird_percent, group=1, colour = "birdbird")) + geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Day", y="Percent of Scooters Chosen (%)") +
  ggtitle("Brand Preference For Bird") +
  scale_colour_manual("",
                      breaks = c("birdlime", "birdbird"),
                      values = c("green", "steelblue"))

print(g)
