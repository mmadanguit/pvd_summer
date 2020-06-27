library(ggplot2)
library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)

data <- read_csv("events_for_multiple_providers_from_19-05-02_to_19-08-31.csv")

# filter unavailable, removed vehicles & rebalance drop-offs
lime_filtered_data <- data %>% filter(!grepl("unavailable", event_type)) %>% 
  filter(!grepl("removed", event_type)) %>% 
  filter(!grepl("rebalance_drop_off", event_type_reason)) %>% 
  filter(!grepl("user_drop_off", event_type_reason)) %>% 
  filter(!grepl("maintenance_drop_off", event_type_reason)) %>%
  filter(!grepl("Bird", provider))

bird_filtered_data <- data %>% filter(!grepl("unavailable", event_type)) %>% 
  filter(!grepl("removed", event_type)) %>% 
  filter(!grepl("rebalance_drop_off", event_type_reason)) %>% 
  filter(!grepl("user_drop_off", event_type_reason)) %>% 
  filter(!grepl("maintenance_drop_off", event_type_reason)) %>%
  filter(!grepl("Lime", provider))

# add new column of 1s later to be used for counting
lime_filtered_data$count_user_pick_up <- rep(1, length.out=nrow(lime_filtered_data))
bird_filtered_data$count_user_pick_up <- rep(1, length.out=nrow(bird_filtered_data))

# change time back
lime_filtered_data$event_time <- lime_filtered_data$event_time - 4*60*60
bird_filtered_data$event_time <- bird_filtered_data$event_time - 4*60*60

# for some reason should use POSIXct instead of POSIXlt
lime_filtered_data$event_time <- as.POSIXct(lime_filtered_data$event_time)
bird_filtered_data$event_time <- as.POSIXct(bird_filtered_data$event_time)

# sort filtered_data by ascending time order
lime_filtered_data <- lime_filtered_data[order(lime_filtered_data$event_time),]
bird_filtered_data <- bird_filtered_data[order(bird_filtered_data$event_time),]

# round time down
lime_filtered_data$event_time <- floor_date(lime_filtered_data$event_time, "1 hour")
bird_filtered_data$event_time <- floor_date(bird_filtered_data$event_time, "1 hour")

# add new time column
lime_time <- format(as.POSIXct(strptime(lime_filtered_data$event_time,"%Y-%m-%d %H:%M:%OS",tz="")) ,format = "%H:%M:%OS")
bird_time <- format(as.POSIXct(strptime(bird_filtered_data$event_time,"%Y-%m-%d %H:%M:%OS",tz="")) ,format = "%H:%M:%OS")
lime_filtered_data$time <- lime_time
bird_filtered_data$time <- bird_time

# group by time
lime_filtered_data <- aggregate(lime_filtered_data$count_user_pick_up, by=list(lime_filtered_data$time), FUN=sum)
bird_filtered_data <- aggregate(bird_filtered_data$count_user_pick_up, by=list(bird_filtered_data$time), FUN=sum)

# update column name
colnames(lime_filtered_data) <- c("Hour", "count_usr_pick_up")
colnames(bird_filtered_data) <- c("Hour", "count_usr_pick_up")

# create a column containing date range
date_range <- seq.Date(from = as.Date('2019-05-01'), to = as.Date('2019-08-31'), by = 'days')
# get length of that seq to be used for averaging later
num_dates <- length(date_range)

as_tibble(lime_filtered_data)
as_tibble(bird_filtered_data)
lime_filtered_data$count_usr_pick_up <- lime_filtered_data$count_usr_pick_up / num_dates
bird_filtered_data$count_usr_pick_up <- bird_filtered_data$count_usr_pick_up / num_dates

g <- ggplot(data = lime_filtered_data, aes(x=Hour, y=count_usr_pick_up, group=1, colour="lime")) + 
  geom_line(data = bird_filtered_data, aes(x=Hour, y=count_usr_pick_up, group=1, colour="bird")) + geom_line() +
  scale_x_discrete(breaks = lime_filtered_data$Hour) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x="Hours (military time)", y="Average Scooter Usage Count") + 
  ggtitle("Hourly Average Scooter Usage (2019 May-August)") +
  scale_colour_manual("", 
                      breaks = c("lime", "bird"),
                      values = c("green", "steelblue"))

print(g)
