library(ggplot2)
library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)

data <- read_csv("events_for_multiple_providers_from_19-12-01_to_20-02-29.csv")

# filter unavailable, removed vehicles & rebalance drop-offs
spin_filtered_data <- data %>% filter(!grepl("unavailable", event_type)) %>% 
  filter(!grepl("removed", event_type)) %>% 
  filter(!grepl("rebalance_drop_off", event_type_reason)) %>% 
  filter(!grepl("user_drop_off", event_type_reason)) %>% 
  filter(!grepl("maintenance_drop_off", event_type_reason)) %>%
  filter(!grepl("Veoride", provider)) %>%
  filter(!grepl("Lime", provider)) 

veoride_filtered_data <- data %>% filter(!grepl("unavailable", event_type)) %>% 
  filter(!grepl("removed", event_type)) %>% 
  filter(!grepl("rebalance_drop_off", event_type_reason)) %>% 
  filter(!grepl("user_drop_off", event_type_reason)) %>% 
  filter(!grepl("maintenance_drop_off", event_type_reason)) %>%
  filter(!grepl("Spin", provider)) %>%
  filter(!grepl("Lime", provider)) 

# add new column of 1s later to be used for counting
spin_filtered_data$count_user_pick_up <- rep(1, length.out=nrow(spin_filtered_data))
veoride_filtered_data$count_user_pick_up <- rep(1, length.out=nrow(veoride_filtered_data))

# change time back
spin_filtered_data$event_time <- spin_filtered_data$event_time - 4*60*60
veoride_filtered_data$event_time <- veoride_filtered_data$event_time - 4*60*60


# for some reason should use POSIXct instead of POSIXlt
spin_filtered_data$event_time <- as.POSIXct(spin_filtered_data$event_time)
veoride_filtered_data$event_time <- as.POSIXct(veoride_filtered_data$event_time)

# sort filtered_data by ascending time order
spin_filtered_data <- spin_filtered_data[order(spin_filtered_data$event_time),]
veoride_filtered_data <- veoride_filtered_data[order(veoride_filtered_data$event_time),]

# round time down
spin_filtered_data$event_time <- floor_date(spin_filtered_data$event_time, "1 hour")
veoride_filtered_data$event_time <- floor_date(veoride_filtered_data$event_time, "1 hour")

# add new time column
spin_time <- format(as.POSIXct(strptime(spin_filtered_data$event_time,"%Y-%m-%d %H:%M:%OS",tz="")) ,format = "%H:%M:%OS")
veoride_time <- format(as.POSIXct(strptime(veoride_filtered_data$event_time,"%Y-%m-%d %H:%M:%OS",tz="")) ,format = "%H:%M:%OS")
spin_filtered_data$time <- spin_time
veoride_filtered_data$time <- veoride_time

# group by time
spin_filtered_data <- aggregate(spin_filtered_data$count_user_pick_up, by=list(spin_filtered_data$time), FUN=sum)
veoride_filtered_data <- aggregate(veoride_filtered_data$count_user_pick_up, by=list(veoride_filtered_data$time), FUN=sum)

# update column name
colnames(spin_filtered_data) <- c("Hour", "count_usr_pick_up")
colnames(veoride_filtered_data) <- c("Hour", "count_usr_pick_up")

# create a column containing date range
date_range <- seq.Date(from = as.Date('2019-05-01'), to = as.Date('2019-08-31'), by = 'days')
# get length of that seq to be used for averaging later
num_dates <- length(date_range)

as_tibble(spin_filtered_data)
as_tibble(veoride_filtered_data)
spin_filtered_data$count_usr_pick_up <- spin_filtered_data$count_usr_pick_up / num_dates
veoride_filtered_data$count_usr_pick_up <- veoride_filtered_data$count_usr_pick_up / num_dates

g <- ggplot(data = spin_filtered_data, aes(x=Hour, y=count_usr_pick_up, group=1, colour="spin")) + 
  geom_line(data = veoride_filtered_data, aes(x=Hour, y=count_usr_pick_up, group=1, colour="veoride")) + geom_line() +
  scale_x_discrete(breaks = veoride_filtered_data$Hour) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x="Hours (military time)", y="Average Scooter Usage Count") + 
  ggtitle("Hourly Average Scooter Usage (2019 Dec - 2020 Feb)") +
  scale_colour_manual("", 
                      breaks = c("spin", "veoride"),
                      values = c("red", "purple"))

print(g)
