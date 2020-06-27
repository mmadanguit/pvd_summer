library(ggplot2)
library(tidyverse)
library(dslabs)
library(tibbletime)
library(lubridate)
library(reshape2)
library(data.table)

data <- read_csv("events_for_multiple_providers_from_19-05-02_to_19-08-31.csv")

# filter unavailable, removed vehicles & rebalance drop-offs
filtered_data <- data %>% filter(!grepl("unavailable", event_type)) %>% 
  filter(!grepl("removed", event_type)) %>% 
  filter(!grepl("rebalance_drop_off", event_type_reason)) %>% 
  filter(!grepl("user_drop_off", event_type_reason)) %>% 
  filter(!grepl("maintenance_drop_off", event_type_reason))

# shift time back
filtered_data$event_time <- filtered_data$event_time - 4*60*60

# add new column of 1s later to be used for counting
filtered_data$count_user_pick_up <- rep(1, length.out=nrow(filtered_data))

# for some reason should use POSIXct instead of POSIXlt
filtered_data$event_time <- as.POSIXct(filtered_data$event_time)

# sort filtered_data by ascending time order
filtered_data <- filtered_data[order(filtered_data$event_time),]

# round time down
filtered_data$event_time <- floor_date(filtered_data$event_time, "1 hour")

# add new time column
time <- format(as.POSIXct(strptime(filtered_data$event_time,"%Y-%m-%d %H:%M:%OS",tz="")) ,format = "%H:%M:%OS")
filtered_data$time <- time

# group by time
filtered_data <- aggregate(filtered_data$count_user_pick_up, by=list(filtered_data$time), FUN=sum)

# update column name
colnames(filtered_data) <- c("Hour", "count_usr_pick_up")


# create a column containing date range
date_range <- seq.Date(from = as.Date('2019-05-01'), to = as.Date('2019-08-31'), by = 'days')
# get length of that seq to be used for averaging later
num_dates <- length(date_range)

as_tibble(filtered_data)
filtered_data$count_usr_pick_up <- filtered_data$count_usr_pick_up / num_dates

g <- ggplot(data = filtered_data, aes(x=Hour, y=count_usr_pick_up, group=1)) + geom_line() + 
  scale_x_discrete(breaks = filtered_data$Hour) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x="Hours (military time)", y="Average Scooter Usage Count") + 
  ggtitle("Hourly Average Scooter Usage")
print(g)