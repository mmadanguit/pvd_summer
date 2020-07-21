library(tidyverse)
library(dplyr)
library(pracma)
library(ggplot2)
source('college_loc.R')

get_unique_loc <- function(data) {
  "
  Only get the unique combinations of LAT LNG
  "
  data <- data %>%
    select(-c(START, END, COUNT, AVAIL, DATE, DAY)) %>%
    distinct(LAT, LNG)

  return(data)
}

near_college_check <- function(data) {
  MI_RADIUS <- 3956
  college_loc <- get_colleges()
  loc <- get_unique_loc(data)

  # check whether their distance is within the college_locs
  for (i in 1:dim(loc)[1]) {
    for (j in 1:dim(college_loc)[1]) {
      dist <- haversine(c(loc$LAT[i], loc$LNG[i]), c(college_loc$LAT[j], college_loc$LNG[j]), MI_RADIUS)
      if (dist <= 0.15) {
        data[(data$LAT == loc$LAT[i] | data$LNG == loc$LNG[i]),]$COLLEGE <- TRUE
      }
    }
  }
  
  return(data)
}


pickups <- read_csv("~/PVD Summer Research/average_num_available/intervalCountsLATLNG.csv")
pickups <- pickups %>%
           filter(DATE >= "2019-4-15" & DATE <= "2019-6-15") %>%
           mutate(COLLEGE = FALSE)


# group only by date to take the average pickups per day
avg_pickups <- pickups %>%
               group_by(DAY) %>%
               summarise(AVG_COUNT = mean(COUNT))
print(head(avg_pickups))

col_pickups <- near_college_check(pickups) %>%
               group_by(DAY, COLLEGE) %>%
               summarise(AVG_COUNT = mean(COUNT))

# Graph
g <- ggplot(data = avg_pickups, aes(x = DAY, y = AVG_COUNT, group = 1, color = "avg")) +
     geom_line(data = col_pickups, aes(x = DAY, y = AVG_COUNT, group = COLLEGE, color = COLLEGE)) +
     geom_line() +
     labs(x="DAY", y="AVG_COUNT") +
     ggtitle("Weekly College Usage - 0.15 mi")
show(g)