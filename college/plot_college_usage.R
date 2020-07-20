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
    flag <- FALSE
    for (j in 1:dim(college_loc)[1]) {
      dist <- haversine(c(loc$LAT[i], loc$LNG[i]), c(college_loc$LAT[j], college_loc$LNG[j]), MI_RADIUS)
      if (dist <= 0.20) {
        flag <- TRUE
      }
    }
    if (flag == FALSE) {
      # remove the ones that do not fall under the distances
      data <- subset(data, LAT != loc$LAT[i] & LNG != loc$LNG[i])
    }
  }
  
  return(data)
}


pickups <- read_csv("~/PVD Summer Research/average_num_available/intervalCountsLATLNG.csv")
pickups <- pickups %>%
           filter(DATE >= "2019-4-15" & DATE <= "2019-6-15")

loc <- get_unique_loc(pickups)

# group only by date to take the average pickups per day
avg_pickups <- pickups %>%
               group_by(DATE) %>%
               summarise(AVG_COUNT = mean(COUNT))
print(head(avg_pickups))

# get average pickups near college buildings
col_pickups <- near_college_check(pickups) %>%
               group_by(DATE) %>%
               summarise(AVG_COUNT = mean(COUNT))

# get the rows not included by the col_pickups
noncol_pickups_rows <- unique(unlist(mapply(function(x, y)
  sapply(setdiff(x, y), function(d) which(x==d)), pickups, col_pickups)))
# get the average pickups not near college buildings
noncol_pickups <- pickups[noncol_pickups_rows, ]%>%
                  group_by(DATE) %>%
                  summarise(AVG_COUNT = mean(COUNT))


# Graph
g <- ggplot(data = avg_pickups, aes(x = DATE, y = AVG_COUNT, color = "avg")) +
     geom_line(data = col_pickups, aes(x = DATE, y = AVG_COUNT, color = "col")) +
     geom_line(data = noncol_pickups, aes(x = DATE, y = AVG_COUNT, color = "noncol")) +
     geom_line() +
     labs(x="DATE", y="AVG_COUNT") +
     ggtitle("College Usage")
print(g)