library(tidyverse)
library(dplyr)
library(pracma)
library(ggplot2)
library(lubridate)
library(tigris)
source('downtown_loc.R')
source('college_loc.R')
source('~/PVD Summer Research/demand/mapToTract.R')

get_unique_loc <- function(data) {
  "
  Only get the unique combinations of LAT LNG
  "
  data <- data %>%
    select(-c(START, END, COUNT, AVAIL, DATE, DAY)) %>%
    distinct(LAT, LNG)
  
  return(data)
}

dist_to_downtown <- function(data) {
  MI_RADIUS <- 3956
  downtown_loc <- get_downtown()
  loc <- get_unique_loc(data)
  
  # check whether their distance is within the college_locs
  for (i in 1:dim(loc)[1]) {
    min_dist <- 987654321
    for (j in 1:dim(downtown_loc)[1]) {
      dist <- haversine(c(loc$LAT[i], loc$LNG[i]), c(downtown_loc$LAT[j], downtown_loc$LNG[j]), MI_RADIUS)
      if (dist < min_dist) { # if dist is smaller than min dist 
        min_dist <- dist
      }
    }
    data[(data$LAT == loc$LAT[i] | data$LNG == loc$LNG[i]),]$DOWNDIST <- min_dist
  }
  
  return(data)
}


dist_to_college <- function(data) {
  MI_RADIUS <- 3956
  college_loc <- get_colleges()
  loc <- get_unique_loc(data)
  
  # check whether their distance is within the college_locs
  for (i in 1:dim(loc)[1]) {
    min_dist <- 987654321
    for (j in 1:dim(college_loc)[1]) {
      dist <- haversine(c(loc$LAT[i], loc$LNG[i]), c(college_loc$LAT[j], college_loc$LNG[j]), MI_RADIUS)
      if (dist < min_dist) { # if dist is smaller than min dist 
        min_dist <- dist
      }
    }
    data[(data$LAT == loc$LAT[i] | data$LNG == loc$LNG[i]),]$COLLDIST <- min_dist
  }
  
  return(data)
}


find_density <- function(data) {
  MI_RADIUS <- 3956
  college_loc <- get_colleges()
  loc <- get_unique_loc(data)
  
  for (i in 1:dim(loc)[1]) {
    for (j in 1:dim(college_loc)[1]) {
      dist <- haversine(c(loc$LAT[i], loc$LNG[i]), c(college_loc$LAT[j], college_loc$LNG[j]), MI_RADIUS)
      if (dist < 0.2) { 

      }
    }
    data[(data$LAT == loc$LAT[i] | data$LNG == loc$LNG[i]),]$COLLDIST <- min_dist
  }
  
  return(data)
}


# if lat lng doesn't output a good result, use Tracts to restrict 
#restrict to a month
# density (how many scooters are avaialble) avg availability and intervalcount - riWac_blahblah
## model_r avg_avail
pickups <- read_csv("~/PVD Summer Research/average_num_available/intervalCountsLATLNG.csv")
pickups <- pickups %>%
  filter(DATE >= "2019-4-15" & DATE <= "2019-5-15") %>%
  mutate(DOWNDIST = 0, COLLDIST = 0) %>%
  dist_to_downtown() %>%
  dist_to_college()



# -------------- GEOID RETRIEVAL ---------------------
#include population level, # of jobs, income level, other factors from census
# map to census tract and get census variables
data <- mapToTract(pickups)
# # data <- tibble(lat = pickups$LAT, lon = pickups$LNG)
# GEOID <- append_geoid(data, 'block')


# -------------- LINEAR REGRESSION MODEL -------------
# # log0 = error
# model <- lm(log(COUNT+1) ~ COLLDIST + DOWNDIST, data = pickups)
# print(summary(model))
# print(summary(model)$coefficient)
# print(summary(model)$r.squared)
# print(sigma(model)/mean(pickups$COUNT))
# 
# layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
# plot(model)