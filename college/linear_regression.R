library(tidyverse)
library(dplyr)
library(pracma)
library(ggplot2)
library(lubridate)
library(tigris)
source('downtown_loc.R')
source('college_loc.R')
source('mapToTract.R')

get_unique_loc <- function(data) {
  "
  Only get the unique combinations of LAT LNG
  "
  data <- data %>%
    select(-c(START, END, COUNT, AVAIL, DATE, DAY)) %>%
    distinct(LAT, LNG)
  
  return(data)
}

get_unique_geocode <- function(data) {
  "
  Only get the unique combinations of LAT LNG
  "
  data <- data %>%
    select(-c(START, END, COUNT, AVAIL, DATE, DAY, LAT, LNG)) %>%
    distinct(GEOID)
  
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

find_census_val <- function(data, ri_data, census_name, col_name) {
  loc <- get_unique_loc(data) %>%
    mapToTract() %>%
    mutate(rounded_geocode = str_sub(as.character(GEOID), 1, 12))
  data <- data %>%
    mutate(rounded_geocode = str_sub(as.character(GEOID), 1, 12))
  census_name <- c(census_name)
  col_name <- c(col_name)
  
  
  for (i in 1:dim(loc)[1]) {
    row <- which(grepl(loc$GEOID[i], ri_wac$w_geocode))
    if (length(row) != 0) { # if there exists the block geoid in ri_wac
      data[data$GEOID == loc$GEOID[i], c(col_name)] <- ri_data[row, c(census_name)]
      
    } else { # if block geoid doesn't exist in ri_wac
      # get the mean of the census_name values for each rounded_geocode
      sub_ri_wac <- ri_data %>%
        group_by(rounded_geocode) %>%
        summarize(COL = mean(get(census_name)))
      
      row <- which(grepl(loc$rounded_geocode[i], sub_ri_wac$rounded_geocode))
      data[data$rounded_geocode == loc$rounded_geocode[i], c(col_name)] <- sub_ri_wac$COL[row]
    }
  }
  return(data)
}


# -------------- PREP PICKUP DATA --------------------
pickups <- read_csv("~/PVD Summer Research/average_num_available/intervalCountsLATLNG.csv")
pickups <- pickups %>%
  filter(DATE >= "2019-4-15" & DATE <= "2019-5-15") %>%
  mutate(DOWNDIST = 0, COLLDIST = 0) %>%
  dist_to_downtown() %>%
  dist_to_college()

# -------------- GEOID RETRIEVAL ---------------------
# map to census tract
data <- mapToTract(pickups)

# get corresponding census variables
# include population level, # of jobs, income level, other factors from census
ri_wac <- read_csv("~/PVD Summer Research/college/ri_wac_S000_JT00_2017.csv") %>%
  mutate(rounded_geocode = str_sub(as.character(w_geocode), 1, 12))


# -------------- GET CENSUS NUMBERS ------------------
data <- data %>%
  mutate(TOTJOBS = 0, LOWINCOME = 0, MEDINCOME = 0, HIGHINCOME = 0) %>%
  find_census_val(ri_wac, "C000", "TOTJOBS") %>%
  find_census_val(ri_wac, "CE01", "LOWINCOME") %>%
  find_census_val(ri_wac, "CE02", "MEDINCOME") %>%
  find_census_val(ri_wac, "CE03", "HIGHINCOME")


# -------------- LINEAR REGRESSION MODEL -------------
## log0 = error, so added 1 to the count when taking the log
model <- lm(log(COUNT+1) ~ COLLDIST + DOWNDIST + TOTJOBS + LOWINCOME + MEDINCOME + HIGHINCOME, data = data)
print(summary(model))
print(summary(model)$coefficient)
print(summary(model)$r.squared)
print(sigma(model)/mean(data$COUNT))

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(model)