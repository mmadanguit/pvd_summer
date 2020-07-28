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


find_census_val_riwac <- function(data, ri_data, census_name, col_name) {
  loc <- get_unique_loc(data) %>%
         mapToTract() %>%
         mutate(rounded_geocode = str_sub(as.character(GEOID), 1, 11))
  
  # necessary step for parameterized column names
  census_name <- c(census_name)
  col_name <- c(col_name)
  
  
  for (i in 1:dim(loc)[1]) {
    row <- which(grepl(loc$GEOID[i], ri_wac$w_geocode))
    if (length(row) != 0) { # if there exists the block geoid in ri_wac
      data[data$GEOID == loc$GEOID[i], c(col_name)] <- ri_data[row, c(census_name)]
      
    } else { # if block geoid doesn't exist in ri_wac
      # get the mean of the census_name values for each rounded_geocode
      sub_ri_wac <- ri_data %>%
        group_by(ROUNDED_GEOID) %>%
        summarize(COL = mean(get(census_name)))
      
      row <- which(grepl(loc$rounded_geocode[i], sub_ri_wac$ROUNDED_GEOID))
      data[data$ROUNDED_GEOID == loc$rounded_geocode[i], c(col_name)] <- sub_ri_wac$COL[row]
    }
  }
  return(data)
}


find_census_val_ridat <- function(data, ri_data, census_name, col_name) {
  loc <- get_unique_loc(data) %>%
         mapToTract() %>%
         mutate(rounded_geocode = str_sub(as.character(GEOID), 1, 11))
  
  for (i in 1:dim(loc)[1]) {
    row <- which(grepl(loc$rounded_geocode[i], ri_data$GEOID))
    if (length(row) != 0) { 
      data[data$ROUNDED_GEOID == loc$rounded_geocode[i], c(col_name)] <- ri_data[row, c(census_name)]
    } else { # if the tract doesn't match? although this should not happen
      # row <- which(grepl(str_sub(as.character(loc$rounded_geocode[i], -4, -1)), str_sub(as.character(ri_data$GEOID, -4, -1))))
      # print(str_sub(as.character(loc$rounded_geocode[i]), -4, -1))
      data[data$ROUNDED_GEOID == loc$rounded_geocode[i], c(col_name)] <- NA
    }
  }
  
  return(data)
}


get_density <- function(data) {
  MI_RADIUS <- 3956
  sub_data <- data %>%
              group_by(DATE, ROUNDED_GEOID) %>%
              summarize(AVG_COUNT = mean(COUNT),
                        LAT = LAT,
                        LNG = LNG)
  print(head(data))
  print(head(sub_data))
  
  for (i in 1:dim(data)[1]) {
    sum <- 0
    # ssub_data <- subset(sub_data, data$DATE[i] == sub_data$DATE)
    ssub_data <- sub_data[(data$DATE[i] == sub_data$DATE),]
    for (j in 1:dim(ssub_data)[1]) {
      dist <- haversine(c(data$LAT[i], data$LNG[i]), c(ssub_data$LAT[j], ssub_data$LNG[j]), MI_RADIUS)
      if (dist < 0.15) {
        sum <- sum + ssub_data$AVG_COUNT[j]
      }
    }
    data$DENSITY[i] <- sum
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


# -------------- GET CENSUS NUMBERS ------------------
ri_wac <- read_csv("~/PVD Summer Research/college/ri_wac_S000_JT00_2017.csv") %>%
  mutate(ROUNDED_GEOID = str_sub(as.character(w_geocode), 1, 11))

ri_dat <- read_csv("~/PVD Summer Research/pvd_summer/censusData/riData.csv")

data <- data %>%
  mutate(ROUNDED_GEOID = str_sub(as.character(GEOID), 1, 11)) %>%
  mutate(TOTJOBS = 0, POP = 0, COMM0 = 0, COMM1 = 0, 
         COMM2 = 0, COMM3 = 0, COMM4 = 0, COMM5 = 0,
         COMM6 = 0, COMM7 = 0, COMM8 = 0, AUTO = 0,
         PUBLIC  = 0, WALK = 0, COLLEGE = 0, POVERTY = 0,
         INC0 = 0, INC1 = 0, INC2 = 0, INC3 = 0,
         INC4 = 0, INC5 = 0, INC6 = 0, INC7 = 0) %>%
  find_census_val_riwac(ri_wac, "C000", "TOTJOBS") %>%
  find_census_val_ridat(ri_dat, "Pop", "POP") %>%
  find_census_val_ridat(ri_dat, "comm0", "COMM0") %>%
  find_census_val_ridat(ri_dat, "comm1", "COMM1") %>%
  find_census_val_ridat(ri_dat, "comm2", "COMM2") %>%
  find_census_val_ridat(ri_dat, "comm3", "COMM3") %>%
  find_census_val_ridat(ri_dat, "comm4", "COMM4") %>%
  find_census_val_ridat(ri_dat, "comm5", "COMM5") %>%
  find_census_val_ridat(ri_dat, "comm6", "COMM6") %>%
  find_census_val_ridat(ri_dat, "comm7", "COMM7") %>%
  find_census_val_ridat(ri_dat, "comm8", "COMM8") %>%
  find_census_val_ridat(ri_dat, "auto", "AUTO") %>%
  find_census_val_ridat(ri_dat, "public", "PUBLIC") %>%
  find_census_val_ridat(ri_dat, "walk", "WALK") %>%
  find_census_val_ridat(ri_dat, "college", "COLLEGE") %>%
  find_census_val_ridat(ri_dat, "Poverty", "POVERTY") %>%
  find_census_val_ridat(ri_dat, "inc0", "INC0") %>%
  find_census_val_ridat(ri_dat, "inc1", "INC1") %>%
  find_census_val_ridat(ri_dat, "inc2", "INC2") %>%
  find_census_val_ridat(ri_dat, "inc3", "INC3") %>%
  find_census_val_ridat(ri_dat, "inc4", "INC4") %>%
  find_census_val_ridat(ri_dat, "inc5", "INC5") %>%
  find_census_val_ridat(ri_dat, "inc6", "INC6") %>%
  find_census_val_ridat(ri_dat, "inc7", "INC7") %>%
  na.omit()


# -------------- LINEAR REGRESSION MODEL -------------
# log0 = error, so added 1 to the count when taking the log
model <- lm(log(COUNT+1) ~ COLLDIST + DOWNDIST + TOTJOBS + POP + COMM0 + 
                           COMM1 + COMM2 + COMM3 + COMM4 + COMM5 +
                           COMM6 + COMM7 + COMM8 + AUTO + PUBLIC +
                           WALK + COLLEGE + POVERTY + INC0 + INC2 + 
                           INC3 + INC4 + INC5 + INC6 + INC7, data = data)
print(summary(model))
print(summary(model)$coefficient)
print(summary(model)$r.squared)
print(sigma(model)/mean(data$COUNT))

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(model)