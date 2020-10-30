library(tidyverse)
setwd("/Users/Alice/Dropbox/pvd_summer/demand/availability/dataPrep")
source('mapTo.R')

prepCountData <- function(locData, start, end){
  "Prep for finding availibility counts"
  locData <- locData %>% 
    drop_na() %>%
    filter(vehicle_status == "available", provider != "JUMP")  %>%
    #filter(difftime(end_time, start_time, units = "mins") > 1) %>%
    select(-c(provider, vehicle_status, vehicle_status_reason, device_type, areas)) %>%
    #filter(TRACT <= 37) %>%
    filter(((start_time >= start) & (start_time <= end))  | 
             ((end_time >= start) & (end_time <= end))) %>%
    gather("typeTime","time",c("start_time","end_time"),factor_key=TRUE)
  return(locData)
}

arrangeData <- function(locData){
  locData <- locData %>%
    arrange(TRACT) %>% group_by(TRACT) %>%
    arrange(time, .by_group = TRUE)
  return(locData)
}

recordInterval <- function(intData, tract, startTime, endTime, count){
  begDay <- endTime
  begDay$sec <- 0
  begDay$min <- 0
  begDay$hour <- 6
  endDay <- endTime
  endDay$sec <- 0
  endDay$min <- 0
  endDay$hour <- 22
  
  if(as.Date(startTime) != as.Date(endTime)){
    endDay$mday <- endDay$mday - 1 # make day earlier
    intData <- recordInterval(intData,tract,startTime,endDay,count)
    intData <- recordInterval(intData,tract,begDay,endTime,count)
  }
  else{
    startTime <- max(startTime,begDay)
    endTime <- min(endTime,endDay)
    if (endTime > startTime){
      intData <- intData %>% add_row(TRACT = tract, START = as.character(startTime), 
                                     END = as.character(endTime), COUNT = count)
    }
  }

  return(intData)
}

getCountData <- function(locData,start,end){
  intervalData <- tibble(TRACT=numeric(), START=character(), END = character(),
                         COUNT = numeric())
  currTime <- start
  currCount <- 0
  currTract <- locData$TRACT[1]
  for (i in 1:nrow(locData)){
    row <- locData[i,]
    
    # Starting new tract
    if (locData$TRACT[i] != currTract){
      currTime <- start
      currCount <- 0
      currTract <- locData$TRACT[i]
    }
    
    # Find next time and count
    nextTime <- as.POSIXlt(locData$time[i])
    nextCount <- currCount + 1
    if (locData$typeTime[i] == "end_time"){
      nextCount <- nextCount - 2
    }
    
    # If within time then record interval
    if((nextTime >= start) & (currTime <= end)){
      intervalData <- recordInterval(intervalData, currTract, currTime, nextTime, currCount)
    }
    currCount <- nextCount
    currTime <- nextTime
    
  }
  return(intervalData)
}


## RUN CODE ##
dayStart <- "06:00:00"
dayEnd <- "22:00:00"
start <- as.POSIXlt("2018-11-01 06:00:00", tz="UTC")
end <- as.POSIXlt("2019-10-31 22:00:00", tz="UTC")
file <- "locations_for_multiple_providers_from_18-11-01_to_19-11-01.csv"

### INTERVALS BY TRACT ###

locationData <- read_csv(file) %>% mapToTract()
#write.csv("locationTRACT.csv")
preppedData <- prepCountData(locationData,start,end)

#fileInf <- "pickupsUnavailIntervals.csv"
#locationDataInf <- read_csv(fileInf)
#locationDataInf <- locationDataInf %>% 
#  gather("typeTime","time",c("start_time","end_time"),factor_key=TRUE)
#preppedData <- rbind(preppedData,locationDataInf)

preppedData <- arrangeData(preppedData)

intervalData <- getCountData(preppedData,start,end)
#write.csv(intervalData,"countData.csv",row.names=FALSE)

start_times <- str_split_fixed(as.character(intervalData$START), " ", 2)
end_times <- str_split_fixed(as.character(intervalData$END), " ", 2)
intervalData <- intervalData %>% mutate(DATE=start_times[,1])
intervalData$START <- start_times[,2]
intervalData$END <- end_times[,2]
intervalData$AVAIL <- difftime(paste(intervalData$DATE, intervalData$END),
                               paste(intervalData$DATE, intervalData$START), units="min")
intervalData <- add_column(intervalData, DAY = weekdays(as.Date(intervalData$DATE)))
write.csv(intervalData,"intervalCountsTRACT.csv",row.names=FALSE)

### INTERVALS BY LAT LNG ###

file <- "locations_for_multiple_providers_from_18-11-01_to_19-11-01.csv"
locationsLatLng <- read_csv(file) %>% roundLatLng() %>% fakeTract() 
preppedData <- prepCountData(locationsLatLng,start,end)

#fileInf <- "pickupsUnavailIntervalsLATLNG.csv"
#locationDataInf <- read_csv(fileInf)
#locationDataInf <- locationDataInf %>% 
#  gather("typeTime","time",c("start_time","end_time"),factor_key=TRUE)
#preppedData <- rbind(preppedData,locationDataInf)

preppedData <- arrangeData(preppedData)

intervalData <- getCountData(preppedData,start,end)
#write.csv(intervalData,"countDataLATLNG.csv",row.names=FALSE)

start_times <- str_split_fixed(as.character(intervalData$START), " ", 2)
end_times <- str_split_fixed(as.character(intervalData$END), " ", 2)
intervalData <- intervalData %>% mutate(DATE=start_times[,1])
intervalData$START <- start_times[,2]
intervalData$END <- end_times[,2]
intervalData$AVAIL <- difftime(paste(intervalData$DATE, intervalData$END),
                               paste(intervalData$DATE, intervalData$START), units="min")
intervalData <- add_column(intervalData, DAY = weekdays(as.Date(intervalData$DATE)))
a <- str_split_fixed(as.character(intervalData$TRACT), "\\.", 2)
intervalData$LAT <- as.numeric(paste("41", a[,1], sep="."))
intervalData$LNG <- as.numeric(paste("-71", a[,2], sep="."))
intervalData <- select(intervalData, -c(TRACT))
write.csv(intervalData,"intervalCountsLATLNG.csv",row.names=FALSE)

# Generating inferred intervals
# locationsLatLng <- read_csv(file) %>% drop_na() %>% roundLatLng() %>% fakeTract() 
# locationsLatLng$length = difftime(locationsLatLng$end_time, locationsLatLng$start_time,units="min")
# groupedLatLng = locationsLatLng %>% group_by(TRACT) %>% summarize(avgAVAIL = mean(length))
# meanLength = mean(locationsLatLng$length)
# pickupsUnvail = read.csv("pickupsUnavailLATLNG.csv")
# pickupsUnvail <- pickupsUnavail %>% fakeTract()
# pickupsUnavail$end_time = as.POSIXlt(paste(as.Date(pickupsUnavail$DATE),pickupsUnavail$TIME))
# pickupsUnavail$start_time = pickupsUnavail$end_time
# for (i in 1:dim(pickupsUnvail)[1]){
#   result <- groupedLatLng %>% filter(TRACT == pickupsUnavail$TRACT[i]) %>% select(avgAVAIL)
#   pickupsUnavail$start_time[i] <- pickupsUnavail$end_time[i] - result$avgAVAIL[1]
#   if (is.na(pickupsUnavail$start_time[i]) == TRUE){
#     pickupsUnavail$start_time[i] <- pickupsUnavail$end_time[i] - meanLength
#   }
# }
# pickupsUnavail <- pickupsUnavail %>% select(c(lat, lng, TRACT, start_time, end_time))
# write.csv(pickupsUnvail, "pickupsUnvailIntervalsLATLNG.csv", row.names=FALSE)
