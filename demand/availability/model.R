library(tidyverse)
library(sf)
library(mapview)
library(pracma)

# fol <- "/home/marion/PVDResearch/Data/demandData/"
# demandLatLng  <- read.csv(paste0(fol, "demandLatLng.csv")) 
# demandTRACT  <- read.csv(paste0(fol, "demandTRACT.csv")) 

avg <- function(trips, latLng = FALSE, type = "demand") {
  "Find daily average for trip data over period of time.
  trips: demand dataframe"
  if (latLng) {
    avg <- trips %>% group_by(LAT, LNG) 
  }
  else {
    avg <- trips %>% group_by(TRACT)
  }
  # print(nrow(summarize(avg %>% group_by(DATE))))
  numDays <- nrow(summarize(avg %>% group_by(DATE)))
  if (type == "demand") { # Create summary statistics for demand data
    avg <- avg %>%
      summarize(meanTrips = mean(ADJTRIPS, na.rm = TRUE),
                medTrips = median(ADJTRIPS, na.rm = TRUE),
                stdTrips = sd(ADJTRIPS, na.rm = TRUE),
                zeroTrips = sum(ADJTRIPS == 0, na.rm = TRUE)/numDays, #This does zeroTrips as a % of every day that any tract has data
                # zeroTrips = sum(ADJTRIPS == 0, na.rm = TRUE)/length(ADJTRIPS), #This does zeroTrips as a % of every day that this tract has data
                
                meanAvailPct = mean(AVAILPCT, na.rm = TRUE),
                medAvailPct = median(AVAILPCT, na.rm = TRUE),
                stdAvailPct = sd(AVAILPCT, na.rm = TRUE),
                zeroAvailPct = sum(AVAILPCT == 0, na.rm = TRUE), # Days w/ zero avail
                meanAvail = mean(COUNTTIME, na.rm = TRUE), 
                medAvail = median(COUNTTIME, na.rm = TRUE), 
                stdAvail = sd(COUNTTIME, na.rm = TRUE),
                zeroAvail = sum(COUNTTIME == 0, na.rm = TRUE))
  } 
  else if (type == "pickup") { # Create summary statistics for pickup data
    avg <- avg %>%
      summarize(meanTrips = mean(TRIPS, na.rm = TRUE),
                medTrips = median(TRIPS, na.rm = TRUE),
                stdTrips = sd(TRIPS, na.rm = TRUE),
                zeroTrips = sum(TRIPS == 0, na.rm = TRUE)/numDays, #This does zeroTrips as a % of every day that any tract has data
                # zeroTrips = sum(ADJTRIPS == 0, na.rm = TRUE)/length(ADJTRIPS), #This does zeroTrips as a % of every day that this tract has data
                
                meanAvailPct = mean(AVAILPCT, na.rm = TRUE),
                medAvailPct = median(AVAILPCT, na.rm = TRUE),
                stdAvailPct = sd(AVAILPCT, na.rm = TRUE),
                zeroAvailPct = sum(AVAILPCT == 0, na.rm = TRUE), # Days w/ zero avail
                meanAvail = mean(COUNTTIME, na.rm = TRUE), 
                medAvail = median(COUNTTIME, na.rm = TRUE), 
                stdAvail = sd(COUNTTIME, na.rm = TRUE),
                zeroAvail = sum(COUNTTIME == 0, na.rm = TRUE))
  }
  else if (type == "difference") { # Create summary statistics for difference in pickups and demand
    avg <- avg %>%
      summarize(meanAdjTrips = mean(ADJTRIPS, na.rm = TRUE),
                medAdjTrips = median(ADJTRIPS, na.rm = TRUE),
                stdAdjTrips = sd(ADJTRIPS, na.rm = TRUE),
                zeroAdjTrips = sum(ADJTRIPS == 0, na.rm = TRUE)/numDays, #This does zeroTrips as a % of every day that any tract has data
                # zeroTrips = sum(ADJTRIPS == 0, na.rm = TRUE)/length(ADJTRIPS), #This does zeroTrips as a % of every day that this tract has data
                
                meanTrips = mean(TRIPS, na.rm = TRUE),
                medTrips = median(TRIPS, na.rm = TRUE),
                stdTrips = sd(TRIPS, na.rm = TRUE),
                zeroTrips = sum(TRIPS == 0, na.rm = TRUE)/numDays, #This does zeroTrips as a % of every day that any tract has data
                # zeroTrips = sum(TRIPS == 0, na.rm = TRUE)/length(ADJTRIPS), #This does zeroTrips as a % of every day that this tract has data
                
                
                meanAvailPct = mean(AVAILPCT, na.rm = TRUE),
                medAvailPct = median(AVAILPCT, na.rm = TRUE),
                stdAvailPct = sd(AVAILPCT, na.rm = TRUE),
                zeroAvailPct = sum(AVAILPCT == 0, na.rm = TRUE), # Days w/ zero avail
                meanAvail = mean(COUNTTIME, na.rm = TRUE), 
                medAvail = median(COUNTTIME, na.rm = TRUE), 
                stdAvail = sd(COUNTTIME, na.rm = TRUE),
                zeroAvail = sum(COUNTTIME == 0, na.rm = TRUE)
                ) %>%
      mutate(meanTrips = meanAdjTrips-meanTrips,
             medTrips = medAdjTrips-medTrips,
             stdTrips = stdAdjTrips-stdTrips,
             zeroTrips = zeroAdjTrips-zeroTrips)
  }
  avg <- avg %>% mutate_if(is.numeric, round, 3) %>% drop_na()
  avg[avg == Inf] <- NA
  return(avg)
}

geoData <- function(trips) {
  "Add geometry data"
  geoData <- readRDS("censusData/riDataGeo.Rds") %>% 
    arrange(TRACT) %>% 
    filter(as.logical(match(TRACT, trips$TRACT)))
  trips <- trips %>%
    add_column(GEOMETRY = geoData$geometry, NAME = geoData$NAME)
  return(st_as_sf(trips, crs = 4326))
}

latLngRect <- function(westLng, eastLng, southLat, northLat) {
  df <- tibble("lng" = as.numeric(), "lat" = as.numeric(), "ID" = as.numeric())
  for (id in 1:length(westLng)) {
    df <- df %>% 
      # Goes upper left, bottom left, bottom right, upper right
      add_row(lng = westLng[id], lat = northLat[id], ID = id) %>% # Upper left
      add_row(lng = westLng[id], lat = southLat[id], ID = id) %>% # Lower left
      add_row(lng = eastLng[id], lat = southLat[id], ID = id) %>% # Lower right
      add_row(lng = eastLng[id], lat = northLat[id], ID = id)
  }
  return(df)
}

geoLatLng <- function(trips) {
  "Build shape data for lat lng trip data"
  rd <- 0.01 # Rounding value
  westLng <- trips$LNG-rd/2
  eastLng <- trips$LNG+rd/2
  southLat <- trips$LAT-rd/2
  northLat <- trips$LAT+rd/2
  df <- latLngRect(westLng, eastLng, southLat, northLat)
  
  ri_tracts <- readRDS("censusData/riDataGeo.Rds")
  points <- st_as_sf(df, coords=c("lng", "lat"),crs = 4326)
  polys <- st_sf(
    aggregate(
      points$geometry,
      list(points$ID),
      function(g) {
        st_cast(st_combine(g),"POLYGON")}
    ))
  trips$geometry <- polys$geometry
  return(st_as_sf(trips))
}

genMap <- function(trips, latLng = FALSE, type = "demand", zcol = "meanTrips", colors = 12) {
  "Generate mapview for demand/pickup data"
  trips <- trips %>% avg(latLng, type)
  pal <- mapviewPalette("mapviewSpectralColors")
  if (latLng) {
    tripData <- geoLatLng(trips)
  }
  else {
    tripData <- geoData(trips)
  }
  data <- as.data.frame(tripData)[zcol]
  nonzeroData <- filter(data, data[zcol] > 0) # Finding the nonzero min makes data with 0s play better. 0 values are always default gray.
  min <- min(nonzeroData) - 0.0000000001 # The subtraction makes sure the nonzero min is included
  max <- max(nonzeroData) + 0.0000000001 # The addition makes sure the max is included
  mv <- mapview(tripData, zcol = zcol, col.regions = pal, at = logseq(min, max, colors)) #at controls the color gradient and makes it log
  # The legend colors are wrong. I think the color for a tract with value x is the legend color (ln(x)/ln(max))*max, or something along those lines but incorporating the min of the colorscale.
  return(mv)
}

# genMap(demandTRACT, latLng = FALSE, type = "difference", zcol = "meanTrips")