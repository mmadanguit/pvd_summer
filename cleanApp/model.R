library(tidyverse)
library(sf)
library(mapview)
library(pracma)
source("mapTo.R")

# To enable labels, uncomment lines 299 and 300

tripsVariables <- c("meanTrips", "medTrips", "stdTrips")

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
                zeroAvailPct = sum(AVAILPCT == 0, na.rm = TRUE)/numDays, # Days w/ zero avail
                meanAvail = mean(COUNTTIME, na.rm = TRUE), 
                medAvail = median(COUNTTIME, na.rm = TRUE), 
                stdAvail = sd(COUNTTIME, na.rm = TRUE)
                # zeroAvail = sum(COUNTTIME == 0, na.rm = TRUE)
                )
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
                zeroAvailPct = sum(AVAILPCT == 0, na.rm = TRUE)/numDays, # Days w/ zero avail
                meanAvail = mean(COUNTTIME, na.rm = TRUE), 
                medAvail = median(COUNTTIME, na.rm = TRUE), 
                stdAvail = sd(COUNTTIME, na.rm = TRUE)
                # zeroAvail = sum(COUNTTIME == 0, na.rm = TRUE)
                )
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
                zeroAvailPct = sum(AVAILPCT == 0, na.rm = TRUE)/numDays, # Days w/ zero avail
                meanAvail = mean(COUNTTIME, na.rm = TRUE), 
                medAvail = median(COUNTTIME, na.rm = TRUE), 
                stdAvail = sd(COUNTTIME, na.rm = TRUE)
                # zeroAvail = sum(COUNTTIME == 0, na.rm = TRUE)
                ) %>%
      mutate(meanTrips = meanAdjTrips-meanTrips,
             medTrips = medAdjTrips-medTrips,
             stdTrips = stdAdjTrips-stdTrips)
  }
  avg <- avg %>% mutate_if(is.numeric, round, 3)
  avg[avg == Inf] <- NA
  return(avg)
}

geoData <- function(trips) {
  "Add geometry data"
  geoData <- readRDS("data/riDataGeo.Rds") %>% 
    arrange(TRACT) %>% 
    filter(as.logical(match(TRACT, trips$TRACT)))
  trips <- trips %>%
    add_column(GEOMETRY = geoData$geometry, NAME = str_extract(geoData$NAME, "^([^,]*)"),)
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
  
  ri_tracts <- readRDS("data/riDataGeo.Rds")
  points <- st_as_sf(df, coords=c("lng", "lat"),crs = 4326)
  polys <- st_sf(
    aggregate(
      points$geometry,
      list(points$ID),
      function(g) {
        st_cast(st_combine(g),"POLYGON")}
    ))
  trips$geometry <- polys$geometry
  trips$NAME <- paste0("Lat: ", trips$LAT, ", Long: ", trips$LNG)
  return(st_as_sf(trips))
}

genMap <- function(trips, latLng = FALSE, type = "demand", zcol = "meanTrips", colors = 11) {
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
  return(mv@map)
}

genMapCol <- function(trips, latLng = FALSE, type = "demand", zcol = "meanTrips", colors = 10) {
  "Generate mapview for demand/pickup data with updated colors"
  if (latLng) {
    # Fake tract before grouping
    latD <- str_sub(as.character(trips$LAT), 4, -1) 
    lngD <- str_sub(as.character(trips$LNG), 5, -1)
    trips$TRACT <- as.numeric(paste(latD, lngD, sep = "."))
    trips <- trips %>% avg(FALSE, type) %>% undoFakeTract()
    
    # Add back lat/lng after grouping
    a <- str_split_fixed(as.character(trips$TRACT), "\\.", 2)
    trips$LAT <- as.numeric(paste("41", a[,1], sep="."))
    trips$LNG <- as.numeric(paste("-71", a[,2], sep="."))
    tripData <- geoLatLng(trips)
  }
  else {
    trips <- trips %>% avg(FALSE, type)
    tripData <- geoData(trips)
  }

  # Log transform to get color scale
  zcolData <- as.data.frame(tripData)[,zcol]
  tripData$logzcol <- log(zcolData+0.001) #Get the log of the selected zcol and add that as a column
  legendVals <- tripData$logzcol
  if(zcol %in% tripsVariables & type != "difference"){ #If meanTrips, medTrips, or stdTrips and making a pickup or estimated demand map
    legendVals <- c(log(0.001), legendVals, log(350)) #Add log(0.001) to the beginning and log(350) to the end of the legend vals 
  } else if (zcol %in% tripsVariables & type == "difference"){
    legendVals <- c(log(0.001), legendVals, log(10))
  }
  pal <- colorNumeric(palette = "plasma", domain = legendVals) #Make the domain of the color scale the legend values. The modification adding log(350) makes the colors constant for comparing pickup and demand
  
  # print(tripData)
  
  popupHTML <- sprintf( #Make a list of labels with HTML styling for each census tract
    "<style>
      th, td {
        padding-right: 5px;
      }
    </style>
    <strong>%s</strong><br>
    <table>
      <tr>
        <td>Mean Trips/Day</td>
        <td>%g</td>
      </tr>
      <tr>
        <td>Median Trips/Day</td>
        <td>%g</td>
      </tr>
      <tr>
        <td>Standard Deviation Trips/Day</td>
        <td>%g</td>
      </tr>
    </table>
    ",
    tripData$NAME,
    tripData$meanTrips,
    tripData$medTrips,
    tripData$stdTrips
  ) %>% lapply(htmltools::HTML)
  if(type == "pickup"){
    popupHTML <- sprintf( #Make a list of labels with HTML styling for each census tract
      "<style>
        th, td {
          padding-right: 5px;
        }
      </style>
      <strong>%s</strong><br>
      <table>
        <tr>
          <td>Mean Trips/Day</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Median Trips/Day</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Standard Deviation Trips/Day</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Mean Scooters Available/Day</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Median Scooters Available/Day</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Standard Deviation of Available/Day</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Mean %% of Day with Scooters Available</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Median %% of Day with Scooters Available</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Standard Deviation of Available %%</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>%% of Days with Zero Trips</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>%% of Days with Zero Available Scooters</td>
          <td>%g</td>
        </tr>
      </table>
    ",
      tripData$NAME,
      tripData$meanTrips,
      tripData$medTrips,
      tripData$stdTrips,
      tripData$meanAvail,
      tripData$medAvail,
      tripData$stdAvail,
      tripData$meanAvailPct,
      tripData$medAvailPct,
      tripData$stdAvailPct,
      tripData$zeroTrips,
      tripData$zeroAvailPct
    ) %>% lapply(htmltools::HTML)
  }
  if(type == "difference"){
    popupHTML <- sprintf( #Make a list of labels with HTML styling for each census tract
      "<style>
        th, td {
          padding-right: 5px;
        }
      </style>
      <strong>%s</strong><br>
      <table>
        <tr>
          <td>Difference in Mean Trips/Day</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Difference in Median Trips/Day</td>
          <td>%g</td>
        </tr>
        <tr>
          <td>Difference in Std Trips/Day Difference</td>
          <td>%g</td>
        </tr>
      </table>
    ",
      tripData$NAME,
      tripData$meanTrips,
      tripData$medTrips,
      tripData$stdTrips
    ) %>% lapply(htmltools::HTML)
  }
  
  mv <- leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = tripData, 
                weight = 1,
                color = "#000000",
                opacity = 1,
                fillOpacity = 0.7,
                fillColor = ~pal(logzcol), #Colors based on the log of the selected zcol
                smoothFactor = 0,
                # label = round(exp(tripData$logzcol), 3),
                # labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE),
                popup = popupHTML,
                highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2, bringToFront = TRUE)
                ) %>%
    addLegend(position = "bottomright", pal = pal, values = legendVals,
              title = "Providence",  
              labFormat = labelFormat(suffix="", transform = function(x) exp(x)-0.001,digits=2), #Labels are now logarithmically spaced
              opacity = 1)
  
  return(mv)
}



