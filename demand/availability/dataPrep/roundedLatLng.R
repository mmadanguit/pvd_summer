library(plyr)
library(stringr)

loc_data <- read_csv("locations2019.csv")

# round lat lng to the nearest 0.005
loc_data$lat_rounded <- round_any(loc_data$lat, 0.005)
loc_data$lng_rounded <- round_any(loc_data$lng, 0.005)

# only save the fraction portion of the lat/lng
lat_rounded_frac <- str_sub(as.character(loc_data$lat_rounded), 4, -1)
lng_rounded_frac <- str_sub(as.character(loc_data$lng_rounded), 5, -1)

# combine the rounded lat and lng together
loc_data$TRACT_latlng <- paste(lat_rounded_frac,
                               lng_rounded_frac,
                               sep = ".")

# convert the TRACT_latlng back to numeric from character
loc_data$TRACT_latlng <- as.numeric(loc_data$TRACT_latlng)

write_csv(loc_data, "locations_rounded_latlng2019.csv")