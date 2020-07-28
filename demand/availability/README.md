# Availability Demand Model Information

## Background
The availability demand model is meant to calculate demand per tract and/or demand based on rounded lat/long information. It uses scooter availability data to determine how complete the data is for each tract to estimate what demand would have been if scooters were 100% available.

## Files/Structure
demand: main folder		
* model.R: Main file for plotting demand
* buildDemand: Builds demand data based on interval count and pickup data, using the CDF to adjust estimates based on when the data was caputered
* createCDF: Creates a CDF based on when scooter usage is highest, such that demand can be adjusted. EG if demand data only exists for peak, the demand estimate will be brought down as the scooters picked up then were picked up during the busiest part of the day.

dataPrep: folder with files for generating data for use in the model
* countIntervals.R: Calculates the intervals where scooters are available based on the locations dataset
* mapTo.R: Map coordinate to tract or rounded latLng
* pickups.R: Compute the number of trips per tract, per day, from events data
* avgNumAvail.R: Find the average number af availble scooters for each TRACT
* getAvgAvail.R:
* startDayAvail.R: Finds the total number  of scooters available for each TRACT
