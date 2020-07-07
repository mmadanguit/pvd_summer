# Availability Demand Model Information

## Background
The availability demand model is meant to calculate demand per tract and/or demand based on rounded lat/long information. It uses scooter availability data to determine how complete the data is for each tract to estimate what demand would have been if scooters were 100% available.

## Files/Structure
demand: main folder		
* model.R: Main file for calculating demand

dataPrep: folder with files for generating data for use in the model
* availIntervals.R: Calculates the intervals where scooters are available based on the locations dataset
* mapToTract.R: Map coordinate to tract
* pickups.R: Compute the number of trips per tract, per day, from events data
