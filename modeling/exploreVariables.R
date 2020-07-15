# Load relevant libraries ------------------------------------------------------
library(GGally)
library(ggplot2)
library(Hmisc)
library(tidyverse)

# Import train and test data ---------------------------------------------------
dir <- "/home/marion/PVDResearch/Data/modelingData"
filenames <- c("trainData", "testData")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for (i in 1:length(filenames)) {
  assign(filenames[i], read.csv(paths[i]))
}

# Explore response and explanatory variables -----------------------------------
dir <- "/home/marion/PVDResearch/Plots" # Set directory to save plots

# Plot distribution of response variable (count)
plotResponse <- ggplot(data = trainData, aes(x = count)) +
  geom_histogram(bins = 10) +
  ggtitle("Distribute of Response Variable")

filename <- "Distribution_of_response_variable"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path) # Save plot

# Plot distribution of square root of response variable (count)
plotResponseSqrt <- ggplot(data = trainData, aes(x = sqrt(count))) +
  geom_histogram(bins = 10) +
  ggtitle("Distribute of Square Root of Response Variable")

filename <- "Distribution_of_response_variable_sqrt"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path) # Save plot

# Plot distribution of log of response variable (count)
plotResponseLog <- ggplot(data = trainData, aes(x = log(count))) +
  geom_histogram(bins = 10) +
  ggtitle("Distribute of Log of Response Variable")

filename <- "Distribution_of_response_variable_log"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path) # Save plot

# Create correlation matrix between explanatory variables 
trainDataSub <- select(trainData, -date, -time)
plotCorr <- ggcorr(trainDataSub, label = TRUE) + # Create correlation matrix between all continuous variables
  ggtitle("Correlation Matrix Between All Variables")

filename <- "Correlation_matrix_between_explanatory_variables"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path) # Save plot

# Plot distribution of explanatory variables 
createHist <- function(data, variable) {
  ggplot(data = data, aes(x = eval(as.symbol(variable)))) +
    geom_histogram(bins = 10) +
    xlab(variable)
}

plotWeekday <- createHist(trainData, "weekday")
plotSeason <- createHist(trainData, "season")
plotAWND <- createHist(trainData, "AWND")
plotPRCP <- createHist(trainData, "PRCP")
plotTAVG <- createHist(trainData, "TAVG")
plotLastCount <- createHist(trainData, "last_count")

plotExplanatory <- ggarrange(plotWeekday, plotSeason, plotAWND, plotPRCP, plotTAVG, plotLastCount)

filename <- "Distribution_of_explanatory_variables"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path) # Save plot