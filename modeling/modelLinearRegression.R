# Load relevant libraries ------------------------------------------------------
library(dplyr)
library(ggfortify)
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

# Remove index column 
trainData <- trainData %>%
  select(-X)
testData <- testData %>%
  select(-X)

# Explore response and explanatory variables -----------------------------------
# Plot histogram for response variable (count)
plotResponse <- ggplot(data = trainData, aes(x = count)) +
  geom_histogram(bins = 10) +
  ggtitle("Distribute of Response Variable") +
  xlab("numTrips")

# Calculate correlation between explanatory variables
trainDataSub <- trainData %>%
  select(-c(date, time))
plotCorr <- ggcorr(trainDataSub, label = TRUE) + # Create correlation matrix between all continuous variables
  ggtitle("Correlation Matrix Between All Variables")
plotCorrAll <- ggpairs(trainDataSub) + # Calculate correlations along with corresponding plots
  ggtitle("Correlation Matrix Between All Variables")

# Build linear models ----------------------------------------------------------
# Build linear model using last_count, the variable that is most highly correlated with count
lmSimple <- lm(count~last_count, data = trainData)
# Compute r-squared value
rSimpleTest <- summary(lmSimple)$r.squared
# Create diagnostic plots 
plotDiagSimple <- autoplot(lmSimple)

# Build linear model using last_count, TAVG, and season
lmPartial <- lm(count~last_count+TAVG+season, data = trainData)
# Compute r-squared value
rPartialTest <- summary(lmPartial)$r.squared
# Create diagnostic plots 
plotDiagPartial <- autoplot(lmPartial)

# Build linear model using all the variables except date
lmFull <- lm(count~., data = select(trainData, -date))
# Compute r-squared value
rFullTest <- summary(lmFull)$r.squared
# Create diagnostic plots 
plotDiagFull <- autoplot(lmFull)

# Use models to predict values on test data ------------------------------------
# Use simple linear model to predict values on test data
predictSimple <- predict(lmSimple, newdata = testData)
# Compute r-squared value
rSimpleTrain <- cor(predictSimple, testData$count)^2

# Use partial linear model to predict values on test data
predictPartial <- predict(lmPartial, newdata = testData)
# Compute r-squared value
rPartialTrain <- cor(predictPartial, testData$count)^2

# Use full linear model to predict values on test data
predictFull <- predict(lmFull, newdata = select(testData, -date))
# Compute r-squared value
rFullTrain <- cor(predictFull, testData$count)^2

# Create plots -----------------------------------------------------------------
# Calculate hourly average scooter usage
calculateAvg <- function(data) {
  data <- data %>%
    group_by(time) %>%
    summarise(count = mean(count))
  return(data)
}

createPlot <- function(predictedData, actualData, title) {
  # Adjust predicted data to match format of actual data
  predictedData <- predictedData %>%
    as_tibble() %>% 
    mutate(count = value,
           time = actualData$time)
  # Calculate hourly average scooter
  predictedAvg <- calculateAvg(predictedData) %>%
    mutate(id = "predicted")
  actualAvg <- calculateAvg(actualData) %>%
    mutate(id = "actual")
  # Merge predicted and actual hourly usage data
  avg <- rbind(predictedAvg, actualAvg)
  # Plot predicted vs actual hourly average scooter usage 
  plot <- ggplot(data = avg, aes(x = time, y = count, group = id)) +
    geom_line(aes(linetype = id)) + 
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

plotFull <- createPlot(predictFull, testData, "Hourly Average Scooter Usage (Test Data)")

# Save plots -------------------------------------------------------------------
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
filenames <- c("Correlation_matrix", 
               "Correlation_matrix_all", 
               "Diagnostic_plot_of_full_linear_model",
               "Diagnostic_plot_of_partial_linear_model",
               "Diagnostic_plot_of_simple_linear_model",
               "Hourly_average_scooter_usage_full",
               "Distribution_of_response_variable")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}