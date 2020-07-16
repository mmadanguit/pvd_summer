# Load relevant libraries ------------------------------------------------------
library(dplyr)
library(ggfortify)
library(ggplot2)
library(ggpubr)
library(tidyverse)

# Import train and test data ---------------------------------------------------
dir <- "/home/marion/PVDResearch/Data/modelingData"
filenames <- c("trainData", "testData")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for (i in 1:length(filenames)) {
  assign(filenames[i], read.csv(paths[i]))
}

# Remove index column and convert weekday and season to factors
trainData <- trainData %>%
  select(-X) %>%
  mutate(weekday = as.factor(weekday),
         season = as.factor(season))
testData <- testData %>%
  select(-X) %>%
  mutate(weekday = as.factor(weekday),
         season = as.factor(season))

# Build linear model -----------------------------------------------------------
# Build linear model using all the variables except date
lm <- lm(sqrt(count)~.+sqrt(last_count)-last_count, data = select(trainData, -date))
# Compute adjusted r-squared value
rTrain <- round(summary(lm)$adj.r.squared, digits = 4)
# Create diagnostic plots 
plotDiag <- autoplot(lm)

# Use model to predict values --------------------------------------------------
# Use model to predict values on test data
predict <- predict(lm, newdata = select(testData, -date))^2
# Compute r-squared value
rTest <- round(cor(predict, testData$count)^2, digits = 4)

# Plot hourly average usage ----------------------------------------------------
# Calculate hourly average scooter usage
calculateAvg <- function(data) {
  data <- data %>%
    group_by(time) %>%
    summarise(count = mean(count))
  return(data)
}

createPlot <- function(predictedData, actualData, rValue) {
  # Adjust predicted data to match format of actual data
  predictedData <- predictedData %>%
    as_tibble() %>% 
    mutate(count = value,
           time = actualData$time)
  # Calculate hourly average scooter usage
  predictedAvg <- calculateAvg(predictedData) %>%
    mutate(id = "predicted")
  actualAvg <- calculateAvg(actualData) %>%
    mutate(id = "actual")
  # Merge predicted and actual hourly usage data
  avg <- rbind(predictedAvg, actualAvg)
  # Plot predicted vs actual hourly average scooter usage 
  plot <- ggplot(data = avg, aes(x = time, y = count, group = id)) +
    geom_line(aes(linetype = id)) + 
    labs(title = "Hourly Average Scooter Usage",
         subtitle = paste("Linear Regression Model", 
                          "\nR-squared:", rValue)) +
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

plotUsage <- createPlot(predict, testData, rTest)

# Plot residuals vs variables --------------------------------------------------
createPlotResiduals <- function(data, variable) {
  # Plot residuals vs variables
  plot <- ggplot(data, aes(x, y)) +
    geom_point() +
    geom_smooth(method = "lm") +
    xlab(variable) +
    ylab("residuals")
  return(plot)
}

timeData <- data.frame(x = trainData$time, y = residuals(lm))
weekdayData <- data.frame(x = trainData$weekday, y = residuals(lm))
seasonData <- data.frame(x = trainData$season, y = residuals(lm))
AWNDData <- data.frame(x = trainData$AWND, y = residuals(lm))
PRCPData <- data.frame(x = trainData$PRCP, y = residuals(lm))
TAVGData <- data.frame(x = trainData$TAVG, y = residuals(lm))
lastCountData <- data.frame(x = sqrt(trainData$last_count), y = residuals(lm))

plotTime <- createPlotResiduals(timeData, "time") 
plotWeekday <- createPlotResiduals(weekdayData, "weekday")
plotSeason <- createPlotResiduals(seasonData, "time")
plotAWND <- createPlotResiduals(AWNDData, "AWND")
plotPRCP <- createPlotResiduals(PRCPData, "PRCP")
plotTAVG <- createPlotResiduals(TAVGData, "TAVG")
plotLastCount <- createPlotResiduals(lastCountData, "sqrt(last_count)")

# Save plots -------------------------------------------------------------------
plots <- mget(ls(pattern="plot"))
dir <- "/home/marion/PVDResearch/Plots"
filenames <- c("Residuals_vs_AWND",
               "Diagnostic_plot_of_linear_model",
               "Residuals_vs_last_count",
               "Residuals_vs_PRCP",
               "Residuals_vs_season",
               "Residuals_vs_TAVG",
               "Residuals_vs_time",
               "Hourly_average_scooter_usage_linear",
               "Residuals_vs_weekday")
paths <- file.path(dir, paste(filenames, ".png", sep = ""))

for(i in 1:length(plots)){
  invisible(mapply(ggsave, file = paths[i], plot = plots[i]))
}

# Understand the weird line in residuals vs fitted -----------------------------
# Select data points that fall on the line y=-x+1
weirdData <- data.frame(trainData, 
                        predicted_count = predict(lm, select(trainData, -date)),
                        residual = residuals(lm)) %>%
  filter(residual < -predicted_count+1) %>%
  mutate(x = predicted_count, 
         y = residual)

# Plot points to check that they all fall on the line
plotWeird <- createPlotResiduals(sample, "fitted values")

# Plot distribution of explanatory variables 
createHist <- function(data, variable) {
  ggplot(data = data, aes(x = eval(as.symbol(variable)))) +
    geom_histogram(bins = 10) +
    xlab(variable)
}

plotWeekday <- createHist(weirdData, "weekday")
plotSeason <- createHist(weirdData, "season")
plotAWND <- createHist(weirdData, "AWND")
plotPRCP <- createHist(weirdData, "PRCP")
plotTAVG <- createHist(weirdData, "TAVG")
plotLastCount <- createHist(weirdData, "last_count")

plotExplanatory <- ggarrange(plotWeekday, plotSeason, plotAWND, plotPRCP, plotTAVG, plotLastCount)

# Save plot
filename <- "Distribution_of_explanatory_variables_weird"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path)