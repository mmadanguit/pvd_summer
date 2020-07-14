# Load relevant libraries ------------------------------------------------------
library(dplyr)
library(ggfortify)
library(ggplot2)
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

# Build linear model -----------------------------------------------------------
# Build linear model using all the variables except date
lm <- lm(sqrt(count)~.+sqrt(last_count)-last_count, data = select(trainData, -date))
# Compute adjusted r-squared value
rTrain <- round(summary(lm)$adj.r.squared, digits = 4)
# Create diagnostic plots 
plotDiag <- autoplot(lm)

# Use models to predict values on test data ------------------------------------
# Use linear model to predict values on test data
predict <- predict(lm, newdata = select(testData, -date))
# Compute r-squared value
rTest <- round(cor(predict, testData$count)^2, digits = 4)

# Create plots -----------------------------------------------------------------
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
    labs(title = "Hourly Average Scooter Usage",
         subtitle = paste("R-squared:", rValue)) +
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

plot1 <- createPlot(predict, testData, rTest)

predict <- predict(lm, data = select(trainData, -date))
rTrain <- round(cor(predict, trainData$count)^2, digits = 4)
plot2 <- createPlot(predict, trainData, rTrain)

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