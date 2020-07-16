# Load relevant libraries ------------------------------------------------------
library(ggplot2)
library(randomForest)
library(tidyverse)

# Import train and test data ---------------------------------------------------
dir <- "/home/marion/PVDResearch/Data/modelingData"
filenames <- c("trainData", "testData")
paths <- file.path(dir, paste(filenames, ".csv", sep = ""))

for (i in 1:length(filenames)) {
  assign(filenames[i], read.csv(paths[i]))
}

# Remove index column 
trainData <- select(trainData, -X)
testData <- select(testData, -X) 

# Build random forest ----------------------------------------------------------
fit <- randomForest(sqrt(count)~.+sqrt(last_count)-last_count, data = select(trainData, -date))
print(fit) # View results
importance(fit) # Importance of each predictor

# Use tree to predict values on test data --------------------------------------
predict <- predict(fit, newdata = testData)^2
rValue <- round(cor(predict, testData$count)^2, digits = 4)

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
         subtitle = paste("Random Forests Model",
                          "\nR-squared:", rValue)) +
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}

plot <- createPlot(predict, testData, rValue)

# Save plot 
dir <- "/home/marion/PVDResearch/Plots"
filename <- "Hourly_average_scooter_usage_random_forest"
path <- file.path(dir, paste(filename, ".png", sep = ""))
ggsave(path)