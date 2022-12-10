# Medha Singh
# Neural Network
# Description: Install and load the required library. Summarise the data
# and generate a neural network model and train to predict CharteredBoat.
# Plot the neural network. Generate probabilities for the testing set.
# Compute confusion
# matrix to measure the accuracy of the model
# Install the tidyverse and neuralnet packages
# install.packages("tidyverse")
# install.packages("neuralnet")
# load the required library
library(tidyverse)
library(neuralnet)
# Set the working directory to Lab11 folder
setwd("C:/Users/ual-laptop/Documents/GitHub/NeuralNetwork")
# Read FishingCharter.csv into a tibble called fishingCharter
fishingCharter <- read_csv(file = "FishingCharter.csv",
                           col_types = "lnn",
                           col_names = TRUE)
# Display fishingCharter in the console
print(fishingCharter)
# Display the structure of fishingCharterin the console
print(str(fishingCharter))
# Display the summary of fishingCharter in the console
print(summary(fishingCharter))
# Scale the AnnualIncome and CatchRate variables
fishingCharter <- fishingCharter %>%
  mutate(AnnualIncomeScaled = (AnnualIncome - min(AnnualIncome))/
           (max(AnnualIncome) - min(AnnualIncome)))
fishingCharter <- fishingCharter %>%
  mutate(CatchRateScaled = (CatchRate - min(CatchRate))/
           (max(CatchRate) - min(CatchRate)))
# Randomly split the dataset into fishingCharterTraining (75% of records)
# and fishingCharterTesting (25% of records) using 591 as the random seed
set.seed(591)
sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter)*0.75),
                    replace = FALSE)
fishingCharterTraining <- fishingCharter[sampleSet,]
fishingCharterTesting <- fishingCharter[-sampleSet,]
# Generate the neural network model to predict CharteredBoat (dependent
# variable) using AnnualIncomeScaled and CatchRateScaled (independent
# variables). Use 3 hidden layers. Use "logistic" as the smoothing method
# and set linear. Output to FALSE.
fishingCharterNeuralNet <- neuralnet(
  formula = CharteredBoat ~ AnnualIncomeScaled + CatchRateScaled,
  data = fishingCharterTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE
)
# Display the neural network numeric results
print(fishingCharterNeuralNet$result.matrix)
# Visualize the neural network
plot(fishingCharterNeuralNet)
# Use fishingCharterNeuralNet to generate probabilities on the
# fishingCharterTesting data set and store this in
fishingCharterProbability <- compute(fishingCharterNeuralNet,
                                     fishingCharterTesting)
# Display the probabilities from the testing dataset on the console
print(fishingCharterProbability$net.result)
# Convert probability predictions into 0/1 predictions and store this
# into fishingCharterPrediction
fishingCharterPrediction <-
  ifelse(fishingCharterProbability$net.result > 0.5, 1, 0)
# Display the 0/1 predictions on the console
print(fishingCharterPrediction)
# Evaluate the model by forming a confusion matrix
fishingCharterCinfusionMatrix <-
  table(fishingCharterTesting$CharteredBoat,
        fishingCharterPrediction)
# Display the confusion matrix on the console
print(fishingCharterCinfusionMatrix)
# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterCinfusionMatrix)) /
  nrow(fishingCharterTesting)
# Display the predictive accuracy on the console
print(predictiveAccuracy)
