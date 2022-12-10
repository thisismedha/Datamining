# Medha Singh
# Naive Bayes
# Description: This code takes data from a CSV, displays the data and its structure,
# summarizes the data and splits it into a training and testing dataset using 75%
# and 25% of the data respectively, generates a Naïve Bayes model for the data,
# calculates the probabilities for each observation in the dataset, makes class
# predictions for each observation, and them finally produces a confusion matrix for
# the model and calculates the model’s predictive accuracy
# Install the tidyverse and e1071 packages
# install.packages("tidyverse")
# install.packages("e1071")
# load the required library
library(tidyverse)
library(e1071)
# Set the working directory to Lab07 folder
setwd("C:/Users/ual-laptop/Documents/GitHub/NaiveBayes")
# Read DwellingType.csv into a tibble called dwellingType
dwellingType <- read_csv(file = "DwellingType.csv",
                         col_types = "filll",
                         col_names = TRUE)
# Display dwellingType in the console
print(dwellingType)
# Display the structure of dwellingType in the console
print(str(dwellingType))
# Display the summary of dwellingType in the console
print(summary(dwellingType))
# Randomly split the dataset into dwellingTypeTraining (75% of records) and
# dwellingTypeTesting (25% of records) using 154 as the random seed
set.seed(154)
sampleSet <- sample(nrow(dwellingType),
                    round(nrow(dwellingType)*0.75),
                    replace = FALSE)
dwellingTypeTraining <- dwellingType[sampleSet,]
dwellingTypeTesting <- dwellingType[-sampleSet,]
# Generate the Naive Bayes model to predict DwellingType based on the other
# variables in the dataset
dwellingTypeModel <- naiveBayes( formula = DwellingType ~ . ,
                                 data = dwellingTypeTraining,
                                 laplace = 1)

# Build probabilities for each record in the testing dataset and store them in
# dwellingTypeProbability
dwellingTypeProbability <- predict(dwellingTypeModel,
                                   dwellingTypeTraining,
                                   type = "raw")
# Display dwellingTypeProbability on the console
print(dwellingTypeProbability)
# Predict classes for each record in the testing dataset and store them in
# dwellingTypePrediction
dwellingTypePrediction <- predict(dwellingTypeModel,
                                  dwellingTypeTesting,
                                  type = "class")
# Display dwellingTypePrediction on the console
print(dwellingTypePrediction)
# Evaluate the model by forming a confusion matrix
dwellingTypeConfusionMatrix <- table(dwellingTypeTesting$DwellingType,
                                     dwellingTypePrediction)
# Display the confusion matrix on the console
print(dwellingTypeConfusionMatrix)
# Calculate the model predictive accuracy and store it into a variable called
# predictiveAccuracy
dwellingTypeAccuracy <- sum(diag(dwellingTypeConfusionMatrix))/
  nrow(dwellingTypeTesting)
# Display the predictive accuracy on the console
print(dwellingTypeAccuracy)