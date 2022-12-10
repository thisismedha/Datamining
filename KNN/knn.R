# Medha Singh
# KNN
# Import SedanSize dataset and generate k-Nearest Neighbors model and find the
# best fitting k value
# Install the tidyverse package
# install.packages("tidyverse")
# load the required library, class library was loaded for the prediction code
library(tidyverse)
library(class)
# Set the working directory to Lab07 folder
setwd("C:/Users/ual-laptop/Documents/GitHub/KNN")
# Read SedanSize.csv into a tibble called sedanSize
sedanSize <- read_csv(file = "SedanSize.csv",
                      col_types = "cfnii",
                      col_names = TRUE)
# Display sedanSize in the console
print(sedanSize)
# Display the structure of sedanSize in the console
str(sedanSize)
# Display the summary of sedanSize in the console
print(summary(sedanSize))
# Remove the MakeModel feature from the tibble, since it is information-only and
# will not be used as a variable in our model
sedanSize <- sedanSize %>%
  select(-MakeModel)
# Separate the tibble into two. One with just the label and one with the other
# variables.
sedanSizeLabels <- sedanSize %>% select(SedanSize)
sedanSize <- sedanSize %>% select(-SedanSize)
# Recreate the displayAllHistograms() function
displayAllHistograms <- function(tibbleDataSet) {
  tibbleDataSet %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key),
                              color = "black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal()
}
# Call the displayAllHistgoram() functions using our sedanSize tibble
displayAllHistograms(sedanSize)
# Randomly split the dataset into sedanSizeTraining (75% of records) and
# sedanSizeTesting (25% of records) using 517 as the random seed
set.seed(517)
sampleSet <- sample(nrow(sedanSize),
                    round(nrow(sedanSize)*0.75),
                    replace = FALSE)
sedanSizeTraining <- sedanSize[sampleSet,]
sedanSizeTrainingLabels <- sedanSizeLabels[sampleSet,]
sedanSizeTesting <- sedanSize[-sampleSet,]
sedanSizeTestingLabels <- sedanSizeLabels[-sampleSet,]
# Generate the k-nearest neighbors model using sedanSizeTraining as the train
# argument, sedanSizeTesting as the test argument,
# sedanSizeTrainingLabels$SedanSize as the cl argument, and 7 as the value for
# the k argument
sedanSizePrediction <- knn(train = sedanSizeTraining,
                           test = sedanSizeTesting,
                           cl = sedanSizeTrainingLabels$SedanSize,
                           k = 7)
# Display the predictions from the testing dataset on the console
print(sedanSizePrediction)
# Display summary of the predictions from the testing dataset
print(summary(sedanSizePrediction))
# Evaluate the model by forming a confusion matrix
sedanSizeConfusionMatrix <- table(sedanSizeTestingLabels$SedanSize,
                                  sedanSizePrediction)
# Display the confusion matrix on the console
print(sedanSizeConfusionMatrix)
# Calculate the model predictive accuracy and store it into a variable called
# predictiveAccuracy
predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix)) /
  nrow(sedanSizeTesting)
# Display the predictive accuracy on the console
print(predictiveAccuracy)
# Create a matrix of k-values with their predictive accuracy (the matrix will be
# empty and have 2 columns and 0 rows). Store the matrix into an object called
# kValueMatrix
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)
# Assign column names of "k value" and "Predictive accuracy" to the kValueMatrix
colnames(kValueMatrix) <- c("k value", "Predictive accuracy")
# Loop through odd values of k from 1 up to the number of records in the
# training dataset to determine the best fitting model. With each pass through
# the loop, store the k-value along with its predictive accuracy
for (kValue in 1:nrow(sedanSizeTraining)) {
  # Only calculate predictive accuracy if the k value is odd
  if(kValue %% 2 != 0) {
    # Generate the model
    sedanSizePrediction <- knn(train = sedanSizeTraining,
                               test = sedanSizeTesting,
                               cl = sedanSizeTrainingLabels$SedanSize,
                               k = kValue)
    # Generate the confusion matrix
    sedanSizeConfusionMatrix <- table(sedanSizeTestingLabels$SedanSize,
                                      sedanSizePrediction)
    # Calculate the predictive accuracy
    predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix)) /
      nrow(sedanSizeTesting)
    # Add a new row to the the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracy))
  }
}
# Display the kValueMatrix on the console to determine the best k-value
print(kValueMatrix)