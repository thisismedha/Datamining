# Medha Singh
# Logistic Regression
# Description: A dataset of MobilePhone is loaded into a tibble. Correlations
# between each variable and each variable's respective histogram is created.
# Dataset is split into training and testing tibble. Class imbalance is dealt
# with using SMOTE. Logistic regression model is created, which predicts the
# probability of service cancellation. Confusion matrix is generated which
# measures the accuracy of the model.
# install required packages
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")
#load the required library
library(tidyverse)
library(corrplot)
library(olsrr)
library(smotefamily)
# Set the working directory to Lab06 folder
setwd("C:/Users/ual-laptop/Documents/GitHub/LogisticRegression")
# Read MobilePhoneSubscribers.csv into a tibble called mobilePhone
mobilePhone <- read_csv(file = "MobilePhoneSubscribers.csv",
                        col_types = "lillnininn",
                        col_names = TRUE)
# Display mobilePhone in the console
print(mobilePhone)
# Display the structure of mobilePhone in the console
print(str(mobilePhone))
# Display the summary of mobilePhone in the console
print(summary(mobilePhone))
# Create a function called DisplayAllHistograms that take in a tibble parameter
# that will display a histogram for all numeric features in the tibble
displayAllHistograms <- function(tibbleDataSet) {
  tibbleDataSet %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key),
                              color = "black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal()
}
# Call the displayAllHistgoram() functions using our mobilePhone tibble
displayAllHistograms(mobilePhone)
# Display a correlation matrix of mobilePhonerounded to two decimal places
mobilePhonerounded <- round(cor(mobilePhone), 2)
print(mobilePhonerounded)
# Display a correlation plot using the "number" method and limit output to the
# bottom left
corrplot(cor(mobilePhone),
         method = "number",
         type = "lower",
         tl.cex = 0.3)
# Remove the data plan and data usage variables from the tibble
mobilePhone <- mobilePhone %>%
  select(-DataPlan, -DataUsage)
# Randomly split the dataset into mobilePhoneTraining (75% of records) and
# mobilePhoneTesting (25% of records) using 203 as the random seed
set.seed(203)
sampleSet <- sample(nrow(mobilePhone),
                    round(nrow(mobilePhone)*0.75),
                    replace = FALSE)
mobilePhoneTraining <- mobilePhone[sampleSet,]
mobilePhoneTesting <- mobilePhone[-sampleSet,]
# Check if we have a class imbalance issue in CancelledService
summary(mobilePhone$CancelledService)
# Store magnitude of class imbalance
mobilePhoneMagnitude <- 1668/483
# Deal with class imbalance using the SMOTE technique using a duplicate size of
# 3. Save the result into a new tibble called mobilePhoneTrainingSmoted
mobilePhoneTrainingSmoted <- tibble(SMOTE(X = data.frame(mobilePhoneTraining),
                                          target = mobilePhoneTraining$CancelledService,
                                          dup_size = 3)$data)
# Convert CancelledService and RecentRenewal back into logical types
mobilePhoneTrainingSmoted <- mobilePhoneTrainingSmoted %>%
  mutate(CancelledService = as.logical(CancelledService),
         RecentRenewal = as.logical(RecentRenewal)
  )
# Get rid of the "class" column in the tibble
mobilePhoneTrainingSmoted <- mobilePhoneTrainingSmoted %>%
  select(-class)
# Check for class imbalance on the smoted dataset
summary(mobilePhoneTrainingSmoted$CancelledService)
# Generate the logistic regression model (using CancelledService as the binary
# dependent variable) and save it in an object called mobilePhoneModel
mobilePhoneModel <- glm(data = mobilePhoneTrainingSmoted,
                        family = binomial,
                        formula = CancelledService~.)
# Display the logistic regression model results using the summary() function
summary(mobilePhoneModel)
# Calculate the odds ratios for each of the 7 independent variable coefficients
exp(coef(mobilePhoneModel)["AccountWeeks"])
exp(coef(mobilePhoneModel)["RecentRenewalTRUE"])
exp(coef(mobilePhoneModel)["CustServCalls"])
exp(coef(mobilePhoneModel)["AvgCallMinsPerMonth"])
exp(coef(mobilePhoneModel)["AvgCallsPerMonth"])
exp(coef(mobilePhoneModel)["MonthlyBill"])
exp(coef(mobilePhoneModel)["OverageFee"])
# Use the model to predict outcomes in the testing dataset,treat anything below
# or equal to 0.5 as a 0, anything above 0.5 as a 1.
mobilePhonePredict <- predict(mobilePhoneModel,
                              mobilePhoneTesting,
                              type = "response")
mobilePhonePredict <- ifelse(mobilePhonePredict <= 0.5, 0, 1)
# Generate a confusion matrix of predictions
mobilePhonePredictConfusion <- table(mobilePhoneTesting$CancelledService,
                                     mobilePhonePredict)
print(mobilePhonePredictConfusion)
# Calculate the false positive rate
mobilePhonePredictConfusion[1,2]/
  (mobilePhonePredictConfusion[1,2] + mobilePhonePredictConfusion[1,1])
# Calculate the false negative rate
mobilePhonePredictConfusion[2,1]/
  (mobilePhonePredictConfusion[2,1] + mobilePhonePredictConfusion[2,2])
# Calculate the model prediction accuracy
sum(diag(mobilePhonePredictConfusion)/nrow(mobilePhoneTesting))