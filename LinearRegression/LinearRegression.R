# Medha Singh
# Linear Regression
# A dataset of zoo spending is loaded into a tibble. The correlations between
# each variable and each variables respective histogram is created. A linear
# regression is created, which predicts zoo spending. Multicollinearity is
# tested.
# Install required packages for the code
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# Load required pacakages for the code
library(tidyverse)
library(corrplot)
library(olsrr)
# Set the working directory to Lab05 folder
setwd("C:/Users/ual-laptop/Documents/GitHub/LinearRegression")
# Read CSV file into a tibble and define column types
# l for logical
# n for numeric
# i for integers
# c for characters
# f for factors
# D for dates
# T for datetimes
zooSpending <- read_csv(file = "ZooVisitSpending.csv",
                        col_types = "niil",
                        col_names = TRUE)
# Display zooSpending tibble
print(zooSpending)
# Display zooSpending structure
print(str(zooSpending))
# Display zooSpending summary
print(summary(zooSpending))
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
# Call the displayAllHistgoram() functions using our zooSpending tibble
displayAllHistograms(zooSpending)
# Correlation matrix of zooSpending variables
print(round(cor(zooSpending),2))
# Correlation plot of zooSpending variables
corrplot(cor(zooSpending),
         method = "number",
         type = "lower")
# Linear regression predicting zooSpending called zooSpendingModel
zooSpendingModel <- lm(data = zooSpending,
                       formula = VisitSpending ~ .)
# Display beta coefficients for zooSpendingModel
print(zooSpendingModel)
# Display zooSpendingModel results
print(summary(zooSpendingModel))
# Test for multicollinearity among explanatory variables
ols_vif_tol(zooSpendingModel)