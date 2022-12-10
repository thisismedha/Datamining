# Medha Singh
# Data Preprocessing
# Description: This code loads data, imputes missing values, identifies # outliers. Following this, we normalise and discretize our tibble.
# Later we Dummy code the Position feature. Finally, we build a scatter # plot of Miles with TreadDepth
# Install, load Tidyverse Dummies packages and set working directory
# install.packages("tidyverse")
library(tidyverse)
# install.packages("dummy")
# library(dummy)
# install.packages("dummies", repos = NULL, type="source")
library(dummies)
setwd("C:/Users/ual-laptop/Documents/GitHub/DataPreprocessing")
# Read and display data
tireTread1 <- read_csv(file = "TireTread.csv", col_types = "cfnni")
print(tireTread1)
# Display Structure and summary of dataset
print(glimpse(tireTread1))
print(summary(tireTread1))
# Impute missing data for UsageMonths with the mean and display
tireTread2 <- tireTread1 %>%
  mutate(UsageMonths = ifelse(
    is.na(UsageMonths),
    mean(UsageMonths, na.rm = TRUE),
    UsageMonths
  ))
print(summary(tireTread2))
# Determine outliers in the TreadDepth feature
# Calculate outliers min and max and store into variable called
# outlierMin and outlierMax
outlierMin <- quantile(tireTread2$TreadDepth, .25) -
  (IQR(tireTread2$TreadDepth) * 1.5)
outlierMax <- quantile(tireTread2$TreadDepth, .75) +
  (IQR(tireTread2$TreadDepth) * 1.5)
# Keep the outliers in the dataset, but add the outliers to their own
# tibble called treadDepthOutliers
treadDepthOutliers <- tireTread2 %>%
  filter(TreadDepth < outlierMin | TreadDepth > outlierMax )
# Normalize the UsageMonths feature by taking the log of UsageMonths
# into a new feature called LogUsageMonths and store the additional
# column in a tibble called tireTread3
tireTread3 <- tireTread2 %>%
  mutate(LogUsageMonths = log(UsageMonths))
# Discretize TreadDepth into NeedsReplacing (tires with tread depth of less than
# or equal to 1.6mm need replacing) and store into new tireTread4
# tibble
tireTread4 <- tireTread3 %>%
  mutate(NeedsReplacing = TreadDepth <= 1.6)
# Dummy code the Position (LF, RF, LR, RR) feature. Start by converting
# tireTread4 into a data frame
tireTread4DataFrame <- data.frame(tireTread4)
tireTread5 <- as_tibble(dummy.data.frame(data = tireTread4DataFrame,
                                         names = "Position"))
print(tireTread5)
# Use ggplot() to build a scatter plot of Miles (x) with TreadDepth (y)
scatterPlotMilesTredDepth <- ggplot(data = tireTread5,
                                    aes(x = Miles,
                                        y = TreadDepth))
# Use the default point size, but change the point color to dark gray.
# Add a linear best fit line to the plot and color it red. Add a title # to the scatter plot, "Tire Miles and Tread Depth Scatter Plot"
scatterPlotMilesTredDepth + geom_point(color = "Dark Grey") +
  geom_smooth(method = lm,
              level = 0,
              color = "Red") +
  ggtitle("Tire Miles and Tread Depth Scatter Plot")