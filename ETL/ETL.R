# Medha Singh
# ETL
# Description: In this code, we are first trying to understand the structure of
# the data set and how its contents are spread or located in the columns
# respectively. We try to observe the measures of central tendency
# (mean, median). Upon understanding the data set, we limit our focus to a few
# features of the data set, and then try to analyze this data by using operators
# and grouping/filtering/sorting functions. Finally, we visualize the data using
# the functions available in the ggplot package.
# Install, load Tidyverse and set working directory
# install.packages("tidyverse")
library(tidyverse)
setwd("C:/Users/ual-laptop/Documents/GitHub/ETL")
# Read the groceryTransactions1 data set into a tibble and display
# contents
groceryTransactions1 <- read_csv(file = "GroceryTransactions.csv",
                                 col_types = "iDffffifffffffin")
print(groceryTransactions1)
print(groceryTransactions1[1:20,])
# Display structure and summary of groceryTransactions1 data set
print(glimpse(groceryTransactions1))
print(summary(groceryTransactions1))
# Use summarize() function from dplyr package over groceryTransactions1
print(summarize(.data = groceryTransactions1,
                mean(Revenue),
                median(UnitsSold),
                sd(Revenue),
                IQR(UnitsSold),
                min(Revenue),
                max(Children)))
# Create a new tibble with subsetted features from groceryTransactions1
groceryTransactions2 <- select(.data = groceryTransactions1,
                               PurchaseDate,
                               Homeowner,
                               Children,
                               AnnualIncome,
                               UnitsSold,
                               Revenue)
# Displaying filtered records of groceryTransactions2 data set
print(filter(.data = groceryTransactions2,
             Homeowner == "N"&
               Children >= 4))
print(filter(.data = groceryTransactions2,
             AnnualIncome == "$150K +" |
               UnitsSold > 6))
# Grouping and sorting the groceryTransactions2 data set
print(groceryTransactions2 %>%
        group_by(AnnualIncome) %>%
        summarize(AverageTransactionRevenue = mean(Revenue)) %>%
        arrange(desc(AverageTransactionRevenue)),
      n = Inf)
# Creating and displaying new tibble groceryTransactions3
groceryTransactions3 <- groceryTransactions2 %>%
  mutate(AveragePricePerUnit = Revenue / UnitsSold)
print(groceryTransactions3)
# Creating a histogram with required specifications using ggplot() package
histogramAveragePricePerUnit <- ggplot(data = groceryTransactions3,
                                       aes(x=AveragePricePerUnit))
histogramAveragePricePerUnit + geom_histogram(binwidth = 1,
                                              color = "black",
                                              fill = "orange",
                                              alpha = 0.5)
# Creating a box plot with required specifications using ggplot() package
boxplotRevenue <- ggplot(data = groceryTransactions3,
                         aes(x = Revenue))
boxplotRevenue + geom_boxplot(color = "#0C234B",
                              fill = "#AB0520")