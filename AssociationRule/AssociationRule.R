# MEDHA SINGH
# Association Rule
# This code demonstrates the summary of InstacartTransactions.csv, display first
# 3 transactions, examine the frequency of an item, create the association rules
# model, and sort by lift parameter.
# Install tidyverse, arules packages -----------------------------
# install.packages("tidyverse")
# install.packages("arules")
# Load tidyverse, arules packages --------------------------------
library(tidyverse)
library(arules)
# Set the current working directory ----------------------------------
setwd("C:/Users/ual-laptop/Documents/GitHub/AssociationRule")
# Read the InstacartTransactions.csv file into a tibble ----------------
InstacartTransactions <- read.transactions(file= "InstacartTransactions.csv",
                                           format = "single",
                                           header = TRUE,
                                           sep = ",",
                                           col = c("OrderID", "ItemID"))
# Display InstacartTransactions tibble -------------------------------------------
print(InstacartTransactions)
# Summarize the InstacartTransactions tibble -------------------------------------
print(summary(InstacartTransactions))
# Display the first 3 transactions ----------------------------------------
inspect(InstacartTransactions[1:3])
# Display frequency of a single item: 24852 -------------------------------
itemFrequency(InstacartTransactions[,"24852"])
# Convert and store the frequency values into instacartTransactionsFrequency ---
instacartTransactionsFrequency <-
  tibble(Items = names(itemFrequency(InstacartTransactions)),
         Frequency = itemFrequency(InstacartTransactions))
# Display the item frequency ----------------------------------------------
print(instacartTransactionsFrequency)
# Display the 10 most frequently purchased items --------------------------
instacartTransactionsFrequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)
# Generate the association rules model ------------------------------------
instacartTransactionRules <-
  apriori(InstacartTransactions,
          parameter = list(
            support = 0.005,
            confidence = 0.2,
            minlen = 2))
# Display the summary of instacartTransactionRules ------------------------
summary(instacartTransactionRules)
# Display the first 10 association rules ----------------------------------
inspect(instacartTransactionRules[1:10])
# Sort the association rules by lift and display the top 10 -------------------------
instacartTransactionRules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()