# This script fits a GLM to the dataset provided. The goal of the analysis is
# to determine how total floral scent emission effects the number of fruits
# produced

# Clear variables
rm(list = ls())

# Load packages
library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(glmmTMB, quietly = TRUE)

# Load data
df <- as_tibble(
  read.table(
    here("Report", "Data", "penstemon_copy.txt"), header = TRUE, sep = "\t"
  )
)
metadata <- read.csv(here("Report", "Data", "metadata.csv"))

# Make grouping variables factors
df <- mutate(df, across(c(Pop, Block), as.factor))


plot(df$height, df$fruits, col = (df$Pop))
