# This script fits a GLM to the dataset provided. The goal of the analysis is
# to determine how total floral scent emission effects the number of fruits
# produced. The fitness variable doesn't make sense here because to me taller
# likely means to be heavier--in other words, weight and fitness are correlated

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

# Filter data and make grouped variables factors
mutate(df, across(c(Pop, Block), as.factor))

# Fit model
m1 <- glmmTMB(fruits ~ height, data = df, family = nbinom2())
m2 <- glmmTMB(
  fruits ~ height + (1|Pop) + (1|Block), data = df, family = nbinom2()
)

# Check support for mixed model
DAIC <- summary(m2)$AIC[1] - summary(m1)$AIC[1]

# Extract parameter estimates
params <- summary(m2)$coef$cond

# Generate sequence of input values to generate predictions on
preds <- matrix(seq(min(df$height), max(df$height), length.out = 10))
preds <- cbind(preds, exp(params[1, 1] + params[2, 1] * preds))

# Using predict function
preds2 <- predict(m2, newdata = data.frame(height = seq(min(df$height), max(df$height), length.out = 10)), type = "response", se.fit = TRUE, re.form = NA)

# Create plot
plot(
  df$height,
  df$fruits,
  xlab = "Plant Height (cm)",
  ylab = "Number of Fruits Produced",
  col = rgb(0, 0, 0, 0.25),
  pch = 16
)

# Create sequence of evenly spaced heights and generate predictions
lines(preds, col = "red")
