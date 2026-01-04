# This script fits a GLM to the dataset provided. The goal of the analysis is
# to determine how total floral scent emission effects the number of fruits
# produced. The fitness variable doesn't make sense here because to me taller
# likely means to be heavier--in other words, weight and fitness are correlated

# Clear variables
rm(list = ls())

# Load packages
library(here, quietly = TRUE)
library(glmmTMB, quietly = TRUE)
library(MuMIn, quietly = TRUE)

# Supress update warning from MuMIn
options(MuMIn.noUpdateWarning = TRUE)

# Load data
df <- read.table(
  here("Report", "Data", "penstemon_copy.txt"), header = TRUE, sep = "\t"
)
metadata <- read.csv(here("Report", "Data", "metadata.csv"))

# Make grouped variables factors
df$Pop <- as.factor(df$Pop)
df$Block <- as.factor(df$Block)

# Fit model
m <- glmmTMB(
  fruits ~ height + (1|Pop) + (1|Block), data = df, family = nbinom2()
)


# Get pseudo-r^2
pseudo_r2 <- r.squaredGLMM(m)

# Extract parameter estimates
params <- summary(m)$coef$cond

# Create data frame with sequence to generate new predictions on
# Ensure the structure of new_data is identical to that of the original data
new_data <- data.frame(
  height = seq(min(df$height), max(df$height), length.out = 15),
  Pop = factor(NA, levels = levels(df$Pop)),
  Block = factor(NA, levels = levels(df$Block))
)

# Generate predictions and standard error estimates
pred <- predict(
  m,
  newdata = new_data,
  type = "response",
  se.fit = TRUE
)

# Create plot
plot(
  df$height,
  df$fruits,
  xlab = "Plant Height (cm)",
  ylab = "Number of Fruits Produced",
  col = rgb(0, 0, 0, 0.25),
  pch = 16
)

# Draw 95% CI ribbon
polygon(
  c(new_data$height, rev(new_data$height)),
  c(pred$fit + 1.96 * pred$se.fit, rev(pred$fit - 1.96 * pred$se.fit)),
  border = FALSE,
  col = rgb(1, 0, 0, 0.25)
)

# Draw regression line
lines(new_data$height, pred$fit, col = "red")