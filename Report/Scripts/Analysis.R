# This script fits a GLM to the dataset provided. The goal of the analysis is
# to determine how total floral scent emission effects the number of fruits
# produced. The fitness variable doesn't make sense here because to me taller
# likely means to be heavier--in other words, weight and fitness are correlated


# ===================================
# CONFIGURE ENVIRONMENT AND LOAD DATA
# ===================================

# Clear variables
rm(list = ls())

# Load packages
library(here, quietly = TRUE)
library(glmmTMB, quietly = TRUE)
library(performance, quietly = TRUE)
library(MuMIn, quietly = TRUE)

# Supress update warning from MuMIn
options(MuMIn.noUpdateWarning = TRUE)

# Load raw data
df <- read.table(
  here("Report", "Data", "penstemon_copy.txt"), header = TRUE, sep = "\t"
)

# Load metadata file for column lookup
# TODO: delete this when no longer needed
metadata <- read.csv(here("Report", "Data", "metadata.csv"))

# Make grouped variables factors
df$Pop <- as.factor(df$Pop)
df$Block <- as.factor(df$Block)


# =========================================
# FIT MODEL AND EXTRACT PARAMETER ESTIMATES
# =========================================

# Fit model
m <- glmmTMB(
  aborted ~ 1 + (1|Pop) + (1|Block), data = df, family = nbinom2()
)


# ================================
# GENERATE PREDICTIONS USING MODEL
# ================================

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
  type = "response",  # Calculate predictions on response scale
  se.fit = TRUE
)


# ==================================
# BUILD SUMMARY STATISTICS TEXT FILE
# ==================================

# Get pseudo-r^2
cat(r.squaredGLMM(m)[3, 2], file = here("Report", "Output", "summary.txt"))

# Perform variance partitioning

VarAmongPop <- attr(VarCorr(m)$cond$Pop, "stddev")^2
VarAmongBlock <- attr(VarCorr(m)$cond$Block, "stddev")^2
VarWithinGroups <- attr(VarCorr(m)$cond, "sc")^2
PctVarExpByPop <- VarAmongPop/(VarAmongPop + VarAmongBlock + VarWithinGroups)*100
PctVarExpByBlock <- VarAmongBlock/(VarAmongPop + VarAmongBlock + VarWithinGroups)*100

CV2_Pop = VarAmongPop/mean(df$aborted)^2
CV2_Block = VarAmongBlock/mean(df$aborted)^2
CV2_Within = VarWithinGroups/mean(df$aborted)^2
CV2_Total = CV2_Pop + CV2_Block + CV2_Within


# ==================
# DRAW AND SAVE PLOT
# ==================

# Create plot
plot(
  df$height,
  df$aborted,
  xlab = "Plant Height (cm)",
  ylab = "Number of Aborted Flowers",
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