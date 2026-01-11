# This script fits a GLM to the dataset provided. The goal of the analysis is
# to determine how total floral scent emission effects a plant's fitness.
# Scripts that generate plots and tables can be found in /Utils/


# =====================
# CONFIGURE ENVIRONMENT
# =====================

# Clear variables
rm(list = ls())

# Load packages
library(here)
library(tidyverse)
library(glmmTMB)
library(MuMIn)

# Supress update warning from MuMIn because all the variables used in the
# original model call have the same values as when the model was fitted
options(MuMIn.noUpdateWarning = TRUE)

# Set data filepath
data_path <- here("Report", "Data", "penstemon_copy.txt")


# =========
# LOAD DATA
# =========

# Load raw data
df <- as_tibble(
  read.table(data_path, header = TRUE)
) |>
  select(Pop, Block, tscent, fitness) |>  # Select relevant columns
  mutate(across(c(Pop, Block), as.factor)) |>  # Make grouped variables factors
  filter(tscent != 0 & fitness != 0) |>  # Drop values whose log is undefined
  mutate(log_tscent = log(tscent)) |>  # Create log_tscent column
  mutate(log_fitness = log(fitness))  # Create log_fitness column

# NOTE: one row is dropped for 0 values


# ==================================
# FIT MODEL AND GENERATE PREDICTIONS
# ==================================

# Fit glmm on a log-log scale
m <- glmmTMB(log_fitness ~ log_tscent + Pop + (1|Block), data = df)

# Create data frames with sequences to generate new predictions on for each pop
new_data <- list()

for (i in seq_along(levels(df$Pop))) {
  new_data[[i]] <- tibble(
    log_tscent = seq(min(df$log_tscent), max(df$log_tscent), length.out = 15),
    Pop = factor(rep(levels(df$Pop)[i], 15))
  )
}

names(new_data) <- levels(df$Pop)

# Generate predictions and standard error estimates
preds <- list()

for (i in seq_along(new_data)) {
  preds[[i]] <- predict(
    m,
    newdata = new_data[[i]],
    type = "response",  # Calculate predictions on response scale
    se.fit = TRUE,
    re.form = NA  # Do not include random effects in predictions
  )
}

names(preds) <- levels(df$Pop)

# Get r^2
rsq <- r.squaredGLMM(m)

# Perform variance partitioning (v_part called in Utils/Table1.R)
v_part <- c(attr(VarCorr(m)$cond$Block, "stddev")^2, attr(VarCorr(m)$cond, "sc")^2)