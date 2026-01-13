# This script sources ../Analysis.R and Predict.R and creates table 1. It
# sources Predict.R because a neat parameter table was produced in it.


# =====================
# CONFIGURE ENVIRONMENT
# =====================

# Clear variables
rm(list = ls())

# Load libraries
library(here)
library(dplyr)
library(knitr)

# Source analysis file
source(here("Report", "Scripts", "Analysis.R"), echo = FALSE)

# Source predict file to get access to neat parameter estimates
source(here("Report", "Scripts", "Utils", "Predict.R"), echo = FALSE)

# Make joint param & SE column
params <- params |>
  mutate("Estimate ± SE" = paste(Estimate, "±", `Std. Error`)) |>
  select(Parameter, `Estimate ± SE`)


# =================
# BUILD LATEX TABLE
# =================

kable(
  params,
  format = "latex",
  caption = "Parameter estimates and standard errors for the fixed effects of the generalized linear mixed model given on the link scale. Conditional pseudo-R-squared = 0.26.",
  booktabs = TRUE
)