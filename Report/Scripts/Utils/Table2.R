# This script sources ../Analysis.R and Predictor.R and creates table 1. It
# sources Predictor.R because a neat parameter table was produced in it.


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

# Source predictor file to get access to neat parameter estimates
source(here("Report", "Scripts", "Utils", "Predictor.R"), echo = FALSE)


# =================
# BUILD LATEX TABLE
# =================

kable(
  params,
  format = "latex",
  col.names = c("Parameter", "Estimate", "Standard error"),
  caption = "Parameter estimates and standard errors for the fixed effects of the generalized linear mixed model given on the link scale. Conditional pseudo-R-squared = 0.26.",
  booktabs = TRUE
)