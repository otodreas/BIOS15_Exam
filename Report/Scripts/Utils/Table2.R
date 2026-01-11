# This script sources ../Analysis.R and creates table 1


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


# ============
# BUILD TIBBLE
# ============

# Get params & SE
params <- as_tibble(round(summary(m)$coefficients$cond[, 1:2], 2)) |>
  mutate(
    Parameter = c(
      "NR intercept (ln(mg))",
      "Slope (ln(mg))/ln(ng/L/h))",
      "TH intercept (ln(mg))",
      "WF intercept (ln(mg))"
    )
  ) |>  # Create parameter names that make sense and move the column left
  relocate(Parameter) |>
  slice(2, 1, 3, 4)  # Reorder tibble

# Make every population's intercept absolute rather than relative
params[3, 2] <- params[2, 2] + params[3, 2]
params[4, 2] <- params[2, 2] + params[4, 2]


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