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

v_part_smry <- as_tibble(
  matrix(
    c(
      "Variance among blocks", round(v_part[1], 2),
      "Variance within groups", round(v_part[2], 2),
      "Percent variance explained by block",
      round(v_part[1] / sum(v_part) * 100, 2)
    ), ncol = 2, byrow = TRUE
  )
)
colnames(v_part_smry) <- c("Component", "Variance (mg^2)")


# =================
# BUILD LATEX TABLE
# =================

kable(
  v_part_smry,
  format = "latex",
  col.names = c("Component", "Variance ($mg^2$)"),
  escape = FALSE,
  caption = "Variance partitioning in generalized linear mixed model. The variance within groups represented the majority of the variance in the model.",
  booktabs = TRUE
)