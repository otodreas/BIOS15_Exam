# This script sources ../Analysis.R and creates table 1


# =====================
# CONFIGURE ENVIRONMENT
# =====================

# Clear variables
rm(list = ls())

# Load libraries
library(here)
library(knitr)

# Source analysis file
source(here("Report", "Scripts", "Analysis.R"), echo = FALSE)


# ================
# BUILD DATA FRAME
# ================

v_part_smry <- data.frame(
  Component = c(
    "Variance among blocks",
    "Variance within groups",
    "Percent variance explained by block"
  ),
  "Variance" = c(
    round(v_part[1], 2),
    round(v_part[2], 2),
    round(v_part[1] / sum(v_part) * 100, 2)
  )
)

v_part_smry[1:2, 2] <- paste(v_part_smry[1:2, 2], "$mg^2$")
v_part_smry[3, 2] <- paste0(v_part_smry[3, 2], "\\%")


# =================
# BUILD LATEX TABLE
# =================

kable(
  v_part_smry,
  format = "latex",
  col.names = c("Component", "Variance"),
  escape = FALSE,
  caption = "Variance partitioning in generalized linear mixed model.",
  booktabs = TRUE
)