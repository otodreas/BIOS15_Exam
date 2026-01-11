# This script sources ../Analysis.R and defines a custom function that the user
# can use to quickly make predictions on the data scale based on input values
# on the data scale in the R console. The predictions used as examples in the
# report are printed to the console when you run this script


# =====================
# CONFIGURE ENVIRONMENT
# =====================

# Clear variables
rm(list = ls())

# Load libraries
library(here)
library(dplyr)

# Source analysis file
source(here("Report", "Scripts", "Analysis.R"), echo = FALSE)


# =============
# ORGANIZE DATA
# =============

# Make an organized tibble with parameter estimates
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


# ===============
# DEFINE FUNCTION
# ===============

# Define function to return preditions on data scale for a value of tscent
make_pred <- function(x_in) {
  # Ensure that each population is paired with the position of the parameter in
  # the params tibble
  estimates <- matrix(nrow = 3, ncol = 2)
  for (i in seq_along(levels(df$Pop))) {
    estimate <- as.numeric(exp(log(x_in) * params[1, 2] + params[i + 1, 2]))
    estimates[i, ] <- c(x_in, estimate)
  }
  colnames(estimates) <- c("Input", "Prediction")
  rownames(estimates) <- levels(df$Pop)
  estimates
}


# =============================================
# MAKE EXAMPLE PREDICTIONS SHOWN IN THE WRITEUP
# =============================================

# Make predictions used in the write-up
pred_iqr1 <- make_pred(as.numeric(summary(df$tscent)[2]))
pred_iqr3 <- make_pred(as.numeric(summary(df$tscent)[5]))

pred_iqr1
pred_iqr3
pred_iqr3 - pred_iqr1
# Intentionally blank lines to avoid printing variables when the file is
# sourced in the report