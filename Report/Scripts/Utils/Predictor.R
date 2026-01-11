# This script sources ../Analysis.R and defines a custom function that the user
# can use to quickly make predictions on the data scale based on input values
# on the data scale in the R console. 


# =====================
# CONFIGURE ENVIRONMENT
# =====================

# Clear variables
rm(list = ls())


# ===============
# DEFINE FUNCTION
# ===============

# Source analysis file
library(here)
source(here("Report", "Scripts", "Analysis.R"), echo = FALSE)

# Define function to return preditions on data scale for a value of tscent
make_pred <- function(x_in) {
  # Ensure that each population is paired with the position of the parameter in
  # the params tibble
  pops <- list(1, 3, 4)
  names(pops) <- levels(df$Pop)
  estimates <- matrix(nrow = 3, ncol = 2)
  row <- 0
  for (i in levels(df$Pop)) {
    row <- row + 1
    estimate <- as.numeric(exp(log(x_in) * params[2, 2] + params[pops[[i]], 2]))
    estimates[row, ] <- c(x_in, estimate)
  }
  colnames(estimates) <- c("Input", "Prediction")
  rownames(estimates) <- levels(df$Pop)
  estimates
}