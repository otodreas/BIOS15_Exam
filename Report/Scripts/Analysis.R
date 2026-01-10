# This script fits a GLM to the dataset provided. The goal of the analysis is
# to determine how total floral scent emission effects a plant's fitness. Users 
# who do not clone the entire repo but instead just download the script will
# need to provide custom filepaths. The data file can be found at
# root/Report/Data/penstemon_copy.txt


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

# Set filepaths
data_path <- here("Report", "Data", "penstemon_copy.txt")
summary_path <- here("Report", "Output", "summary.txt")
params_path <- here("Report", "Output", "params.csv")
vpart_path <- here("Report", "Output", "vpart.csv")


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


# =========
# DRAW PLOT
# =========

# Create plot on data scale
plot(
  df$tscent,
  df$fitness,
  xlab = "Total floral scent emission (ng/L/h)",
  ylab = "Fitness (mg)",
  col = adjustcolor(seq_along(levels(df$Pop)), alpha.f = 0.4),
  pch = 19,
)

# Draw legend
legend(
  "topleft",
  legend = levels(df$Pop),
  col = adjustcolor(seq_along(levels(df$Pop)), alpha.f = 0.4),
  pch = 19,
  bty = "n",
  horiz = TRUE,
  title = "Population",
  inset = 0.02
)

# Draw 95% CI ribbons on data scale
for (i in seq_along(preds)) {
  polygon(
    c(exp(new_data[[i]]$log_tscent), rev(exp(new_data[[i]]$log_tscent))),
    c(
      exp(preds[[i]]$fit + 1.96 * preds[[i]]$se.fit),
      rev(exp(preds[[i]]$fit - 1.96 * preds[[i]]$se.fit))
    ),
    col = adjustcolor(i, alpha.f = 0.15),
    border = FALSE
  )
}

# Draw regression lines on data scale
for (i in seq_along(preds)) {
  lines(exp(new_data[[i]]$log_tscent), exp(preds[[i]]$fit), col = i)
}

# NOTE: Plot was not saved but rendered using Quarto


# ==============================
# BUILD SUMMARY STATISTICS FILES
# ==============================

# Assign variances to a vector
v_part <- c(attr(VarCorr(m)$cond$Block, "stddev")^2, attr(VarCorr(m)$cond, "sc")^2)

# Build summary file
cat(
  paste0(
    # R^2
    "R^2",
    "\nMarginal (represents variance explained only by the fixed effects): ",
    round(r.squaredGLMM(m)[1], 2),
    "\nConditional (represents variance explained by the whole model): ",
    round(r.squaredGLMM(m)[2], 2),
    
    # Variance partitioning
    "\n\nVariance partitioning",
    "\nVariance among blocks: ", round(v_part[1], 2),
    "\nVariance within groups: ", round(v_part[2], 2),
    "\n% variance explained by block: ", round(v_part[1] / sum(v_part) * 100, 2)
  ),
  file = summary_path
)

# Build variance partition tibble
vpart_clean <- as_tibble(
  matrix(
    c(
      "Variance among blocks", round(v_part[1], 2),
      "Variance within groups", round(v_part[2], 2),
      "Percent variance explained by block",
      round(v_part[1] / sum(v_part) * 100, 2)
    ), ncol = 2, byrow = TRUE
  )
)
colnames(vpart_clean) <- c("Component", "Variance (mg^2)")
write_csv(vpart_clean, vpart_path)

# Build parameters tibble
params <- as_tibble(round(summary(m)$coefficients$cond[, 1:2], 2)) |>  # Get params & SE
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

# Write parameters to file
write_csv(params, params_path)

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