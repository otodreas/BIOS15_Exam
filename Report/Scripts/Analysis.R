# This script fits a GLM to the dataset provided. The goal of the analysis is
# to determine how total floral scent emission effects a plant's fitness. Users 
# who do not clone the entire repo but instead just download the script will
# need to provide custom filepaths. The data file can be found at
# root/Report/Data/penstemon_copy.txt


# ===================================
# CONFIGURE ENVIRONMENT AND LOAD DATA
# ===================================

# Clear variables
rm(list = ls())

# Load packages
library(here)
library(tidyverse)
library(glmmTMB)
library(MuMIn)

# Supress update warning from MuMIn
options(MuMIn.noUpdateWarning = TRUE)

# Set filepaths
data_path <- here("Report", "Data", "penstemon_copy.txt")
summary_path <- here("Report", "Output", "summary.txt")
params_path <- here("Report", "Output", "params.csv")

# Load raw data
df <- as_tibble(
  read.table(data_path, header = TRUE)
) |>
  mutate(across(c(Pop, Block), as.factor)) |>  # Make grouped variables factors
  filter(tscent != 0) |>  # Drop tscent values whose logarithms are undefined
  mutate(log_tscent = log(tscent)) |>  # Create log_tscent column
  mutate(log_fitness = log(fitness))  # Create log_fitness column

# NOTE: Elasticity = for a 1% change in x (tscent), a x% change in y (fitness)
# is expected ("Percent change in y per percent change in x")
# TODO: double check with course literature


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


# ===================================
# BUILD SUMMARY STATISTICS TEXT FILES
# ===================================

# Assign variances to a vector
v_part <- c(attr(VarCorr(m)$cond$Block, "stddev")^2, attr(VarCorr(m)$cond, "sc")^2)

# Build summary file
cat(
  paste0(
    # R^2
    "R^2",
    "\nMarginal (represents variance explained only by the fixed effects): ",
    r.squaredGLMM(m)[1],
    "\nConditional (represents variance explained by the whole model): ",
    r.squaredGLMM(m)[2],
    
    # Variance partitioning
    "\n\nVariance partitioning",
    "\nVariance among blocks: ", v_part[1],
    "\nVariance within groups: ", v_part[2],
    "\n% variance explained by block: ", v_part[1] / sum(v_part) * 100
  ),
  file = summary_path
)

# Build parameters tibble
params <- as_tibble(summary(m)$coefficients$cond[, 1:2]) |>  # Get params & SE
  mutate(
    Parameter = c(
      "PopNR intercept (ln(Fitness))",
      "Slope (ln(Fitness)/ln(Total floral scent emission (ng/L/h)))",
      "PopTH intercept (ln(Fitness))",
      "PopWF intercept (ln(Fitness))"
    )
  ) |>  # Create parameter names that make sense and move the column left
  relocate(Parameter)

# Make every population's intercept absolute rather than relative
params[3, 2] <- params[1, 2] + params[3, 2]
params[4, 2] <- params[1, 2] + params[4, 2]

# Write parameters to file
write_csv(params, params_path)


# =========
# DRAW PLOT
# =========

# Create plot on data scale
plot(
  df$tscent,
  df$fitness,
  xlab = "Total floral scent emission (ng/L/h)",
  ylab = "Fitness",
  col = adjustcolor(seq_along(levels(df$Pop)), alpha.f = 0.4),
  pch = 19,
)

# Draw legend
legend(
  "top",
  legend = levels(df$Pop),
  col = seq_along(levels(df$Pop)),
  lty = 1,
  bty = "n",
  title = "Population"
)

# Draw 95% CI ribbons on data scale
for (i in seq_along(preds)) {
  polygon(
    c(exp(new_data[[i]]$log_tscent), rev(exp(new_data[[i]]$log_tscent))),
    c(
      exp(preds[[i]]$fit) + 1.96 * exp(preds[[i]]$se.fit),
      rev(exp(preds[[i]]$fit) - 1.96 * exp(preds[[i]]$se.fit))
    ),
    col = adjustcolor(i, alpha.f = 0.25),
    border = FALSE
  )
}

# Draw regression lines on data scale
for (i in seq_along(preds)) {
  lines(exp(new_data[[i]]$log_tscent), exp(preds[[i]]$fit), col = i)
}

# NOTE: Plots were saved using RStudio/Positron interface