# This script fits a GLM to the dataset provided. The goal of the analysis is
# to determine how total floral scent emission effects the number of fruits
# produced. The fitness variable doesn't make sense here because to me taller
# likely means to be heavier--in other words, weight and fitness are correlated


# ===================================
# CONFIGURE ENVIRONMENT AND LOAD DATA
# ===================================

# Clear variables
rm(list = ls())

# Load packages
library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(glmmTMB, quietly = TRUE)
library(performance, quietly = TRUE)
library(MuMIn, quietly = TRUE)

# Supress update warning from MuMIn
options(MuMIn.noUpdateWarning = TRUE)

# Load raw data
df <- as_tibble(
  read.table(here("Report", "Data", "penstemon_copy.txt"), header = TRUE)
) |>
  mutate(across(c(Pop, Block), as.factor)) |>  # Make grouped variables factors
  filter(tscent != 0) |>  # Drop tscent values whose logarithms are undefined
  mutate(log_tscent = log(tscent)) |>  # Create log_tscent column
  mutate(log_fitness = log(fitness))  # Create log_fitness column

# Load metadata file for column lookup
# TODO: delete this when no longer needed
metadata <- read.csv(here("Report", "Data", "metadata.csv"))


# ==================================
# FIT MODEL AND GENERATE PREDICTIONS
# ==================================

# Fit model
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
    re.form = NA  # Do not include random effects
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
    "\nMarginal: ", r.squaredGLMM(m)[1],
    "\nConditional: ", r.squaredGLMM(m)[2],
    
    # Variance partitioning
    "\n\nVariance partitioning",
    "\nVariance among blocks: ", v_part[1],
    "\nVariance within groups: ", v_part[2],
    "\n% variance explained by block: ", v_part[1] / sum(v_part) * 100
  ),
  file = here("Report", "Output", "summary.txt")
)

# Build parameters tibble
params <- as_tibble(summary(m)$coefficients$cond[, 1:2]) |>
  mutate(
    Parameter = c(
      "PopNR intercept (ln(Fitness))",
      "Slope (ln(Fitness)/ln(Total floral scent emission (ng/L/h)))",
      "PopTH intercept (ln(Fitness))",
      "PopWF intercept (ln(Fitness))"
    )
  ) |>
  relocate(Parameter)

# Make every population's intercept absolute rather than relative
params[3, 2] <- params[1, 2] + params[3, 2]
params[4, 2] <- params[1, 2] + params[4, 2]

# Write parameters to file
write_csv(params, here("Report", "Output", "params.csv"))


# =========
# DRAW PLOT
# =========

# Create plot
plot(
  df$log_tscent,
  df$log_fitness,
  xlab = "ln(Total floral scent emission (ng/L/h))",
  ylab = "ln(Fitness)",
  col = adjustcolor(seq_along(levels(df$Pop)), alpha.f = 0.5),
  pch = 19,
)

# Draw legend
legend(
  "bottomright",
  legend = levels(df$Pop),
  col = seq_along(levels(df$Pop)),
  lty = 1,
  bty = "n",
  title = "Population"
)

# Draw regression lines
for (i in seq_along(preds)) {
  lines(new_data[[i]]$log_tscent, preds[[i]]$fit, col = i)
}; rm(i)

# NOTE: Plots were saved using RStudio/Positron interface