# This script calls the script ../Analysis.R and creates a plot.


# =====================
# CONFIGURE ENVIRONMENT
# =====================

# Clear variables
rm(list = ls())

# Source analysis file
library(here)
source(here("Report", "Scripts", "Analysis.R"), echo = FALSE)


# ==========
# MAKE PLOTS
# ==========

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