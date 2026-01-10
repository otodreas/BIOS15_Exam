library(here)
library(knitr)
params <- read.csv(here("Report", "Output", "params.csv"))
kable(
  params,
  format = "latex",
  col.names = c("Parameter", "Estimate", "Standard error"),
  caption = "Parameter estimates and standard errors for the fixed effects of the generalized linear mixed model given on the link scale. Conditional pseudo-R-squared = 0.26.",
  booktabs = TRUE
)