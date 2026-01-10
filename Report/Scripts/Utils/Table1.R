library(here)
library(knitr)
smry <- read.csv(here("Report", "Output", "vpart.csv"))
kable(
  smry,
  format = "latex",
  col.names = c("Component", "Variance ($mg^2$)"),
  escape = FALSE,
  caption = "Variance partitioning in generalized linear mixed model. The variance within groups represented the majority of the variance in the model.",
  booktabs = TRUE
)