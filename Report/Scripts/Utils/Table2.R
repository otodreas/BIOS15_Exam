library(here)
library(knitr)
params <- read.csv(here("Report", "Output", "params.csv"))
kable(,
  format = "latex",
  caption = "CAP",
  booktabs = TRUE
)